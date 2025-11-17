;;; testrun-golang.el --- golang adapter  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:

;;; Code:

(require 'treesit)


(defun testrun--go-backend (test-spec &optional verbose update)
  "Adapts Go for testrun."
  (format
   "go test %s%s%s%s"
   (pcase test-spec
     ('at-point (testrun--go-test-command-at-point))
     ('in-current-directory ".")
     ('in-current-file (testrun--go-test-command-current-file)))
   (if verbose " -test.v" "")
   (if update " -update" "")
   (if-let* ((tags (and (not (eq test-spec 'in-current-directory)) (testrun--go-find-build-constraints))))
       (format " -tags=%s" tags) "")))


(defun testrun--go-func-testing-parameter-list-p (node)
  "Recognizes if the treesitter NODE is like (t *testing.T)."
  (pcase node
    ((and (pred treesit-node-p)
          (app treesit-node-type "parameter_list")
          (app treesit-node-children
               `(,_
                 ,(and (app treesit-node-type "parameter_declaration")
                       (app treesit-node-children
                            `(,_
                              ,(and (app treesit-node-type "pointer_type")
                                    (app treesit-node-children
                                         `(,(app treesit-node-text "*")
                                           ,(app treesit-node-children
                                                 `(,(app treesit-node-text "testing")
                                                   ,(app treesit-node-text ".")
                                                   ,(app treesit-node-text "T")))
                                           ))))))
                 ,_)))
     t)
    (_ nil)))


(defun testrun--go-func-testing-node-p (node)
  "Recognizes if the treesitter NODE is like func(t *testing.T) {..}."
  (pcase node
    ((and (pred treesit-node-p)
          (app treesit-node-type "func_literal")
          (app treesit-node-children
               `(,_
                 ,(pred testrun--go-func-testing-parameter-list-p)
                 ,_)))
     t)
    (_ nil)))


(defun testrun--go-parse-t-run-node (node)
  "Recognizes if the treesitter NODE is like t.Run(.., func(t *testing.T) {})."
  (pcase node
    ((and (pred treesit-node-p)
          (app treesit-node-type "call_expression")
          (app treesit-node-children
               `(,(and (app treesit-node-type "selector_expression")
                       (app treesit-node-children
                            `(,_
                              ,_
                              ,(app treesit-node-text "Run"))))
                 ,(and (app treesit-node-type "argument_list")
                       (app treesit-node-children
                            `(,_
                              ,x
                              ,_
                              ,(pred testrun--go-func-testing-node-p)
                              ,_))))))
     (json-parse-string (treesit-node-text x t)))
    (_ nil)))


(defun testrun--go-test-command-at-point ()
  "Return the test command at point for Go."
  ;; Unless there is a `treesit-parser' already, create one.
  (unless (treesit-parser-list)
    (treesit-parser-create 'go))
  (let ((c (testrun--go-recognize-test-chain)))
    (if c
        (format "-test.run %s"
                (shell-quote-argument
                 (string-join (mapcar (lambda (x) (format "^%s$" x)) c) "/")))
        nil
      (error "Not inside a Go test"))))


(defvar testrun--go-tests-fns-in-node
  (treesit-query-compile
   'go
   '(((function_declaration
       name: (identifier) @function-name (:match "^Test" @function-name)
       parameters: (parameter_list
                    (parameter_declaration
                     name: (identifier)
                     type: (pointer_type
                            (qualified_type
                             package: (package_identifier) @pkg (:equal "testing" @pkg)
                             name: (type_identifier) @type-name  (:equal "T" @type-name)))))
       @parameter-list (:pred testrun--3-children-predicate @parameter-list)))))
  "A `treesit' query that will match all function nodes that will be run as tests.")


(defun testrun--3-children-predicate (node)
  "A predicate for tree-sitter: (= (children NODE) 3).

`treesit' doesn't allow this function to be inlined or moved out of the global scope."
  (= 3 (treesit-node-child-count node)))

(defun testrun--go-find-build-constraints ()
  "Find the build constraints in the current Go file.
Returns the build constraint string (e.g., 'go:build linux') or nil if none found.
Only searches comments at the beginning of the file, stopping when a non-comment
node is encountered.

See https://pkg.go.dev/cmd/go#hdr-Build_constraints."
  (unless (treesit-parser-list)
    (treesit-parser-create 'go))
  (when-let* ((parser (car (treesit-parser-list nil 'go)))
              (root (treesit-parser-root-node parser)))
    (let ((child (treesit-node-child root 0))
          (build-constraint nil))
      (while (and child
                  (not build-constraint)
                  (equal (treesit-node-type child) "comment"))
        (let ((text (treesit-node-text child t)))
          (when (string-prefix-p "//go:build " text)
            (setq build-constraint (testrun--go-format-build-constraint (string-remove-prefix "//go:build" text)))))
        (setq child (treesit-node-next-sibling child)))
      build-constraint)))

(defun testrun--go-format-build-constraint (expr)
  "Simplify the go build constraint expression EXPR into a set of tags usable with:

	go build --tags"
  (let* ((cst (testrun--go-parse-build-constraint expr))
         (ast1 (testrun--go-unquote-build-constraint cst))
         (ast2 (testrun--go-ast-build-constraint ast1)))
    (testrun--go-solve-build-constraint ast2)))

(defun testrun--go-parse-build-constraint (expr)
  (let ((trimmed (string-trim expr)))
    (condition-case nil
        (read (if (string-prefix-p "(" trimmed)
                  trimmed
                (concat "(" trimmed ")")))
      (error
       (message "Unable to parse build constraint %s" trimmed)
       nil))))

(defun testrun--go-unquote-build-constraint (expr)
  (cond
   ((symbolp expr)
    (let ((s (symbol-name expr)))
      (cond
       ((string-equal s "&&") '&&)
       ((string-equal s "||") '||)
       ((string-equal s "!") '!)
       ((string-prefix-p "!" s) (list '! (string-remove-prefix "!" s)))
       (t s))))
   ((listp expr)
    (seq-map #'testrun--go-unquote-build-constraint expr))
   (t expr)))

(defun testrun--go-ast-build-constraint (expr)
  "Lift EXPR from a CST into an AST.

We employ the following transformations:

- Symbols like !A are transformed into (! A)
- Non-conjoined symbols like A B are transformed into (|| A B)
- In-fix symbols (||, &&) are hoisted into prefix position.
 - Following Go's specification, && has a higher precedence then ||.

The returned AST has replaced all tag symbols with strings, but kept operators as symbols."
  (let* ((ast (testrun--go-ast-bind-unary-prefix '! expr))
         (ast (testrun--go-ast-insert-implicit '|| ast))
         (ast (testrun--go-ast-bind-infix '&& ast))
         (ast (testrun--go-ast-bind-infix '|| ast)))
    ast))

(defun testrun--go-ast-bind-infix (op expr)
  (if (not (listp expr))
      expr
    (let (result
          (expr (seq-map (apply-partially #'testrun--go-ast-bind-infix op) expr)))
      (while expr
        (if (eq op (car expr))
            (cond
             ((null (car result))
              (message "Dropping leading binary operator %s" op)
              (setq expr (cdr expr)))
             ((null (cadr expr))
              (message "Dropping trailing binary operator %s" op)
              (setq expr (cdr expr)))
             (t
              (setq result (cons (list op (car result) (testrun--go-ast-bind-infix op (cdr expr))) (cdr result)))
              (setq expr nil)))
          (setq result (cons (car expr) result))
          (setq expr (cdr expr))))
      (nreverse result))))

(defun testrun--go-ast-bind-unary-prefix (op expr)
  "Hoist OP into a unary prefix when found in EXPR.

For example, this would transform (A && ! B) into (A && (! B))."
  (if (not (listp expr))
      expr
    (let (result
          (expr (seq-map (apply-partially #'testrun--go-ast-bind-unary-prefix op) expr)))
          (while expr
            (if (eq op (car expr))
                (if (null (cadr expr))
                    (progn
                      (message "Dropping unexpected unary operator %s" op)
                      (setq expr (cdr expr)))
                  (setq result (cons (list op (cadr expr)) result))
                  (setq expr (cddr expr)))
              (setq result (cons (car expr) result))
              (setq expr (cdr expr))))
          (nreverse result))))

(defun testrun--go-ast-insert-implicit (op expr)
  "Insert OP into adjacent terms in EXPR without a separating infix operator.

For example, (\"A\" \"B\") would be transformed into (\"A\" '|| \"B\")."
  (if (not (listp expr))
      expr
    (let (result
          (expr (seq-map (apply-partially #'testrun--go-ast-insert-implicit op) expr)))
      (while expr
        (if (and (car result)
                 (not (symbolp (car expr)))
                 (not (symbolp (car result))))
            (progn
              (setq result (cons (list (car result) op (car expr)) (cdr result)))
              (setq expr (cdr expr)))
          (setq result (cons (car expr) result))
          (setq expr (cdr expr))))
      (nreverse result))))

(defun testrun--go-solve-build-constraint (expr)
  "Implement a poor man's SAT solver on EXPR.
A comma delineated string will be returned, suitable for the -tags option of \"go test\".

While this algorithm is technically a tiny SAT solver, only minimal
effort is made to solve the expression.  The algorithm is as follows:

1. Given \"X || Y\", attempt to solve X. If X resolves to no tags, attempt to resolve Y.
2. Given \"X && Y\", attempt to solve both X & Y and concatenate them.
3. Given \"!X\", discard (implicitly assuming that !X will be true)"
  (cond
   ((stringp expr) expr)
   ((listp expr)
    (cond
     ((length= expr 1) (testrun--go-solve-build-constraint (car expr)))
     ((eq (car expr) '!) nil) ;; Discard the right hand side of ! operators to simplify the SAT solver
     ((eq (car expr) '||) (or (testrun--go-solve-build-constraint (cadr expr))
                              (testrun--go-solve-build-constraint (caddr expr))))
     ((eq (car expr) '&&) (let ((r (testrun--go-solve-build-constraint (cadr expr)))
                                (l (testrun--go-solve-build-constraint (caddr expr))))
                            (if (and r l)
                                (concat r "," l)
                              (or r l))))
     (t (message "Unknown operator %s" (car expr)) nil)))
    (t (message "Unknown expression %s" expr))))

(defun testrun--go-test-command-current-file (&optional update)
  "Return a command that runs all tests in the current directory in Go."
  ;; Ensure there is a `treesit-parser` for Go; create one if needed.
  (unless (treesit-parser-list)
    (treesit-parser-create 'go))
  (if-let* ((matches (treesit-query-capture (car (treesit-parser-list nil 'go)) testrun--go-tests-fns-in-node)))
      (concat (format "-test.run \"^(%s)$\" "
                      (mapconcat
                       (lambda (match) (treesit-node-text (cdr match)))
                       (seq-filter (lambda (match) (eq (car match) 'function-name)) matches)
                       "|"))
              ".") ;; We only need to run tests in ".", since we will already be in the correct directory
    (user-error "No test functions found")))


(defun testrun--go-recognize-test-chain ()
  "Recognize nesting levels of Go Test and t.Run sub-tests around point as a list."
  (let ((loop t)
        (acc nil)
        (n (treesit-node-at (point))))
    (while (and n loop)
      (let ((x (testrun--go-parse-func-test n)))
        (if x (progn (setq acc (cons x acc))
                     (setq loop nil))
          (let ((y (testrun--go-parse-t-run-node n)))
            (when y (setq acc (cons y acc))))))
      (setq n (treesit-node-parent n)))
    acc))


(defun testrun--go-parse-func-test (node)
  "Recognizes if the treesitter NODE is a test function."
  (pcase node
    ((and (pred treesit-node-p)
          (app treesit-node-type "function_declaration")
          (app treesit-node-children
               `(,_
                 ,x
                 ,(pred testrun--go-func-testing-parameter-list-p)
                 ,_)))
     (treesit-node-text x t))
    (_ nil)))

(provide 'testrun-golang)

;;; testrun-golang.el ends here
