;;; golang.el --- golang adapter  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:

;;; Code:

(require 'treesit)


(defun testrun--go-backend (test-spec &optional verbose update)
  "Adapts Go for testrun."
  (format
   "go test %s%s%s"
   (pcase test-spec
     ('at-point (testrun--go-test-command-at-point))
     ('in-current-directory ".")
     ('in-current-file (testrun--go-test-command-current-file)))
   (if verbose " -test.v" "")
   (if update " -update" "")))


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


(defun testrun--go-test-command-current-file (&optional update)
  "Return a command that runs all tests in the current directory in Go."
  ;; Ensure there is a `treesit-parser` for Go; create one if needed.
  (unless (treesit-parser-list)
    (treesit-parser-create 'go))
  (if-let ((matches (treesit-query-capture (car (treesit-parser-list nil 'go)) testrun--go-tests-fns-in-node)))
      (concat (format "-test.run \"^(%s)$\" "
                      (mapconcat
                       (lambda (match) (treesit-node-text (cdr match)))
                       (seq-filter (lambda (match) (eq (car match) 'function-name)) matches)
                       "|"))
              "./...")
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


;;; golang.el ends here
