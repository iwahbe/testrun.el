;;; pytest.el --- pytest adapter  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:

;;; Code:


(defun testrun--pytest-backend (test-spec &optional verbose update)
  "Adapts Pytest for testrun."
  (format
   "uv run pytest %s%s"
   (pcase test-spec
     ('at-point (testrun--pytest-args-at-point))
     ('in-current-directory ".")
     ('in-current-file (buffer-file-name)))
   (if verbose " -s" "")))


(defun testrun--pytest-args-at-point ()
  "Configure pytest to run the current test def at point."
  (let ((current-def (testrun--pytest-def-name-at-point)))
    (if current-def
      (format "%s -k %s" (buffer-file-name) current-def)
      (error "not within a def"))))


(defun testrun--pytest-def-name-at-point ()
  "Find the name of the current def the point is within."
  (let ((node nil))
    (save-excursion
      (treesit-beginning-of-defun)
      (setq node
            (treesit-node-parent
             (treesit-node-at (point)))))
    (pcase node
      ((and (pred treesit-node-p)
            (app treesit-node-type "function_definition")
            (app treesit-node-children `(,_ ,x ,_ ,_ ,_)))
       (treesit-node-text x t)))))


;;; pytest.el ends here
