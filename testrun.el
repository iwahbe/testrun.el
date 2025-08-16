;;; testrun.el --- helpers for running tests  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:

;;; Code:

(require 'compile)
(require 'treesit)


;;;; Customization


(defcustom testrun-backends
  '((:mode go-ts-mode :backend testrun--go-backend)
    (:mode go-mode :backend testrun--go-backend)
    (:mode python-mode :backend testrun--pytest-backend)
    (:mode python-ts-mode :backend testrun--pytest-backend))

  "Configure backends by programming mode.

A backend is a function that returns a compile-command string for a
given test-spec, as follows:

(lambda (test-spec &optional verbose update) ...)

The test-spec is one of the following symbols:

    at-point
    in-current-directory
    in-current-file

If verbose is set to t, the command should configure verbose logging.

If update is set to t, the command should configure automatically
updating golden test files.
"

  :type '(sexp)
  :group 'languages)


(defcustom testrun-switch-to-compilation-buffer nil
  "A flag that enables switching to the compilation buffer after each test command."
  :type 'boolean
  :group 'languages)


;;;; State


(defvar testrun--verbose nil)


;;;; Commands


;;;###autoload
(defun testrun-at-point (arg)
  "Run a test at point."
  (interactive "p")
  (testrun--test 'at-point arg))


;;;###autoload
(defun testrun-in-current-file (arg)
  "Run all tests in the current file."
  (interactive "p")
  (testrun--test 'in-current-file arg))


;;;###autoload
(defun testrun-in-current-directory (arg)
  "Run tests in the current directory."
  (interactive "p")
  (testrun--test 'in-current-directory arg))


;;;###autoload
(defun testrun-repeat ()
  "Repeat the most recently executed test command."
  (interactive)
  (recompile)
  (when testrun-switch-to-compilation-buffer
    (compilation-goto-in-progress-buffer)))


;;;###autoload
(defun testrun-toggle-verbosity ()
  "Toggle verbosity level for testing."
  (interactive)
  (setq testrun--verbose (not testrun--verbose))
  (message "testrun verbosity is turned %s" (if testrun--verbose "on" "off")))


;;;; Implementation


(defun testrun--test (test-spec arg)
  (let* ((update (equal arg 4))
         (b (testrun--pick-backend))
         (c (apply b test-spec testrun--verbose update)))
    (testrun--compile c)))


(defun testrun--pick-backend ()
  "Pick a backend for the current major mode."
  (let ((selected-backend nil))
    (dolist (backend testrun-backends)
      (when (derived-mode-p (plist-get backend :mode))
        (setq selected-backend (plist-get backend :backend))))
    (if selected-backend
        selected-backend
      (error "No backend found for %s" major-mode))))


(defun testrun--compile (c)
  (compile c)
  (when testrun-switch-to-compilation-buffer
    (compilation-goto-in-progress-buffer)))


(provide 'testrun)
;;; testrun.el ends here
