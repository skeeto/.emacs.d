;;; compile-bind.el --- manage build system keybindings -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

(defcustom compile-bind-command "make -k"
  "Command used for performing builds, without the target."
  :type 'string
  :risky t
  :group 'programming)

(defcustom compile-bind-root-regex "^Makefile"
  "File to find before invoking compilation."
  :type 'string
  :group 'programming)

(defvar compile-bind-command-history ()
  "History of values for `compile-bind-command'.")

(defvar compile-bind-root-history ()
  "History of values for `compile-bind-root-regex'.")

(defun compile-bind-set-command (command)
  "Set `compile-bind-command' to a new value."
  (interactive
   (list (read-shell-command "Compile command: " compile-bind-command
                             'compile-bind-command-history)))
  (setf compile-bind-command command))

(defun compile-bind-set-root-file (regex)
  "Set `compile-bind-root-regex' to a new value."
  (interactive
   (list (read-regexp (format "Root file (%s): " compile-bind-root-regex)
                      compile-bind-root-regex 'compile-bind-root-history)))
  (setf compile-bind-root-regex regex))

(defun compile-bind (map key target)
  "Define a key binding for a build system target (i.e. make,
ant, scons) in a particular keymap."
  (define-key map key
    (lambda (n)
      (interactive "p")
      (let* ((buffer-name (format "*compilation-%d*" n))
             (compilation-buffer-name-function (lambda (_) buffer-name))
             (predicate
              (apply-partially #'string-match-p compile-bind-root-regex)))
        (save-buffer)
        (with-temp-buffer
          (while (not (or (cl-some predicate (directory-files "."))
                          (string= "/" (cd "..")))))
          (compile (format "%s %s" compile-bind-command target) t))))))

(defmacro compile-bind* (map keys/fns)
  "Create several compile-bind bindings in a row."
  `(progn
     ,@(cl-loop for (key fn) on keys/fns by 'cddr
                collecting `(compile-bind ,map (kbd ,key) ,fn))))

(provide 'compile-bind)

;;; compile-bind.el ends here
