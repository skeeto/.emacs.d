;;; compile-bind.el --- easily create build system keybindings

;;; Code:

(require 'cl-lib)

(defcustom compile-bind-command "make"
  "Command used for performing builds, without the target."
  :type 'string
  :risky t)

(defvar compile-bind-command-history ()
  "History of values for `compile-bind-command'.")

(defun compile-bind-set-command (command)
  "Set `compile-bind-command' to a new value."
  (interactive
   (list (read-shell-command "Compile command: " compile-bind-command '
                             compile-bind-command-history)))
  (setf compile-bind-command command))

(defmacro compile-bind (map key target)
  "Define a key binding for a build system target (i.e. make,
ant, scons) in a particular keymap."
  `(define-key ,map ,key
     (lambda (n)
       (interactive "p")
       (let* ((buffer-name (format "*compilation-%d*" n))
              (compilation-buffer-name-function (lambda (x) buffer-name)))
         (save-buffer)
         (compile (format "%s %s" compile-bind-command ,target) t)))))

(defmacro compile-bind* (map keys/fns)
  "Create several compile-bind bindings in a row."
  `(progn
     ,@(cl-loop for (key fn) on keys/fns by 'cddr
                collecting `(compile-bind ,map (kbd ,key) ,fn))))

(provide 'compile-bind)

;;; compile-bind.el ends here
