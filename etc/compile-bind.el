;;; compile-bind.el --- easily create build system keybindings

;;; Code:

(require 'cl-lib)

(defvar-local compile-bind-command "make"
  "Command used for performing builds, without the target.")

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
