;;; compile-bind.el --- easily create build system keybindings

;;; Code:

(require 'cl)

(defmacro compile-bind (map key builder target)
  "Define a key binding for a build system target (i.e. make,
ant, scons) in a particular keymap."
  `(define-key ,map ,key
     (lambda (n)
       (interactive "p")
       (let* ((buffer-name (format "*compilation-%d*" n))
              (compilation-buffer-name-function (lambda (x) buffer-name)))
         (save-buffer)
         (compile (format "%s %s" ,builder ,target) t)))))

(defmacro compile-bind* (map builder keys/fns)
  "Create several compile-bind bindings in a row."
  `(progn
     ,@(loop for (key fn) on keys/fns by 'cddr
             collecting `(compile-bind ,map (kbd ,key) ,builder ,fn))))

(provide 'compile-bind)

;;; compile-bind.el ends here
