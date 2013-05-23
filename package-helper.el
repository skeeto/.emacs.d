(require 'cl)
(require 'package)

(defun packagep (name)
  "Return t if NAME is an available package."
  (unless package-archive-contents
    (package-refresh-contents))
  (not (null (assoc name package-archive-contents))))

(defun package-real-name (package)
  "PACKAGE may be the name of an autoloads; return the actual package name."
  (intern (replace-regexp-in-string "-autoloads$" "" (symbol-name package))))

(defmacro with-package (packages &rest body)
  "Like `eval-after-load', but also automatically register
PACKAGES for installation by `package-install'. PACKAGES can be
either a single symbol or a list of symbols.

  BODY is only ever evaluated once, and only after all PACKAGES
have been loaded. This means if any of the listed packages are
loaded, the others will be immediately loaded as well.

  The body is wrapped in a function so that it gets properly
compiled. Normally with `eval-after-load' it gets quoted for a
literal future `eval', so it appears as data to the compiler."
  (declare (indent defun))
  (let ((has-run-sym (make-symbol "has-run-p"))
        (f-sym (make-symbol "f")))
    (when (symbolp packages)
      (setf packages (list packages)))
    `(progn
       (setq ,has-run-sym nil)
       (fset ',f-sym (lambda ()
                       (unless ,has-run-sym
                         (setq ,has-run-sym t)
                         ,@(loop for package in packages collect
                                 `(require ',(package-real-name package)))
                         ,@body)))
       ,@(loop for package in packages
               for real-name = (package-real-name package)
               collect `(when (and (packagep ',real-name)
                                   (not (package-installed-p ',real-name)))
                          (package-install ',real-name)))
       ,@(loop for package in packages collect
               `(eval-after-load ',package '(,f-sym))))))

(defmacro with-package* (packages &rest body)
  "Like `with-package*' but also `require' all of the packages.
This is mostly for code organization purposes."
  (declare (indent defun))
  (when (symbolp packages)
    (setf packages (list packages)))
  `(progn
     (with-package ,packages ,@body)
     ,@(loop for package in packages collect `(require ',package))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\<\\(with-package\\*?\\)\\> +(?\\([^()]+\\))?"
     (1 'font-lock-keyword-face)
     (2 'font-lock-constant-face))))

(provide 'package-helper)
