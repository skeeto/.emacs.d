;;; gpkg.el --- git package management -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Git submodules are terrible and I refuse to use them to manage
;; packages. This package downloads packages directly from Git
;; repositories and compiles them at specific commits.

;; It's a brute-force process with no dependency management. Some
;; packages require massaging in order to get them working, such as
;; using the :removal feature to delete unnecessary files.

;;; Code:

(require 'cl-lib)

(defvar gpkg-root "~/.emacs.d/gpkg"
  "Installation directory for all packages.")

(defvar gpkg-packages ()
  "List of all installed packages.")

(defvar gpkg-removal '("^t$" "^tests?$" "-pkg.el$")
  "Files/directories in packages matching these patterns are deleted.")

(defun gpkg-repository (name)
  "Return repository path for package NAME."
  (expand-file-name (concat name ".git") gpkg-root))

(defun gpkg-git (name &rest args)
  "Run git in NAME with the given command line arguments."
  (when name
    (push (gpkg-repository name) args)
    (push "-C" args))
  (princ (format "%S\n" (cons 'git args)))
  (apply #'call-process "git" nil '(:file "/dev/stdout") nil args))

(defun gpkg-clone (name url)
  "Clone the repository for NAME if needed."
  (let ((repository (gpkg-repository name)))
    (unless (file-exists-p repository)
      (gpkg-git nil "clone" url repository)
      (gpkg-git name "gc"))))

(defun gpkg-purge (dir removal)
  "Recursively delete everything under DIR matching REMOVAL list."
  (dolist (pattern removal)
    (dolist (file (directory-files dir t pattern t))
      (cond ((file-directory-p file)
             (delete-directory file t))
            ((file-exists-p file) (delete-file file)))))
  (dolist (subdir (directory-files dir t "^[^.]" t))
    (when (file-directory-p subdir)
      (gpkg-purge subdir removal))))

(defun gpkg-path (name &rest subdirs)
  "Return the installation root for NAME package."
  (cl-loop with path = (expand-file-name emacs-version gpkg-root)
           for dir in (cons name subdirs)
           do (setf path (expand-file-name dir path))
           finally return path))

(defun gpkg-package-url (name)
  "Return :removal value for package NAME."
  (let ((package (assoc name gpkg-packages)))
    (plist-get (cdr package) :url)))

(defun gpkg-package-ref (name)
  "Return :ref value for package NAME."
  (let ((package (assoc name gpkg-packages)))
    (plist-get (cdr package) :ref)))

(defun gpkg-package-removal (name)
  "Return :removal value for package NAME."
  (let ((package (assoc name gpkg-packages)))
    (plist-get (cdr package) :removal)))

(defun gpkg-checkout (name)
  "Checkout files for NAME."
  (let ((default-directory (file-name-as-directory gpkg-root)))
    (call-process-shell-command
     (format
      "git -C \"%s\" archive --format=tar --prefix=\"%s/%s/\" \"%s\" | tar xf -"
      (shell-quote-argument (gpkg-repository name))
      (shell-quote-argument emacs-version)
      (shell-quote-argument name)
      (shell-quote-argument (gpkg-package-ref name))))
    (gpkg-purge (gpkg-path name)
                (append (gpkg-package-removal name) gpkg-removal))))

(defun gpkg-ref-to-commit (name ref)
  "Return the commit for NAME at optional REF (HEAD)."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "-C" (gpkg-repository name)
                               "rev-parse" (format "%s^{commit}" ref)))
      (buffer-substring (point-min) (1- (point-max))))))

(cl-defun gpkg-install (name url ref &key removal)
  "Install package NAME from URL on commit REF, cloning if necessary."
  (gpkg-clone name url)
  (cl-pushnew `(,name :url ,url :ref ,ref :removal ,removal) gpkg-packages
              :key #'car :test #'equal)
  (unless (file-exists-p (gpkg-path name))
    ;; Is REF available?
    (unless (gpkg-ref-to-commit name ref)
      (gpkg-git name "fetch")
      (gpkg-git name "gc")
      (unless (gpkg-ref-to-commit name ref)
        (user-error "Unknown ref in %s: %s" name ref)))
    (gpkg-checkout name))
  (cl-pushnew (gpkg-path name) load-path :test #'equal))

(defun gpkg-compile ()
  "Byte-compile all installed packages."
  (byte-recompile-directory (expand-file-name emacs-version gpkg-root) 0))

(defun gpkg-clean ()
  "Remove all non-repository files for all versions of Emacs."
  (dolist (file (directory-files gpkg-root t "[^.]"))
    (when (and (file-directory-p file) (not (string-match-p "\\.git$" file)))
      (delete-directory file t))))

(defmacro gpkg-config (&rest packages)
  "Thread each list as arguments for `gpkg-install'."
  (cl-loop for package in packages
           collect `(gpkg-install ,@package) into sexp
           finally return `(progn ,@sexp)))

(provide 'gpkg)

;;; gpkg.el ends here
