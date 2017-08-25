;;; ctags.el --- an interface to Exuberant Ctags -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This file doesn't read from a "tags" file, but instead invokes
;; "ctags -x" on demand. Modern computers are plenty fast that only
;; truly gigantic projects really need a "tags" database. Currently it
;; relies on Exuberant Ctags' "-R" option.

;; This package provides one command: M-x `ctags-find'. The suggested
;; binding:

;;   (global-set-key (kbd "M-.") #'ctags-find)
;;   (global-set-key (kbd "M-?") #'ctags-find-reference)

;; I haven't bothered implementing a tag stack since I normally rely
;; on Evil's jump list (C-o, C-i).

;;; Code:

(require 'cl-lib)

(defvar ctags-program-name "ctags"
  "The installation path for ctags, or simply the name in PATH.")

(defvar ctags-dominating-file "Makefile"
  "The file used to identify the root of the current project.")

(defmacro ctags--with-temp-buffer (&rest body)
  "Return the root"
  (declare (indent defun))
  `(let ((default-directory (locate-dominating-file "." ctags-dominating-file)))
     (with-temp-buffer
       ,@body)))

(defun ctags--extract-tag ()
  "Extract file and line number information from the current line.
This must only be called after a `re-search-forward' on the tag."
  (let* ((tag (match-string 0))
         (lineno (progn (re-search-forward "[0-9]+")
                        (string-to-number (match-string 0))))
         (file (progn (re-search-forward "\\S-+")
                      (match-string 0))))
    (list tag (expand-file-name file default-directory) lineno)))

(defun ctags--entries (&optional tag)
  "Return a list of all tag entries, or just the entries for TAG."
  (ctags--with-temp-buffer
    (call-process ctags-program-name nil t nil "-Rx")
    (setf (point) (point-min))
    (cl-loop with regexp =
             (if tag
                 (format "^%s" (regexp-opt (list tag) 'symbols))
               "^\\S-+")
             while (re-search-forward regexp nil t)
             collect (ctags--extract-tag))))

(defun ctags--visit (tag file n)
  "Switch to FILE, line number N, at TAG."
  (switch-to-buffer (find-file-noselect file))
  (goto-char (point-min))
  (forward-line (1- n))
  (recenter)
  (let ((regexp (regexp-opt (list tag) 'symbols)))
    (when (re-search-forward regexp (line-end-position) t)
      (setf (point) (match-beginning 0)))))

(defun ctags--guess-tag (prefix)
  "Try to guess what tag the user wants, or just ask."
  ;; TODO: don't run ctags an extra time just for completion
  (let ((tag (thing-at-point 'symbol)))
    (if (or (null tag) prefix)
        (let* ((tags (mapcar #'car (ctags--entries)))
               (tag-valid-p (member tag tags))
               (prompt (if tag-valid-p (format "Tag (%s): " tag) "Tag: ")))
          (completing-read prompt tags nil t nil nil (if tag-valid-p tag)))
      tag)))

(defun ctags-find (tag)
  "Switch to buffer and line for the next definition of TAG.

When called interactively, TAG is the symbol under the point.
Given a prefix argument, read TAG from the minibuffer."
  (interactive (list (ctags--guess-tag current-prefix-arg)))
  (let* ((current (list tag buffer-file-name (line-number-at-pos)))
         (entries (ctags--entries tag))
         (entry (car (or (cdr (member current entries))
                         entries))))
    (when entry
      (apply #'ctags--visit entry))))

(defun ctags--grep-tag (tag entries)
  "Return all references to TAG not included in ENTRIES."
  (let ((files (cl-delete-duplicates (mapcar #'cadr entries) :test #'equal))
        (regexp (format "\\<%s\\>" tag)))
    (ctags--with-temp-buffer
      (apply #'call-process "grep" nil t nil "-n" regexp "/dev/null" files)
      (setf (point) (point-min))
      (let ((refs ()))
        (while (re-search-forward "^[^:]+" nil t)
          (let* ((file (match-string 0))
                 (lineno (progn (re-search-forward "[0-9]+")
                                (string-to-number (match-string 0))))
                 (ref (list tag file lineno)))
            (unless (member ref entries)
              (push ref refs))))
        (nreverse refs)))))

(defun ctags-find-reference (tag)
  "Switch to buffer and line for the next reference to TAG.

Tags are discovered using grep, and definitions are skipped in
this search. When called interactively, TAG is the symbol under
the point. Given a prefix argument, read TAG from the minibuffer."
  (interactive (list (ctags--guess-tag current-prefix-arg)))
  (let* ((current (list tag buffer-file-name (line-number-at-pos)))
         (entries (ctags--entries tag))
         (refs (ctags--grep-tag tag entries))
         (ref (car (or (cdr (member current refs))
                       refs))))
    (when ref
      (apply #'ctags--visit ref))))

(provide 'ctags)

;;; ctags.el ends here
