;;; ctags.el --- an interface to ctags -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This file doesn't read from a "tags" file, but instead invokes
;; "ctags -x" on demand. Modern computers are plenty fast that only
;; truly gigantic projects really need a "tags" database. Currently it
;; relies on Exuberant Ctags' "-R" option.

;; This package provides one command: M-x `ctags-jump'. The suggested
;; binding:

;;   (global-set-key (kbd "M-.") #'ctags-jump)

;; I haven'te bothered implementing a tag stack since I normally rely
;; on Evil's jump list (C-o, C-i).

;;; Code:

(require 'cl-lib)

(defvar ctags-program-name "ctags"
  "The installation path for ctags, or simply the name in PATH.")

(defvar ctags-dominating-file "Makefile"
  "The file used to identify the root of the current project.")

(defun ctags--extract-tag-info ()
  "Extract file and line number information from the current line.
This must only be called after a `re-search-forward' on the tag."
  (let* ((tag (match-string 0))
         (lineno (progn (re-search-forward "[0-9]+")
                        (string-to-number (match-string 0))))
         (file (progn (re-search-forward "\\S-+")
                      (match-string 0))))
    (list tag (expand-file-name file default-directory) lineno)))

(defun ctags-entries (&optional tag)
  "Return a list of all tag entries, or just the entries for TAG."
  (let ((default-directory
          (or (locate-dominating-file
               default-directory ctags-dominating-file)
              default-directory)))
    (with-temp-buffer
      (call-process ctags-program-name nil t nil "-Rx")
      (setf (point) (point-min))
      (cl-loop with regexp =
               (if tag
                   (format "^%s" (regexp-opt (list tag) 'symbols))
                 "^\\S-+")
               while (re-search-forward regexp nil t)
               collect (ctags--extract-tag-info)))))

(defun ctags--visit (tag file n)
  "Switch to FILE, line number N, at TAG."
  (switch-to-buffer (find-file-noselect file))
  (goto-char (point-min))
  (forward-line (1- n))
  (recenter)
  (let ((regexp (regexp-opt (list tag) 'symbols)))
    (when (re-search-forward regexp (line-end-position) t)
      (setf (point) (match-beginning 0)))))

(defun ctags-jump (tag)
  "Switch to buffer and line for TAG.

If already visiting this tag, go to the next instance of this tag.

When called interactively, TAG is the symbol under the point.
Given a prefix argument, read TAG from the minibuffer."
  ;; TODO: don't run ctags an extra time just for completion
  (interactive
   (let ((tag (thing-at-point 'symbol)))
     (if (or (null tag) current-prefix-arg)
         (let* ((tags (mapcar #'car (ctags-entries)))
                (tag-valid-p (member tag tags))
                (prompt (if tag-valid-p (format "Tag (%s): " tag) "Tag: ")))
           (list (completing-read
                  prompt tags nil t nil nil (if tag-valid-p tag))))
       (list tag))))
  (let* ((current (list tag buffer-file-name (line-number-at-pos)))
         (entries (ctags-entries tag))
         (entry (car (or (cdr (member current entries))
                         entries))))
    (when entry
      (apply #'ctags--visit entry))))

(provide 'ctags)

;;; ctags.el ends here
