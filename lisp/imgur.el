;;; imgur.el --- various functions for interacting with imgur.com

;; This is free and unencumbered software released into the public domain.

;; A work in progress.

;;; Code:

(require 'cl-lib)
(require 'json)

(defun imgur/get-json (url)
  "Get JSON data from an imgur album at URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (re-search-forward " image[[:space:]]*:"))
    (json-read)))

(defun imgur/get-hashes (json)
  "Get the list of image hash IDs from JSON."
  (cl-map 'list (lambda (e) (cdr (assoc 'hash e)))
          (cdr (assoc 'images (cdr (assoc 'album_images json))))))

(defun imgur/insert-wget-script (prefix hashes)
  "Insert a download script with a filename PREFIX for the list of HASHES."
  (let ((count 0))
    (dolist (hash hashes)
      (insert (format "wget -O %s-%03d-%s.jpg http://i.imgur.com/%s.jpg\n"
                      prefix count hash hash))
      (cl-incf count))))

;;;###autoload
(defun imgur/gen-script (prefix url)
  "Insert a download script with file PREFIX for the image album at URL."
  (interactive "sPrefix: \nsUrl: ")
  (imgur/insert-wget-script prefix (imgur/get-hashes (imgur/get-json url))))

(provide 'imgur)

;;; imgur.el ends here
