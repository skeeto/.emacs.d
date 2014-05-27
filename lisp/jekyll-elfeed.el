;;; jekyll-elfeed.el --- Jekyll/Elfeed integration

;;; Code:

(require 'jekyll)
(require 'elfeed)
(require 'url-parse)

(defun jekyll/url-base (url)
  "Return base URL for URL."
  (if (string-match-p "youtube\\.com" url)
      (save-match-data
        (string-match "\\([^/]+\\)/uploads" url)
        (format "http://www.youtube.com/user/%s" (match-string 1 url)))
    (let ((obj (url-generic-parse-url url)))
      (setf (url-filename obj) nil
            (url-target obj) nil)
      (url-recreate-url obj))))

(cl-defun jekyll/insert-urls (filter &optional (years 0.5))
  "Insert feeds sorted by frequency into the current buffer."
  (interactive (list (read-from-minibuffer "Filter: ")
                     (read-number "Within last X years: " 0.5)))
  (let ((table (make-hash-table))
        (base (make-hash-table))
        (compiled (elfeed-search-parse-filter filter))
        (limit (- (float-time) (* 60 60 24 365 years))))
    (with-elfeed-db-visit (entry feed)
      (when (< (elfeed-entry-date entry) limit)
        (elfeed-db-return))
      (when (elfeed-search-filter compiled entry feed)
        (incf (gethash feed table 0))
        (let ((url (elfeed-entry-link entry)))
          (unless (string-match-p "feedproxy" url) ; sorry, feedproxy sucks
            (if (string-match-p "youtube\\.com" url)
                (setf (gethash feed base)
                      (jekyll/url-base (elfeed-feed-url feed)))
              (setf (gethash feed base) (jekyll/url-base url)))))))
    (save-excursion
      (setf (point) (point-max))
      (cl-loop for feed being the hash-keys of table
               using (hash-value count)
               for url = (gethash feed base)
               for title = (replace-regexp-in-string
                            "^Uploads by \\| ?[:-]+ .+" ""
                            (elfeed-feed-title feed))
               when url collect (list url title count) into results
               finally
               (cl-loop with list = (cl-sort results #'> :key #'cl-third)
                        for (url title _) in list
                        do (progn
                             (insert "<li>")
                             (insert (format "<a href=\"%s\">%s</a>" url title))
                             (insert "</li>\n")))))))

(provide 'jekyll-elfeed)

;;; jekyll-elfeed.el ends here
