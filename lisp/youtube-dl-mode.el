;;; youtube-dl-mode.el --- run youtube-dl in a buffer

;; This mode manages a youtube-dl process, automatically killing the
;; associated buffer if the download completed successfully. The main
;; entry point for this mode is `youtube-dl-download'.

;; This mode was created for use with the Elfeed web feed reader.

;;; Code:

(require 'url-parse)

(defcustom youtube-dl-directory "~"
  "Directory in which to run youtube-dl.")

(defcustom youtube-dl-arguments '("--title" "--no-mtime" "--restrict-filenames")
  "Arguments to be send to youtube-dl.")

(defvar youtube-dl-hosts '("www.youtube.com" "youtube.com" "youtu.be")
  "Domain names for YouTube.")

(defun youtube-dl-get-id (url)
  "Get the YouTube video ID from URL. Returns nil for invalid URLs."
  (let* ((obj (url-generic-parse-url url))
         (host (url-host obj))
         (path (url-filename obj))
         (match (string-match-p "[-_a-zA-Z0-9]\\{11\\}" path)))
    (when (and match (member host youtube-dl-hosts))
      (substring path match (+ match 11)))))

(defun youtube-dl-quit ()
  "Kill the current if the process is complete, else bury it."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (quit-window)
    (kill-buffer (current-buffer))))

(defun youtube-dl-sentinel (process event)
  "Responds to completed youtube-dl processes."
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (when (string-match-p "finished" event)
        (message "youtube-dl %s completed." (youtube-dl-get-id youtube-dl-url))
        (kill-buffer buffer))
      (when (string-match-p "abnormal" event)
        (message "youtube-dl %s failed." (youtube-dl-get-id youtube-dl-url))))))

(defvar youtube-dl-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "q" 'youtube-dl-quit)))
  "Keymap for `youtube-dl-mode'.")

(defvar youtube-dl-url nil
  "The URL being downloaded in this buffer.")

(defun youtube-dl-mode ()
  "Major mode for showing youtube-dl processes. You probably
don't want to run this function directly, see `youtube-dl-download'."
  (kill-all-local-variables)
  (make-local-variable 'youtube-dl-url)
  (use-local-map youtube-dl-mode-map)
  (setq major-mode 'youtube-dl-mode
        mode-name "youtube-dl"
        buffer-read-only t)
  (run-hooks 'youtube-dl-mode-hook))

(defun youtube-dl-download (url)
  "Download the video at URL with youtube-dl. Returns the buffer
that will show progress output. The buffer is killed if the
download completes successfully."
  (interactive (list (read-from-minibuffer "URL: " (x-get-selection-value))))
  (let ((id (youtube-dl-get-id url)))
    (when id
      (let* ((process-name (format "youtube-dl-%s" id))
             (buffer-name (format "*youtube-dl %s*" id))
             (buffer (get-buffer-create buffer-name)))
        (unless (get-buffer-process buffer)
          (with-current-buffer buffer
            (erase-buffer)
            (youtube-dl-mode)
            (setq youtube-dl-url url)
            (setq default-directory
                  (concat (directory-file-name youtube-dl-directory) "/"))
            (set-process-sentinel
             (apply #'start-process process-name buffer "youtube-dl" "--newline"
                    (append youtube-dl-arguments (list "--" id)))
             'youtube-dl-sentinel)))
        buffer))))

(provide 'youtube-dl-mode)

;;; youtube-dl-mode.el ends here
