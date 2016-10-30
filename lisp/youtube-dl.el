;;; youtube-dl.el --- manages a youtube-dl queue

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; The `youtube-dl' function enqueues a URL for download. Only one
;; video is downloaded at a time, and failures are retried up to
;; `youtube-dl-max-failures'.

;; The `youtube-dl-list' command displays a list of all active
;; video downloads.

;;; Code:

(require 'cl-lib)

(defgroup youtube-dl ()
  "Download queue for the youtube-dl command line program."
  :group 'external)

(defcustom youtube-dl-directory "~"
  "Directory in which to run youtube-dl."
  :group 'youtube-dl)

(defcustom youtube-dl-program "youtube-dl"
  "The name of the program invoked for downloading YouTube videos."
  :group 'youtube-dl)

(defcustom youtube-dl-arguments
  '("--title" "--no-mtime" "--restrict-filenames"
    "--format" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best")
  "Arguments to be send to youtube-dl."
  :group 'youtube-dl)

(defcustom youtube-dl-max-failures 10
  "Maximum number of retries for a single video."
  :group 'youtube-dl)

(cl-defstruct (youtube-dl-item (:constructor youtube-dl-item--create))
  "Represents a single video to be downloaded with youtube-dl."
  id failures destination progress total)

(defvar youtube-dl-items ()
  "List of all items still to be downloaded.")

(defvar youtube-dl-process nil
  "The currently active youtube-dl process.")

(defun youtube-dl--next ()
  "Returns the next item to be downloaded."
  (let (best)
    (dolist (item youtube-dl-items best)
      (let ((failures (youtube-dl-item-failures item)))
        (when (< failures youtube-dl-max-failures)
          (cond ((null best)
                 (setf best item))
                ((< failures (youtube-dl-item-failures best))
                 (setf best item))))))))

(defun youtube-dl--remove (item)
  "Remove ITEM from the queue."
  (setf youtube-dl-items (cl-delete item youtube-dl-items)))

(defun youtube-dl--add (item)
  "Add ITEM to the queue."
  (setf youtube-dl-items (nconc youtube-dl-items (list item))))

(defun youtube-dl--sentinel (proc status)
  (let ((item (plist-get (process-plist proc) :item)))
    (setf youtube-dl-process nil)
    (if (equal status "finished\n")
        (youtube-dl--remove item)
      (cl-incf (youtube-dl-item-failures item)))
    (youtube-dl--run)))

(defun youtube-dl--progress (output)
  "Return the download progress for the given output.
Progress lines that straddle output chunks are lost. That's fine
since this is just used for display purposes."
  (let ((start 0)
        (pair nil))
    (while (string-match "\\([^ ]+%\\) +of +\\([^ ]+\\) " output start)
      (setf pair (cons (match-string 1 output) (match-string 2 output))
            start (match-end 0)))
    pair))

(defun youtube-dl--destination (output)
  "Return the destination file for the given output (if any).
The destination filename may potentially straddle two output
chunks, but this is incredibly unlikely. It's only used for
display purposes anyway."
  (when (string-match " Destination: \\([^\n]+\\)" output)
    (match-string 1 output)))

(defun youtube-dl--filter (proc output)
  (let ((item (plist-get (process-plist proc) :item))
        (progress (youtube-dl--progress output))
        (destination (youtube-dl--destination output)))
    (when progress
      (cl-destructuring-bind (percentage . total) progress
        (setf (youtube-dl-item-progress item) percentage
              (youtube-dl-item-total item) total)))
    (when destination
      (setf (youtube-dl-item-destination item) destination))
    (youtube-dl--redisplay)))

(defun youtube-dl--run ()
  "Try to launch a new download."
  (let ((item (youtube-dl--next)))
    (when item
      (let* ((default-directory
               (concat (directory-file-name youtube-dl-directory) "/"))
             (id (youtube-dl-item-id item))
             (proc (apply #'start-process
                          "youtube-dl" nil youtube-dl-program "--newline"
                          (append youtube-dl-arguments (list "--" id)))))
        (set-process-plist proc (list :item item))
        (set-process-sentinel proc #'youtube-dl--sentinel)
        (set-process-filter proc #'youtube-dl--filter)
        (setf youtube-dl-process proc)))
    (youtube-dl--redisplay)))

(defun youtube-dl--id-from-url (url)
  "Return the 11-character video ID for URL."
  (let ((id-start (string-match-p "[-_a-zA-Z0-9]\\{11\\}" url)))
    (when id-start
      (substring url id-start (+ id-start 11)))))

(defun youtube-dl (url)
  "Queues URL for download using youtube-dl."
  (let* ((id (youtube-dl--id-from-url url))
         (item (youtube-dl-item--create :id id :failures 0)))
    (when id
      (youtube-dl--add item)
      (if youtube-dl-process
          (youtube-dl--redisplay)
        (youtube-dl--run)))))

(defun youtube-dl-list-redisplay ()
  "Immediately redraw the queue list buffer."
  (interactive)
  (let ((point (point)))
    (youtube-dl--fill-listing)
    (setf (point) point)))

(defun youtube-dl--redisplay ()
  "Redraw the queue list buffer only if visible."
  (when (get-buffer-window (youtube-dl--buffer))
    (youtube-dl-list-redisplay)))

(defun youtube-dl-list-kill ()
  "Remove the selected item from the queue."
  (interactive)
  (let* ((n (1- (line-number-at-pos)))
         (item (nth n youtube-dl-items)))
    (when item
      (youtube-dl--remove item)
      (if (null youtube-dl-process)
          (youtube-dl--redisplay)
        (let* ((plist (process-plist youtube-dl-process))
               (current-item (plist-get plist :item)))
          (if (eq item current-item)
              ;; Sentinel will cleanup.
              (kill-process youtube-dl-process)
            (youtube-dl--redisplay)))))))

(defun youtube-dl--buffer ()
  "Returns the queue listing buffer."
  (with-current-buffer (get-buffer-create "*youtube-dl list*")
    (special-mode)
    (hl-line-mode)
    (setf header-line-format
          (format " %-11s %-6.6s %-12.12s %-40.40s"
                  "id" "done" "size" "destination"))
    (local-set-key "g" #'youtube-dl-list-redisplay)
    (local-set-key "k" #'youtube-dl-list-kill)
    (current-buffer)))

(defun youtube-dl--fill-listing ()
  "Erase and redraw the queue in the queue listing buffer."
  (with-current-buffer (youtube-dl--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (item youtube-dl-items)
        (let ((id (youtube-dl-item-id item))
              (failures (youtube-dl-item-failures item))
              (progress (youtube-dl-item-progress item))
              (total (youtube-dl-item-total item))
              (destination (youtube-dl-item-destination item)))
          (insert (format "%-11s %-6.6s %-12.12s %-40.40s%s\n"
                          id
                          (or progress "0.0%")
                          (or total "???")
                          (or destination "")
                          (cond ((= failures 0) "")
                                ((= failures 1) "1 try")
                                (t (format " %d tries" failures))))))))))

(defun youtube-dl-list ()
  "Display a list of all videos queued for download."
  (interactive)
  (youtube-dl--fill-listing)
  (pop-to-buffer (youtube-dl--buffer)))

(provide 'youtube-dl)

;;; youtube-dl.el ends here
