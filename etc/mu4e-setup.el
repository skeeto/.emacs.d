;;; mu4e-setup.el --- set up environment for mu4e

;; This mounts the remote Maildir and mu database with sshfs before
;; allowing mu4e to begin driving mu. Attempts to unmount when mu4e
;; quits.

;;; Code:

(defvar sshfs-program-name (executable-find "sshfs")
  "Path to sshfs executable.")

(defvar fusermount-program-name (executable-find "fusermount")
  "Path to fusermount executable.")

(defun directory-empty-p (dir)
  "Return t if DIR exists and is an empty directory."
  (when (file-exists-p dir)
    (equal '("." "..") (directory-files dir))))

(defun mu4e-sshfs-mount ()
  "Mount the remote Maildir."
  (let ((maildir (expand-file-name "~/Maildir"))
        (user "wellons")
        (host "mail.nullprogram.com")
        (path "Maildir"))
    (unless (file-exists-p maildir)
      (mkdir maildir))
    (when (directory-empty-p maildir)
      (unless (zerop (call-process
                      sshfs-program-name nil nil nil
                      "-p443" (format "%s@%s:%s" user host path) maildir))
        (error "sshfs failed")))))

(defun mu4e-sshfs-unmount ()
  "Unmount the remote Maildir."
  (zerop
   (call-process fusermount-program-name nil nil nil
                 "-u" (expand-file-name "~/Maildir"))))

(defadvice mu4e (before sshfs-mount activate)
  (mu4e-sshfs-mount))

(defadvice mu4e-quit (after ssfs-unmount activate)
  (mu4e-sshfs-unmount))

(provide 'mu4e-setup)

;;; mu4e-setup.el ends here
