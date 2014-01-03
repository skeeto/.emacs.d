;;; uuid-simple.el --- generate high-quality UUIDs

;;; Code:

(defun uuid-get-file-bytes (file count)
  "Get the first COUNT bytes from FILE. Requires the head program
in your path. Useful for reading non-regular files like
/dev/random or /dev/urandom."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (call-process "head" file (current-buffer) nil
                  "-c" (number-to-string count))
    (substring (buffer-string) 0 count)))

(defun make-uuid ()
  "Return a newly generated UUID from system entropy."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (buffer-list)
                        (recent-keys)
                        (when (file-exists-p "/dev/urandom")
                          (uuid-get-file-bytes "/dev/urandom" 16))))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun uuid-insert ()
  "Insert a new UUID at the point."
  (interactive)
  (insert (make-uuid)))

(provide 'uuid-simple)

;;; uuid-simple.el ends here
