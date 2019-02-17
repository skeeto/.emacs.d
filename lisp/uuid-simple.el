;;; uuid-simple.el --- UUID generator -*- lexical-binding: t; -*-

;;; Code:

(defvar uuid--state
  (if (executable-find "dd")
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (call-process "dd" nil t nil "if=/dev/urandom" "bs=48" "count=1")
        (buffer-string))
    (prin1-to-string (list (current-time) (emacs-pid) (random))))
  "A buffer of random data used for generating UUIDs.")

(defun make-uuid ()
  "Return a newly generated UUID from system entropy."
  (let ((uuid (make-string 36 0)))
    (prog1 uuid
      (setf uuid--state (secure-hash 'sha384 uuid--state nil nil t))
      (dotimes (i (length uuid))
        (setf (aref uuid i)
              (aref "0123456789abcdef" (logand (aref uuid--state i) #xf))))
      (setf (aref uuid  8) ?-
            (aref uuid 13) ?-
            (aref uuid 18) ?-
            (aref uuid 23) ?-))))

(defun uuid-insert ()
  "Insert a new UUID at the point."
  (interactive)
  (insert (make-uuid)))

(provide 'uuid-simple)

;;; uuid-simple.el ends here
