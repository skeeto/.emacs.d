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
  "Generate and return a verion 4 UUID."
  (let ((state (secure-hash 'sha384 uuid--state nil nil t))
        (hex (eval-when-compile
               (cl-loop repeat 16
                        collect "0123456789abcdef" into hex
                        finally return (mapconcat #'identity hex "")))))
    (setf uuid--state state)
    (string (aref hex (aref state  0))
            (aref hex (aref state  1))
            (aref hex (aref state  2))
            (aref hex (aref state  3))
            (aref hex (aref state  4))
            (aref hex (aref state  5))
            (aref hex (aref state  6))
            (aref hex (aref state  7))
            ?-
            (aref hex (aref state  9))
            (aref hex (aref state 10))
            (aref hex (aref state 11))
            (aref hex (aref state 12))
            ?-
            ?4
            (aref hex (aref state 15))
            (aref hex (aref state 16))
            (aref hex (aref state 17))
            ?-
            (aref "89ab" (logand (aref state 19) #x3))
            (aref hex (aref state 20))
            (aref hex (aref state 21))
            (aref hex (aref state 22))
            ?-
            (aref hex (aref state 24))
            (aref hex (aref state 25))
            (aref hex (aref state 26))
            (aref hex (aref state 27))
            (aref hex (aref state 28))
            (aref hex (aref state 29))
            (aref hex (aref state 30))
            (aref hex (aref state 31))
            (aref hex (aref state 32))
            (aref hex (aref state 33))
            (aref hex (aref state 34))
            (aref hex (aref state 35)))))

(defun uuid-insert ()
  "Insert a new UUID at the point."
  (interactive)
  (insert (make-uuid)))

(provide 'uuid-simple)

;;; uuid-simple.el ends here
