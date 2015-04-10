;;; email-setup.el --- customize my personal email -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'utility)
(require 'notmuch)

;; Displaying zip/tar inline is a really, really stupid default!
(setf mm-inlined-types
      (cl-remove-if (apply-partially #'string-match-p "\\(x-g?tar\\|zip\\)")
                    mm-inlined-types))

(defun notmuch-address-selection-function (prompt collection initial-input)
  (completing-read prompt collection nil nil nil 'notmuch-address-history
                   initial-input))

(add-hook 'message-header-setup-hook
          (lambda ()
            (let* ((name (notmuch-user-name))
                   (email (notmuch-user-primary-email))
                   (header (format "Bcc: %s <%s>\n" name email)))
              (message-add-header header))))

(defun notmuch-search-toggle (tag)
  "Return a function that toggles TAG on the current item."
  (lambda ()
    (interactive)
    (if (member tag (notmuch-search-get-tags))
        (notmuch-search-tag (list (concat "-" tag) "+inbox"))
      (notmuch-search-tag (list (concat "+" tag) "-inbox" "-unread")))))

;; Notmuch mail listing keybindings.

(define-key notmuch-search-mode-map "g"
  'notmuch-poll-and-refresh-this-buffer)

(define-key notmuch-search-mode-map "d"
  (notmuch-search-toggle "trash"))

(define-key notmuch-search-mode-map "S"
  (notmuch-search-toggle "spam"))

(define-key notmuch-hello-mode-map "g"
  'notmuch-poll-and-refresh-this-buffer)

(define-key notmuch-hello-mode-map "i"
  (expose #'notmuch-hello-search "tag:inbox"))

(define-key notmuch-hello-mode-map "u"
  (expose #'notmuch-hello-search "tag:unread"))

(define-key notmuch-hello-mode-map "a"
  (expose #'notmuch-hello-search "tag:archive"))

(provide 'email-setup)

;;; email-setup.el ends here
