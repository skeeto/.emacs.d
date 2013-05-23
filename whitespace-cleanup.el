;;; whitespace-cleanup.el --- perform whitespace cleanup on save

;;; Commentary:

;; Maybe this should be a minor mode?

;;; Code:

(require' whitespace)

(defvar-local whitespace-cleanup-p t
  "When true, do `whitespace-cleanup' when the current buffer is saved.")

(defun toggle-whitespace-cleanup ()
  "Turn the `whitespace-cleanup-safe' hook on and off."
  (interactive)
  (setq whitespace-cleanup-p (not whitespace-cleanup-p))
  (message "whitespace-cleanup %s"
           (if whitespace-cleanup-p "enabled" "disabled")))

(defun whitespace-cleanup-safe ()
  "Run `whitespace-cleanup' only when it makes sense to do so."
  (when (and (not buffer-read-only) whitespace-cleanup-p)
    ;; turn off and on to work around Emacs bug #4069
    (whitespace-turn-on)
    (whitespace-turn-off)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'whitespace-cleanup-safe)
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

(provide 'whitespace-cleanup)

;;; whitespace-cleanup.el ends here
