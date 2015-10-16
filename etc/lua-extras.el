;;; lua-extras.el --- extra goodies for lua-mode -*- lexical-binding: t; -*-

;;; Code:

(require 'lua-mode)

(defun skeeto/lua-send-buffer (arg)
  "Like `lua-send-buffer', but `lua-restart-with-whole-file' with prefix arg."
  (interactive "P")
  (if arg
      (lua-restart-with-whole-file)
    (lua-send-buffer)))

(defun skeeto/lua-toggle-process-buffer ()
  "Like `lua-show-process-buffer', but toggle it."
  (interactive)
  (if (get-buffer-window lua-process-buffer)
      (lua-hide-process-buffer)
    (lua-show-process-buffer)))

(defun skeeto/lua-error (message)
  "Pop up a Lua error message in `special-mode'."
  (with-current-buffer (get-buffer-create "*lua-error*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert message))
    (pop-to-buffer (current-buffer))))

(defun skeeto/lua-echo (result)
  "Echo comint result in minibuffer."
  (with-temp-buffer
    (insert result)
    (re-search-backward lua-prompt-regexp nil :noerror)
    (let* ((fixed (buffer-substring (point-min) (point)))
           (trim "^[[:space:]]+\\|[[:space:]]+$")
           (actual (replace-regexp-in-string trim "" fixed)))
      (if (string-match-p lua-traceback-line-re actual)
          (skeeto/lua-error actual)
        (message "%s" actual)))))

(defun skeeto/lua-add-filter ()
  "Set up filter in Lua comint buffer."
  (with-current-buffer lua-process-buffer
    (add-hook 'comint-output-filter-functions #'skeeto/lua-echo nil t)))

(provide 'lua-extras)

;;; lua-extras.el ends here
