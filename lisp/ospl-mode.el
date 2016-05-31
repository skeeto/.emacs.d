;;; ospl-mode.el --- one-sentence-per-line minor mode

;;; Comments:

;; TODO

;;; Code:

(define-minor-mode ospl-mode
  "One-sentence-per-line minor mode."
  :init-value nil
  :lighter " ospl"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-q") 'ospl-fill-paragraph)
            map)
  (if ospl-mode
      (progn
        (visual-line-mode 1)
        (setq right-margin-width (- (window-body-width) fill-column)))
    (visual-line-mode -1)
    (setq right-margin-width 0))
  ;; Account for new margin width
  (set-window-buffer (selected-window) (current-buffer)))

(defun ospl-unfill-paragraph ()
  "Unfill the paragraph at point.
This repeatedly calls `join-line' until the whole paragraph does
not contain hard line breaks any more."
  (interactive)
  (forward-paragraph 1)
  (forward-paragraph -1)
  (unless (looking-at paragraph-start)
    (forward-line 1))
  (let ((beg (point)))
    (forward-paragraph 1)
    (backward-char 1)
    (while (> (point) beg)
      (join-line)
      (beginning-of-line))))

(defun ospl-fill-paragraph ()
  "Fill the current paragraph until there is one sentence per line.
This unfills the paragraph, and places hard line breaks after
each sentence."
  (interactive)
  (save-excursion
    (fill-paragraph)         ; takes care of putting 2 spaces if needed
    (ospl-unfill-paragraph)  ; remove hard line breaks
    ;; insert line breaks again
    (let ((end-of-paragraph (make-marker)))
      (save-excursion
        (forward-paragraph)
        (backward-sentence)
        (forward-sentence)
        (set-marker end-of-paragraph (point)))
      (forward-sentence) 
      (while (< (point) end-of-paragraph)
        (just-one-space)
        (delete-backward-char 1)
        (newline)
        (forward-sentence))
      (set-marker end-of-paragraph nil)))) 

(provide 'ospl-mode)

;;; ospl-mode.el ends here
