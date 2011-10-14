;;; highlight-tags-mode.el --- highlight matching HTML tags

;;; Commentary:

;; Created by reddit user deactivated.
;; http://reddit.com/r/emacs/comments/ha7l9/h/c1tt4a6

;;; Code:

(defvar hilight-tags-start-overlay nil)
(make-variable-buffer-local 'hilight-tags-start-overlay)

(defvar hilight-tags-end-overlay nil)
(make-variable-buffer-local 'hilight-tags-end-overlay)

(defun hilight-tags-context ()
  (save-excursion
    (let ((ctx (sgml-get-context)))
      (and ctx
           (if (eq (sgml-tag-type (car ctx)) 'close)
               (cons (sgml-get-context) ctx)
             (cons ctx (progn
                         (sgml-skip-tag-forward 1)
                         (backward-char 1)
                         (sgml-get-context))))))))

(defun highlight-tags-update ()
  (let ((ctx (hilight-tags-context)))
    (if (null ctx)
        (hilight-tags-hide)
      (hilight-tags-show)
      (move-overlay hilight-tags-end-overlay
                    (sgml-tag-start (caar ctx))
                    (sgml-tag-end (caar ctx)))
      (move-overlay hilight-tags-start-overlay
                    (sgml-tag-start (cadr ctx))
                    (sgml-tag-end (cadr ctx))))))

(defun hilight-tags-show ()
  (unless hilight-tags-start-overlay
    (setq hilight-tags-start-overlay (make-overlay 1 1)
          hilight-tags-end-overlay (make-overlay 1 1))
    (overlay-put hilight-tags-start-overlay 'face 'show-paren-match-face)
    (overlay-put hilight-tags-end-overlay 'face 'show-paren-match-face)))

(defun hilight-tags-hide ()
  (when hilight-tags-start-overlay
    (delete-overlay hilight-tags-start-overlay)
    (delete-overlay hilight-tags-end-overlay)))

(define-minor-mode highlight-tags-mode
  "Toggle highlight-tags mode."
  nil "" nil
  (if highlight-tags-mode
      (add-hook 'post-command-hook 'highlight-tags-update nil t)
    (remove-hook 'post-command-hook 'highlight-tags-update t)
    (hilight-tags-hide)))

(provide 'highlight-tags-mode)
