;;; keyboard-cat-mode.el --- give the illusion that you're a fast typist

;;; Commentary:

;; http://reddit.com/r/emacs/comments/1orlrr/_/ccuxj9w

;;; Code:

(defvar-local keyboard-cat-overlay nil
  "Invisibility overlay for `keyboard-cat-mode'.")

(defvar keyboard-cat-step-function (lambda () 1)
  "Function that the number of characters to \"emit\" at a time.
This is a function so that a random number could be returned for
each individual key event.")

(defvar keyboard-cat-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [remap self-insert-command] #'keyboard-cat-next)
      (define-key map [remap keyboard-quit] #'keyboard-cat-mode)))
  "Keymap for `keyboard-cat-mode'.")

(define-minor-mode keyboard-cat-mode
  "Clear the buffer and set the keymap to re-type the buffer on all input.
Gives the illusion that you're a fast typist."
  :keymap keyboard-cat-mode-map
  (if keyboard-cat-mode
      (overlay-put
       (setf keyboard-cat-overlay (make-overlay (point-min) (point-max)))
       'invisible t)
    (delete-overlay keyboard-cat-overlay)))

(defun keyboard-cat-next ()
  "Insert the next character in this `keyboard-cat-mode' buffer."
  (interactive)
  (move-overlay keyboard-cat-overlay
                (min (+ (funcall keyboard-cat-step-function)
                        (overlay-start keyboard-cat-overlay)) (point-max))
                (overlay-end keyboard-cat-overlay))
  (goto-char (point-max)))

(provide 'keyboard-cat-mode)

;;; keyboard-cat-mode.el ends here
