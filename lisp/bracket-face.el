;;; bracket-face.el --- parenface for other brackets

;;; Code:

(defvar bracket-face 'bracket-face)

(defface bracket-face
    '((((class color))
       (:foreground "Gray60")))
  "Face for displaying a bracket."
  :group 'faces)

(defmacro bracket-face (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
     (let* ((regexp "\\[\\|\\]\\|{\\|}")
            (match (assoc regexp ,keywords)))
       (unless (eq (cdr match) bracket-face)
         (setq ,keywords (cons (cons regexp bracket-face) ,keywords))))))

(provide 'bracket-face)

;;; bracket-face.el ends here
