;;; utility.el --- Elisp Utility Functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl)

(defun partition (predicate seq)
  "Divide SEQ into two lists, selected by PREDICATE."
  (loop for element in (coerce seq 'list)
        when (funcall predicate element) collect element into a
        else collect element into b
        finally (return (list a b))))

(defun curry (function args)
  "Partially apply FUNCTION to ARGS."
  (lambda (&rest more-args)
    (apply function args more-args)))

(defun rcurry (function args)
    "Partially apply FUNCTION to ARGS as the rightmost arguments."
  (lambda (&rest more-args)
    (apply function more-args args)))

(defun constantly (value)
  "Return a function that accepts any number of arguments, has no
side-effects, and always returns VALUE."
  (lambda (&rest args) value))

(defun complement (function)
  "Return a function that takes the same arguments as FUNCTION
and returns the opposite truth value."
  (lambda (&rest args)
    (not (apply function args))))

(defun compose (&rest functions)
  "Return a function which is created by composing FUNCTIONS
right-associatively."
  (lambda (x)
    (reduce #'funcall functions :initial-value x :from-end t)))

;;; utility.el ends here
