;;; utility.el --- Elisp Utility Functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl-lib)

;; Higher-order functions

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
  (lambda (&rest _) value))

(defun complement (function)
  "Return a function that takes the same arguments as FUNCTION
and returns the opposite truth value."
  (lambda (&rest args)
    (not (apply function args))))

(defun compose (&rest functions)
  "Return a function which is created by composing FUNCTIONS
right-associatively."
  (lambda (x)
    (cl-reduce #'funcall functions :initial-value x :from-end t)))

(defun expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

;; Utility

(defun partition (predicate seq)
  "Divide SEQ into two lists, selected by PREDICATE."
  (cl-loop for element in (cl-coerce seq 'list)
           when (funcall predicate element) collect element into a
           else collect element into b
           finally (return (list a b))))

;; Anaphoric macros

(defmacro amap (expression seq)
  "Anahoric map: binds IT in expression to SEQ elements."
  `(mapcar (lambda (it) ,expression) ,seq))

(defmacro aif (condition then &rest else)
  "Anaphoric if: binds IT to the condition."
  `(let ((it ,condition))
     (if it ,then ,@else)))

(defmacro awhile (condition &rest body)
  "Anaphoric while: binds IT to CONDITION in BODY."
  (declare (indent defun))
  `(let (it)
     (while (setf it ,condition)
       ,@body)))

(defmacro alambda (params &rest body)
  "Anaphoric lambda: binds SELF to the anonymous function itself."
  (declare (indent defun))
  `(labels ((self ,params ,@body))
     #'self))

(provide 'utility)

;;; utility.el ends here
