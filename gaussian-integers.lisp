(in-package :cl-user)
(defpackage :gaussian-integers
  (:use :cl))

(in-package :gaussian-integers)

(defun print-g-int (g-int)
  (format nil "(~A+~A)i" (car g-int) (cadr g-int)))

(defun add-g-ints (&rest g-ints)
  (funcall #'apply-g-int-operation #'+ g-ints))

(defun subtract-g-ints (&rest g-ints)
  (funcall #'apply-g-int-operation #'- g-ints))

(defun apply-g-int-operation (fn g-ints)
  (reduce #'(lambda (result item)
              (list (funcall fn (first result) (first item))
                    (funcall fn (second result) (second item))))
          (cdr g-ints) :initial-value (car g-ints)))
