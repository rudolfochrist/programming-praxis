;;;
;;; see http://programmingpraxis.com/2014/09/30/thue-morse-sequence/

(in-package :cl-user)
(defpackage :thue-morse
  (:use :cl :cl-commons)
  (:export :tm))

(in-package :thue-morse)

(defun invert (number)
  (if (zerop number)
      1
      0))

(defun complement-list (list)
  (mapcar #'invert list))

(defun tm (n)
  (if (zerop n)
      '(0)
      (let ((step (tm (1- n))))
        (append step (complement-list step)))))

