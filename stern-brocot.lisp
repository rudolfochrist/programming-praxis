;;; see http://programmingpraxis.com/2014/12/09/every-possible-fraction/

(in-package :cl-user)
(defpackage :stern-brocot
  (:nicknames :stb)
  (:use :cl))

(in-package :stern-brocot)

(defun next-x (x)
  (let* ((n (floor x))
         (y (- x n)))
    (/ 1 (+ 1 (- n y)))))

(defun collect (upper-bound)
  (labels ((recu (list counter)
             (if (< counter upper-bound)
                 (recu (cons (next-x (car list)) list) (1+ counter))
                 list)))
    (reverse (recu '(1) 0))))
