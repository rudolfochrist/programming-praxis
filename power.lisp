(in-package :cl-user)
(defpackage :powering
  (:use :cl))

(in-package :powering)

(defun pow-linear (n expn)
  (let ((result 1))
      (dotimes (i expn result)
        (setf result (* n result)))))
