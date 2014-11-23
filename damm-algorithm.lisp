;;; see http://programmingpraxis.com/2014/11/18/damms-algorithm/
(in-package :cl-user)
(defpackage :damm
  (:use :cl))

(in-package :damm)

(defvar lookup-table
  '((0 3 1 7 5 9 8 6 4 2)
    (7 0 9 2 1 5 4 8 6 3)
    (4 2 0 6 8 7 1 3 5 9)
    (1 7 5 0 9 8 3 4 2 6)
    (6 1 2 3 0 4 5 9 7 8)
    (3 6 7 4 2 0 9 5 8 1)
    (5 8 6 9 7 2 0 1 3 4)
    (8 9 4 5 3 6 2 0 1 7)
    (9 4 3 8 6 1 7 2 0 9)
    (2 5 8 1 4 3 6 7 9 0)))

(defun digits (number)
  (map 'list #'digit-char-p (write-to-string number)))

(defun check-digit (current-digit digits)
  (if (null digits)
      current-digit
      (let ((new-digit (nth (car digits) (nth current-digit lookup-table))))
        (check-digit new-digit (cdr digits)))))

(defun check (number)
  (+ (* 10 number) (check-digit 0 (digits number))))

(defun valid? (number)
  (= 0 (check-digit 0 (digits number))))

(defun check (number))
