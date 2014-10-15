
;;; See http://programmingpraxis.com/2014/10/14/spiral-wrapping/

(in-package :cl-user)
(defpackage :spiral-wrapping
  (:use :cl :cl-commons)
  (:export :matrix
           :run))

(in-package :spiral-wrapping)

(defvar matrix '((1 2 3 4)
                 (5 6 7 8)
                 (9 10 11 12)
                 (13 14 15 16)
                 (17 18 19 20)))

(defun write-number (number)
  (format t "~D " number))

(defun read-right (list matrix)
  (unless (null matrix)
    (if (null list)
                 (read-up matrix)
                 (progn
                   (write-number (car list))
                   (read-right (cdr list) matrix)))))

(defun read-up (matrix)
  (unless (null matrix)
    (let ((new-matrix (reverse (mapcar #'(lambda (sublist)
                                           (write-number (car (last sublist)))
                                           (butlast sublist))
                                       (reverse matrix)))))
      (read-left (car new-matrix) (cdr new-matrix)))))

(defun read-left (list matrix)
  (unless (null matrix)
    (if (null list)
        (read-down matrix)
        (progn
          (write-number (car (last list)))
          (read-left (butlast list) matrix)))))

(defun read-down (matrix)
  (unless (null matrix)
    (let ((new-matrix (mapcar #'(lambda (sublist)
                                  (write-number (car sublist))
                                  (cdr sublist))
                              matrix)))
      (read-right (car (last new-matrix)) (butlast new-matrix)))))

(defun run (matrix)
  (read-left (car matrix) (cdr matrix)))
