(in-package :cl-user)
(defpackage #:interview-questions
  (:nicknames :iq)
  (:use :cl))

(in-package :interview-questions)

;;; see https://github.com/kensterz/interview-questions-in-javascript

;;; 1.1 Given an array of integers, find the largest product yielded from three of the integers

(defvar *numbers* (list -10 7 29 30 5 -10 -70))

(product *numbers*)                     ;=> 8

(defun product (&optional (numbers *numbers*))
  (let* ((sorted (sort (copy-list numbers) #'<))
         (last-3 (last sorted 3))
         (first-3 (append (subseq sorted 0 2) (last sorted))))
    ;; the largest product of three elements of a sorted sequence S of size n is
    ;; 1. S_1 * S_2 * S_n or
    ;; 2. S_n-2 * S_n-1 * S_n
    (max (apply #'* last-3)
         (apply #'* first-3))))

;;; 1.2 Being told that an unsorted array contains (n - 1) of n consecutive numbers (where the bounds are defined),
;;; find the missing number in O(n) time

(defvar *integers* (list 2  5  1  4  9  6  3  7))
(defvar *upper-bound* 9)
(defvar *lower-bound* 1)

;;; (find-missing-number *integers* *upper-bound* *lower-bound*)  ;=> 8

(defun find-missing-number (numbers upper-bound lower-bound)
  (let ((sum-1 (apply #'+ numbers))
        (sum-2 (loop for i from lower-bound to upper-bound sum i)))
    (- sum-2 sum-1)))

;;; 1.3 Removing duplicates of an array and returning an array of only unique elements

(defvar *list* (list 1  2  3  5  1  5  9  1  2  8))

;;; (uniques-1 *list*)  ;=> (1 2 3 5 9 8)
;;; (uniques-2 *list*)  ;=> (1 2 3 5 9 8)

;;; cheapskate common lisp
(defun uniques-1 (list)
  (remove-duplicates list))

;;; self
(defun uniques-2 (list)
  (loop with set = (make-hash-table :size (length list))
     for number in list
     unless (gethash number set)
     collect (setf (gethash number set) number)))
