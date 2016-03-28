(in-package :cl-user)
(defpackage #:interview-questions
  (:nicknames :iq)
  (:use :cl))

(in-package :interview-questions)

;;; see https://github.com/kensterz/interview-questions-in-javascript

;;; 1.1 Given an array of integers, find the largest product yielded from three of the integers

(defvar *numbers* (list -10 7 29 30 5 -10 -70))

;; (product *numbers*)                     ;=> 8

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

;; 1.4 Given an array of integers, find the largest difference between
;;     two elements such that the element of lesser value must come before
;;     the greater element

(defvar *list2* (list 7  8  4  9  9  15  3  1  10))

;; (7  8  4  9  9  15  3  1  10) would return `11` based on the
;; difference between `4` and `15` Notice: It is not `14` from the
;; difference between `15` and `1` because 15 comes before 1.

;;; (find-largest-difference *list2*)  ;=> 11

(defun find-largest-difference (list)
  (let* ((max-n (apply #'max list))
         (min-n (apply #'min (subseq list 0 (position max-n list)))))
    (- max-n min-n)))

;;; 2.1
;; Given a string, reverse each word in the sentence "Welcome to this
;; Javascript Guide!" should be become "emocleW ot siht tpircsavaJ
;; !ediuG"

;; (reverse-sentence "Welcome to this Javascript Guide!")

(defun space-delimiter-p (char)
  (char= char #\Space))

(defun split-string (string &optional (delimiterp #'space-delimiter-p))
  (loop :for beg = (position-if-not delimiterp string)
     :then (position-if-not delimiterp string :start (1+ end))
     :for end = (and beg (position-if delimiterp string :start beg))
     :when beg :collect (subseq string beg end)
     :while end))

(defun reverse-sentence (sentence)
  (format nil "~{~A~^ ~}"
          (mapcar #'reverse (split-string sentence))))

;;; 2.2
;; Given two strings, return true if they are anagrams of one another
;; "Mary" is an anagram of "Army"

;; (anagramp "Mary" "Army")  ;=> t

(defun anagramp (string-1 string-2)
  (string= (sort (string-downcase string-1) #'char<)
           (sort (string-downcase string-2) #'char<)))

;;; 2.3
;; Check if a given string is a palindrome "racecar" is a
;; palindrome. "race car" should also be considered a palindrome. Case
;; sensitivity should be taken into account

;; (palindromep "racecar")  ;=> t
;; (palindromep "race car")  ;=> t

(defun palindromep (string)
  (let ((non-space (remove-if #'space-delimiter-p string)))
    (string= non-space (reverse non-space))))

;;; 3.1
;; Don't get this??? Why two stacks?

;;; 3.2
;; Create a function that will evaluate if a given expression has
;; balanced parentheses -- Using stacks In this example, we will only
;; consider "{}" as valid parentheses {}{} would be considered
;; balancing. {{{}} is not balanced

(defvar *expression* "{{}}{}{}")
(defvar *false-expression* "{}{{}")

;; (balanced-paren-p *expression*)  ;=> t
;; (balanced-paren-p *false-expression*)  ;=> nil
;; (balanced-paren-p "")  ;=> t

(defun balanced-paren-p (expression)
  (let ((stack '()))
    (loop for paren across expression
       when (char= paren #\{)
       do (push paren stack)
       when (char= paren #\})
       do (pop stack))
    (zerop (length stack))))

;;; 4.1
;; Write a recursive function that returns the binary string of a given
;; decimal number Given 4 as the decimal input, the function should
;; return 100

;; (decimal->binary 3)                   ;=> 11
;; (decimal->binary 8)                   ;=>1000
;; (decimal->binary 1000)                ;=>1111101000

;;; cheapskate common lisp version
(defun decimal->binary (number)
  (check-type number integer)
  (format nil "~B" number))

;; (recursive-decimal->binary 3)                   ;=> 11
;; (recursive-decimal->binary 8)                   ;=>1000
;; (recursive-decimal->binary 1000)                ;=>1111101000

(defun recursive-decimal->binary (number)
  (labels ((recu (rest acc)
             (if (zerop rest)
                 acc
                 (recu (ash rest -1)
                       (cons (logand rest 1) acc)))))
    (format nil "~{~A~}" (recu number nil))))
