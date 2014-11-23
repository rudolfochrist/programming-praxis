
;;; see http://programmingpraxis.com/2014/11/14/dawkins-weasel/
(defpackage :dawkins-weasel
  (:use :cl))

(in-package :dawkins-weasel)

(defvar *target* (coerce "METHINKS IT IS LIKE A WEASEL" 'list)
  "This is the target string.")

(defvar *perfect-score* 28
  "The perfert score")

(defvar *chars* (cons #\Space (loop for char from (char-code #\A) to (char-code #\Z)
                                 collect (code-char char)))
  "List of upper case characters from A to Z and one Space")

(defun random-string ()
  "This generates a random string. Note: Although this functions says it generates a random-string, a list of random chars is returned.
For the least surprise, of course."
  (labels ((recu (current)
             (if (eq 28 (length current))
                 current
                 (let ((rand-num (random (length *chars*))))
                   (recu (cons (nth rand-num *chars*) current))))))
    (recu nil)))

(defun mutate (string)
  "Randomly replace every character in the string with a 0.05 probability."
  (mapcar #'(lambda (char)
              (if (< (random 1.0) 0.05)
                  (nth (random (length *chars*)) *chars*)
                  char))
          string))

(defun score (string)
  (apply #'+ (mapcar #'(lambda (char1 char2)
               (if (equal char1 char2)
                   1
                   0))
                     string *target*)))

(defun join (string)
  (reduce #'(lambda (m o)
              (concatenate 'string m (write-to-string o)))
          (cdr string) :start (write-to-string (car string))))

(defun join (char-list)
  (format nil "~{~a~}" char-list))

(defun run (&optional (string (random-string)))
  (format t "Using string: ~{~a~}~%" string)
  (let* ((clones (loop for i upto 100
                    collect string))
         (mutants (mapcar #'mutate clones))
         (scores (mapcar #'score mutants))
         (max-score (apply #'max scores))
         (max-score-position (position max-score scores)))
    (if (= max-score *perfect-score*)
        (format t (join *target*))
        (let ((next (nth max-score-position mutants)))
          (format t "Try mutation ~a with score ~d~%" (join next) max-score)
          (run next)))))
