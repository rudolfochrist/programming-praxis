
(in-package :cl-user)
(defpackage :buzz-words
  (:use :cl)
  (:export :buzz))

(in-package :buzz-words)

(defvar *broughton-wordlist* '(("integrated"
                      "total"
                      "systematized"
                      "parallel"
                      "functional"
                      "responsive"
                      "optional"
                      "synchronized"
                      "compatible"
                      "balanced")
                     ("management"
                      "organizational"
                      "monitored"
                      "reciprocal"
                      "digital"
                      "logistical"
                      "transitional"
                      "incremental"
                      "third-generation"
                      "policy")
                     ("options"
                      "flexibility"
                      "capability"
                      "mobility"
                      "programming"
                      "concept"
                      "time-phase"
                      "projection"
                      "hardware"
                      "contingency")))

(defun buzz ()
  (let ((max-random (length (nth 0 *broughton-wordlist*))))
    (format nil "~{~a~^ ~}" (loop for i below 3
                               for rand = (random max-random)
                               collect (nth rand (nth i *broughton-wordlist*))))))
