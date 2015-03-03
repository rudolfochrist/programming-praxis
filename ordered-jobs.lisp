(ql:quickload "ptester")
(use-package :ptester)

(defun schedule (graph)
  (when graph
    (reduce #'(lambda (result current)
                (if (listp current)
                    (let ((dep (string-downcase (symbol-name (car current))))
                          (pre-condition (string-downcase (symbol-name (cadr current)))))
                      (when (string= dep pre-condition)
                        (error "Jobs cannot depend on themselves"))
                      (cond
                        ((and (not (search dep result)) (not (search pre-condition result)))
                         (concatenate 'string pre-condition result dep))
                        ((not (search dep result))
                         (concatenate 'string result dep))
                        ((not (search pre-condition result))
                         (concatenate 'string pre-condition result))
                        (t
                         (error "Circular dependencies detected"))))
                    (if (not (find (symbol-name current) result :test #'string-equal))
                      (concatenate 'string result (string-downcase (symbol-name current)))
                      result)))
            graph :initial-value "")))

;;; tests

(defmacro contains (element seq)
  `(not (null (find ,element ,seq :test #'string=))))

(defmacro scheduled-before (elem1 elem2 seq)
  `(when (and (contains ,elem1 ,seq)
              (contains ,elem2 ,seq))
     (< (position ,elem1 ,seq :test #'string=)
        (position ,elem2 ,seq :test #'string=))))

(test nil (schedule nil))
(test "a" (schedule '(a)) :test #'string=)
(test "abc" (schedule '(a b c)) :test #'string=)

(let ((jobs (schedule '(a (b c) c))))
  (print jobs)
  (test t (contains "a" jobs))
  (test t (contains "b" jobs))
  (test t (contains "c" jobs))
  (test t (scheduled-before "c" "b" jobs)))

(let ((jobs (schedule '(a (b c) (c f) (d a) (e b) f))))
  (print jobs)
  (test t (contains "a" jobs))
  (test t (contains "b" jobs))
  (test t (contains "c" jobs))
  (test t (contains "d" jobs))
  (test t (contains "e" jobs))
  (test t (contains "f" jobs))
  (test t (scheduled-before "f" "c" jobs))
  (test t (scheduled-before "c" "b" jobs))
  (test t (scheduled-before "b" "e" jobs))
  (test t (scheduled-before "a" "d" jobs)))

(test-error (schedule '(a b (c c))))
(test-error (schedule '( a (b c) (c f) (d a) e (f b))))
