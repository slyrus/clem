
(in-package :clem-test)

(defmacro with-benchmark (&body body)
  (let ((start-var (gensym))
        (end-var (gensym)))
    `(let ((,start-var (get-internal-run-time)))
       (values
        (multiple-value-list
         (progn ,@body))
        (let ((,end-var (get-internal-run-time)))
          (coerce (/ (- ,end-var ,start-var) internal-time-units-per-second)
                  'double-float))))))

(defmacro benchmark-time (benchmark-results)
  `(nth-value 1 ,benchmark-results))

(defparameter *matrix-benchmark-times* (make-hash-table))

(defmacro with-matrix-benchmark ((key) &body body)
  (let ((time-sym (gensym))
        (results-sym (gensym)))
    `(multiple-value-bind (,results-sym ,time-sym)
         (with-benchmark
           (progn ,@body))
       (setf (gethash ,key *matrix-benchmark-times*)
             ,time-sym)
       (values-list ,results-sym))))

(defun get-benchmark-time (key)
  (gethash key *matrix-benchmark-times*))

(defun list-benchmarks ()
  (let ((l))
    (maphash #'(lambda (k v)
                 (push (cons k v) l))
             *matrix-benchmark-times*)
    (nreverse l)))

(defun clear-benchmarks ()
  (clrhash *matrix-benchmark-times*))

;;; simple double float benchmarks
(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0))
      (n (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 2d0)))
  (with-matrix-benchmark (:add-1024-1024/double-float)
    (m+ m n)))

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 2d0))
      (n (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0)))
  (with-matrix-benchmark (:subtr-1024-1024/double-float)
    (m- m n)))

(let ((m (make-instance 'double-float-matrix :dimensions '(512 512) :initial-element 1d0))
      (n (make-instance 'double-float-matrix :dimensions '(512 512) :initial-element 2d0)))
  (with-matrix-benchmark (:mult-512-512/double-float)
    (m* m n)))

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0))
      (n (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 2d0)))
  (with-matrix-benchmark (:mult-1024-1024/double-float)
    (m* m n)))

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0))
      (n (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 2d0)))
  (with-matrix-benchmark (:hprod-1024-1024/double-float)
    (m.* m n)))

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0)))
  (with-matrix-benchmark (:scale-1024-1024/double-float)
    (mat-scale m 2.0d0)))

;;; simple single float benchmarks
(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0))
      (n (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 2s0)))
  (with-matrix-benchmark (:add-1024-1024/single-float)
    (m+ m n)))

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 2s0))
      (n (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0)))
  (with-matrix-benchmark (:subtr-1024-1024/single-float)
    (m- m n)))

(let ((m (make-instance 'single-float-matrix :dimensions '(512 512) :initial-element 1s0))
      (n (make-instance 'single-float-matrix :dimensions '(512 512) :initial-element 2s0)))
  (with-matrix-benchmark (:mult-512-512/single-float)
    (m* m n)))

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0))
      (n (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 2s0)))
  (with-matrix-benchmark (:mult-1024-1024/single-float)
    (m* m n)))

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0))
      (n (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 2s0)))
  (with-matrix-benchmark (:hprod-1024-1024/single-float)
    (m.* m n)))

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0)))
  (with-matrix-benchmark (:scale-1024-1024/single-float)
    (mat-scale m 2.0s0)))
