
(in-package :clem-benchmark)

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

(defparameter *matrix-benchmark-times* (make-hash-table :test 'eql))

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

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/double-float)
    (mat-scale m 2.0d0 :in-place t)))

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

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/single-float)
    (mat-scale m 2.0s0 :in-place t)))

;;; simple sb-8 benchmarks
(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/sb8)
    (m+ m n)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 2))
      (n (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:subtr-1024-1024/sb8)
    (m- m n)))

(let ((m (make-instance 'sb8-matrix :dimensions '(512 512) :initial-element 1))
      (n (make-instance 'sb8-matrix :dimensions '(512 512) :initial-element 2)))
  (with-matrix-benchmark (:mult-512-512/sb8)
    (m* m n)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:mult-1024-1024/sb8)
    (m* m n)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:hprod-1024-1024/sb8)
    (m.* m n)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/sb8)
    (mat-scale m 2)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/sb8)
    (mat-scale m 2 :in-place t)))


;;; simple ub-8 benchmarks
(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/ub8)
    (m+ m n)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 2))
      (n (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:subtr-1024-1024/ub8)
    (m- m n)))

(let ((m (make-instance 'ub8-matrix :dimensions '(512 512) :initial-element 1))
      (n (make-instance 'ub8-matrix :dimensions '(512 512) :initial-element 2)))
  (with-matrix-benchmark (:mult-512-512/ub8)
    (m* m n)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:mult-1024-1024/ub8)
    (m* m n)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:hprod-1024-1024/ub8)
    (m.* m n)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/ub8)
    (mat-scale m 2)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/ub8)
    (clem::mat-scale-2 m m 2)))
