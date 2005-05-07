
(in-package :clem)

(defclass typed-matrix (matrix)
  ((specialzied-array :allocation :class :accessor specialized-array-p :initform nil))
  (:metaclass standard-matrix-class))

(defmethod set-val-fit ((m typed-matrix) i j v &key (truncate nil))
  (set-val m i j (if truncate (truncate v) v)))


(defmethod map-matrix-fit (f (a typed-matrix))
  (destructuring-bind (m n) (dim a)
    (dotimes (i m)
      (dotimes (j n)
	(set-val-fit a i j (funcall f a i j)))))
  a)


