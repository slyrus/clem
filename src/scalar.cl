
(in-package :clem)

(defclass scalar (matrix) ())

(defun scalar (val &key (matrix-class 'scalar))
  (let ((m (make-instance matrix-class)))
    (scalar-set-val m val)
    m))

(defmethod scalar-val ((s scalar)) (matrix-vals s))
(defmethod scalar-set-val ((s scalar) v) (setf (matrix-vals s) v))

(defmethod dim ((s scalar)) '(1 1))
(defmethod val ((s scalar) i j) (declare (ignore i) (ignore j)) (scalar-val s))
(defmethod set-val ((s scalar) i j v &key (coerce t))
  (declare (ignore i) (ignore j))
  (scalar-set-val s (if coerce (coerce v (element-type (class-of s))) v)))



  
