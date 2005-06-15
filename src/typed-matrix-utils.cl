
(in-package :clem)

(defun copy-to-matrix-type (m mtype &key (constrain nil))
  (let ((b (make-instance mtype :rows (rows m) :cols (cols m))))
    (matrix-move m b :constrain constrain)
    b))

(defmethod copy-to-ub8-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'ub8-matrix :constrain constrain))

(defmethod copy-to-double-float-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'double-float-matrix :constrain constrain))

(defmethod copy-to-single-float-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'single-float-matrix :constrain constrain))

(defmethod copy-to-fixnum-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'fixnum-matrix :constrain constrain))

(defmethod copy-to-bit-matrix ((m matrix) &key (constrain t))
  (copy-to-matrix-type m 'bit-matrix :constrain constrain))
