
(in-package :clem)

(defun copy-to-matrix-type (m mtype &key (constrain nil))
  (let ((b (make-instance mtype :rows (rows m) :cols (cols m))))
    (matrix-move m b :constrain constrain)
    b))

(defgeneric copy-to-ub8-matrix (m &key constrain))
(defmethod copy-to-ub8-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'ub8-matrix :constrain constrain))

(defgeneric copy-to-double-float-matrix (m &key constrain))
(defmethod copy-to-double-float-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'double-float-matrix :constrain constrain))

(defgeneric copy-to-single-float-matrix (m &key constrain))
(defmethod copy-to-single-float-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'single-float-matrix :constrain constrain))

(defgeneric copy-to-fixnum-matrix (m &key constrain))
(defmethod copy-to-fixnum-matrix ((m matrix) &key (constrain nil))
  (copy-to-matrix-type m 'fixnum-matrix :constrain constrain))

(defgeneric copy-to-bit-matrix (m &key constrain))
(defmethod copy-to-bit-matrix ((m matrix) &key (constrain t))
  (copy-to-matrix-type m 'bit-matrix :constrain constrain))

(defgeneric copy-to-complex-matrix (m &key constrain))
(defmethod copy-to-complex-matrix ((m matrix) &key (constrain t))
  (copy-to-matrix-type m 'complex-matrix :constrain constrain))
