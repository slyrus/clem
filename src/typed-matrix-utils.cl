
(in-package :clem)

(defun copy-to-matrix-type (m mtype &key (truncate nil))
  (let ((b (make-instance mtype :rows (rows m) :cols (cols m))))
    (mat-copy-into m b :truncate truncate)
    b))

(defmethod copy-to-unsigned-byte-matrix ((m matrix))
  (copy-to-matrix-type m 'unsigned-byte-matrix :truncate t))

(defmethod copy-to-double-float-matrix ((m matrix))
  (copy-to-matrix-type m 'double-float-matrix))

(defmethod copy-to-fixnum-matrix ((m matrix))
  (copy-to-matrix-type m 'fixnum-matrix :truncate t))

