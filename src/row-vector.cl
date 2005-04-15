;;;
;;; file: row-vector.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defclass row-vector (matrix) ())

(defmethod allocate-matrix-vals ((object row-vector) &key rows cols adjustable initial-element element-type)
  (declare (ignore rows))
  (setf (slot-value object 'm)
	(make-array (list cols)
		    :adjustable adjustable
		    :initial-element initial-element
		    :element-type element-type)))

(defmethod array->row-vector ((a array))
  (let ((d (array-dimensions a)))
    (cond ((= (length d) 2)
	   (let* ((cols (second d))
		  (m (make-instance 'row-vector :cols cols)))
	     (dotimes (i cols)
	       (vec-set-val m i (aref a 0 i)))
	     m))
	  ((= (length d) 1)
	   (let ((m (row-vector (first d))))
	     (dotimes (i (first d)) (vec-set-val m i (aref a i)))
	     m)))))

(defmethod vec-dim ((rv row-vector)) (array-dimensions (matrix-vals rv)))
(defmethod vec-val ((rv row-vector) i) (aref (matrix-vals rv) i))
(defmethod vec-set-val ((rv row-vector) i v) (setf (aref (matrix-vals rv) i) v))

(defmethod dim ((rv row-vector)) (append '(1) (vec-dim rv)))
(defmethod rows ((rv row-vector)) 1)
(defmethod cols ((rv row-vector)) (first (array-dimensions (matrix-vals rv))))
(defmethod val ((rv row-vector) i j) (declare (ignore i)) (vec-val rv j))
(defmethod set-val ((rv row-vector) i j v &key (coerce t))
  (declare (ignore i))
  (vec-set-val rv j (if coerce (coerce v (storage-type rv)) v)))

(defmethod transpose ((rv row-vector))
  (let ((c (cols rv)))
    (let ((cv (col-vector c)))
      (dotimes (j c cv)
	(vec-set-val cv j (vec-val rv j))))))
      
(defmethod print-matrix ((rv row-vector))
  (let ((d (vec-dim rv)))
    (format t "~&((")
    (dotimes (i (first d))
      (format t (if (= i 0) "~a" " ~a") (vec-val rv i)))
    (format t "))")))

(defmethod set-row ((m matrix) r (v row-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (cols v)))
    (set-val m r i (vec-val v i))))

(defmethod set-col ((m matrix) r (v row-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (cols v)))
    (set-val m i r (vec-val v i))))

(defmethod get-row-vector ((m matrix) r)
  (row-vector (second (dim m))
	      (get-row-list m r)))

(defmethod zero-row-vector((j fixnum))
  (zero-matrix 1 j))

