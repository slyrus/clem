;;;
;;; file: row-vector.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defclass row-vector (base-vector) ())

(defmethod allocate-matrix-vals ((object row-vector) &key rows cols adjustable initial-element)
  (declare (ignore rows))
  (setf (slot-value object 'm)
	(make-array (list cols)
		    :adjustable adjustable
		    :initial-element initial-element
		    :element-type (element-type (class-of object)))))

(defgeneric array->row-vector (a))
(defmethod array->row-vector ((a array))
  (let ((d (array-dimensions a)))
    (cond ((= (length d) 2)
	   (let* ((cols (second d))
		  (m (make-instance 'row-vector :cols cols)))
	     (dotimes (i cols)
	       (vec-set-val m i (aref a 0 i)))
	     m))
	  ((= (length d) 1)
	   (let ((m (make-instance 'row-vector :cols (first d))))
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
  (vec-set-val rv j (if coerce (coerce v (element-type (class-of rv))) v)))

(defmethod transpose ((rv row-vector))
  (let ((c (cols rv)))
    (let ((cv (make-instance 'col-vector :rows c)))
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

(defgeneric get-row-vector (m r))
(defmethod get-row-vector ((m matrix) r)
  (let ((rv (make-instance 'row-vector :cols (second (dim m)))))
    (dotimes (i (second (dim m)))
      (vec-set-val rv i (val m r i)))))

(defgeneric zero-row-vector (j))
(defmethod zero-row-vector((j fixnum))
  (zero-matrix 1 j))

