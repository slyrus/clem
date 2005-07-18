
(in-package :clem)

(defclass col-vector (base-vector) ())

(defmethod allocate-matrix-vals ((object col-vector) &key rows cols adjustable initial-element)
  (declare (ignore cols))
  (setf (slot-value object 'm)
	(make-array (list rows)
		    :adjustable adjustable
		    :initial-element initial-element
		    :element-type (element-type (class-of object)))))

(defmethod vec-dim ((cv col-vector)) (array-dimensions (matrix-vals cv)))
(defmethod vec-val ((cv col-vector) i) (aref (matrix-vals cv) i))
(defmethod vec-set-val ((cv col-vector) i v) (setf (aref (matrix-vals cv) i) v))

(defmethod dim ((cv col-vector)) (append (vec-dim cv) '(1)))
(defmethod rows ((cv col-vector)) (first (array-dimensions (matrix-vals cv))))
(defmethod cols ((cv col-vector)) 1)
(defmethod val ((cv col-vector) i j) (declare (ignore j)) (vec-val cv i))
(defmethod set-val ((cv col-vector) i j v &key (coerce t))
  (declare (ignore j))
  (vec-set-val cv i (if coerce (coerce v (element-type (class-of cv))) v)))

(defmethod array->col-vector ((a array))
  (let ((d (array-dimensions a)))
    (cond ((= (length d) 2)
	   (let* ((rows (first d))
		  (m (make-instance 'col-vector :rows rows)))
	     (dotimes (i rows) (vec-set-val m i (aref a i 0)))
	     m))
	  ((= (length d) 1)
	   (let ((m (make-instance 'col-vector :rows (first d))))
	     (dotimes (i (first d)) (vec-set-val m i (aref a i)))
	     m)))))

(defmethod transpose ((cv col-vector))
  (let ((r (rows cv)))
    (let ((rv (make-instance 'row-vector :cols r)))
      (dotimes (j r rv)
	(vec-set-val rv j (vec-val cv j))))))

(defmethod print-matrix ((cv col-vector))
  (let ((d (vec-dim cv)))
    (format t "~&(")
    (dotimes (i (first d))
      (format t (if (= i 0) "(~a)" " (~a)") (vec-val cv i)))
    (format t ")")))

(defmethod set-row ((m matrix) r (v col-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (rows v)))
    (set-val m r i (vec-val v i))))

(defmethod set-col ((m matrix) c (v col-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (first (dim v))))
    (set-val m i c (val v i 0))))

(defgeneric get-row-as-col-vector (m r))
(defmethod get-row-as-col-vector ((m matrix) r)
  (let ((cv (make-instance 'col-vector :rows (second (dim m)))))
    (dotimes (i (second (dim m)))
      (vec-set-val cv i (val m r i)))))
		 
(defgeneric get-col-vector (m r))
(defmethod get-col-vector ((m matrix) r)
  (let ((cv (make-instance 'col-vector :rows (first (dim m)))))
    (dotimes (i (first (dim m)))
      (vec-set-val cv i (val m i r)))))

(defgeneric zero-col-vector (j))
(defmethod zero-col-vector((j fixnum))
  (zero-matrix j 1))
