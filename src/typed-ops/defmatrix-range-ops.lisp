
(in-package :clem)

;;; hmmm... maybe we can delte this. I'm tired. check in the morning.
(macrolet
    ((frob-min-range (matrix-type)
       (let ((element-type (element-type (find-class matrix-type))))
	 `(defmethod min-range ((m ,matrix-type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
	    (let ((a (matrix-vals m)))
	      (declare (type (simple-array ,element-type (* *)) a))
	      (let ((acc (aref a 0 0)))
		(declare (type ,element-type acc))
		(do ((i startr (1+ i)))
		    ((> i endr))
		  (declare (dynamic-extent i) (type fixnum i))
		  (do ((j startc (1+ j)))
		      ((> j endc))
		    (declare (dynamic-extent j) (type fixnum j))
		    (when (< (aref a i j) acc)
		      (setf acc (aref a i j)))))
		acc))))))
  (frob-min-range double-float-matrix))
