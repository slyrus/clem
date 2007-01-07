
(in-package :clem)

;;; slow functions

(defmethod min-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((retval (val m startr startc)))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (setf retval (min retval v))))
    retval))

(defmethod max-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (let ((retval (val m startr startc)))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (setf retval (max retval v))))
    retval))

(defmethod min-val ((m matrix))
  (let ((minval (val m 0 0)))
    (let ((d (dim m)))
      (dotimes (i (first d))
	(dotimes (j (second d))
	  (setf minval (min minval (val m i j))))))
    minval))

(defmethod max-val ((m matrix))
  (let ((maxval (val m 0 0)))
    (let ((d (dim m)))
      (dotimes (i (first d))
	(dotimes (j (second d))
	  (setf maxval (max maxval (val m i j))))))
    maxval))

;;; fast functions

(defmacro def-matrix-min-max (type)
  (let ((element-type (element-type (find-class `,type)))
	(accumulator-element-type (element-type (find-class `,type))))
    `(progn
       (defmethod min-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
         (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-element-type)))
           (declare (type ,accumulator-element-type acc))
           (with-map-range m ,element-type startr endr startc endc (a i j)
             (when (< (aref a i j) acc)
               (setf acc (aref a i j))))
           acc))
       
       (defmethod max-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
         (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-element-type)))
           (declare (type ,accumulator-element-type acc))
           (with-map-range m ,element-type startr endr startc endc (a i j)
             (when (> (aref a i j) acc)
               (setf acc (aref a i j))))
           acc)))))

(macrolet ((frob (type)
	     `(def-matrix-min-max ,type)))
  (frob double-float-matrix)
  (frob single-float-matrix)
  (frob ub8-matrix)
  (frob ub16-matrix)
  (frob ub32-matrix)
  (frob sb8-matrix)
  (frob sb16-matrix)
  (frob sb32-matrix)
  (frob fixnum-matrix)
  (frob bit-matrix)
  (frob integer-matrix)
  (frob real-matrix))

