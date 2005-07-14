;;;
;;; file: defmatrix-sum.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro %%sum-range (m startr endr startc endc element-type accumulator-type)
  `(let ((acc (coerce 0 ',accumulator-type))
         (a (matrix-vals ,m)))
     (declare (type ,accumulator-type acc)
              (type (simple-array ,element-type (* *)) a))
     (do ((i ,startr (1+ i)))
         ((> i ,endr))
       (declare (dynamic-extent i) (type fixnum i))
       (do ((j ,startc (1+ j)))
           ((> j ,endc))
         (declare (dynamic-extent j) (type fixnum j))
         (setf acc (+ acc (aref a i j)))))
     acc))

(macrolet
    ((frob-sum-range (matrix-type accumulator-type)
       (let ((element-type (element-type (find-class matrix-type))))
	 `(defmethod sum-range ((m ,matrix-type)
                                (startr fixnum) (endr fixnum)
                                (startc fixnum) (endc fixnum))
            (%%sum-range m startr endr startc endc
                         ,element-type ,accumulator-type)))))
  (frob-sum-range double-float-matrix double-float)
  (frob-sum-range single-float-matrix single-float)

  (frob-sum-range ub8-matrix (unsigned-byte 32))
  (frob-sum-range ub16-matrix (unsigned-byte 32))
  (frob-sum-range ub32-matrix (unsigned-byte 32))

  (frob-sum-range sb8-matrix (signed-byte 32))
  (frob-sum-range sb16-matrix (signed-byte 32))
  (frob-sum-range sb32-matrix (signed-byte 32))

  (frob-sum-range fixnum-matrix (signed-byte 32))
  (frob-sum-range bit-matrix (signed-byte 32)))


(macrolet
    ((frob-sum-square-range (matrix-type accumulator-type)
       (let ((element-type (element-type (find-class matrix-type))))
	 `(defmethod sum-square-range ((m ,matrix-type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
	    (let ((acc (coerce 0 ',accumulator-type))
		  (a (matrix-vals m)))
	      (declare (type ,accumulator-type acc)
		       (type (simple-array ,element-type (* *)) a))
	      (do ((i startr (1+ i)))
		  ((> i endr))
		(declare (dynamic-extent i) (type fixnum i))
		(do ((j startc (1+ j)))
		    ((> j endc))
		  (declare (dynamic-extent j) (type fixnum j))
		  (incf acc (* (aref a i j) (aref a i j)))))
	      acc)))))

  (frob-sum-square-range double-float-matrix double-float)
  (frob-sum-square-range single-float-matrix single-float)

  (frob-sum-square-range ub8-matrix (unsigned-byte 32))
  (frob-sum-square-range ub16-matrix (unsigned-byte 32))
  (frob-sum-square-range ub32-matrix (unsigned-byte 32))

  (frob-sum-square-range sb8-matrix (signed-byte 32))
  (frob-sum-square-range sb16-matrix (signed-byte 32))
  (frob-sum-square-range sb32-matrix (signed-byte 32))

  (frob-sum-square-range fixnum-matrix (signed-byte 32))
  (frob-sum-square-range bit-matrix (signed-byte 32)))

