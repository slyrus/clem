
(in-package :clem)

(defmacro def-matrix-hprod (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(make-intern (concatenate 'string "mat-hprod-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc)
	 (destructuring-bind (mr mc) (dim m)
	   (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
	     (with-matrix-vals (m ,element-type-1 a)
	       (with-matrix-vals (n ,element-type-2 b)
		 (with-matrix-vals (p ,accumulator-element-type c)
                   (loop for i fixnum from startr to endr
                      do (loop for j fixnum from startc to endc
                            do (setf (aref c i j)
			             (* (aref a i j) (aref b i j))))))))
	     p)))
       
       (defmethod ,(make-intern (concatenate 'string "mat-hprod" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (mat-hprod-range m n 0 (1- mr) 0 (1- mc)))))))

(defmacro def-matrix-hprod! (type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod ,(make-intern (concatenate 'string "mat-hprod-range!" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc)
	 (with-matrix-vals (m ,element-type-1 a)
	   (with-matrix-vals (n ,element-type-2 b)
	     (loop for i fixnum from startr to endr
                      do (loop for j fixnum from startc to endc
                            do (setf (aref a i j)
		                     (* (aref a i j) (aref b i j))))))
	   m))

       (defmethod ,(make-intern (concatenate 'string "mat-hprod!" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (mat-hprod-range! m n 0 (1- mr) 0 (1- mc)))))))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-hprod ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-hprod! ,type-1 ,type-2 ,type-3 :suffix ,suffix))))

  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix ub8-matrix double-float-matrix)
  (frob double-float-matrix ub16-matrix double-float-matrix)
  (frob double-float-matrix ub32-matrix double-float-matrix)
  (frob double-float-matrix sb8-matrix double-float-matrix)
  (frob double-float-matrix sb16-matrix double-float-matrix)
  (frob double-float-matrix sb32-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)

  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob single-float-matrix ub8-matrix single-float-matrix)
  (frob single-float-matrix ub16-matrix single-float-matrix)
  (frob single-float-matrix ub32-matrix single-float-matrix)
  (frob single-float-matrix sb8-matrix single-float-matrix)
  (frob single-float-matrix sb16-matrix single-float-matrix)
  (frob single-float-matrix sb32-matrix single-float-matrix)
  (frob single-float-matrix fixnum-matrix single-float-matrix)
  (frob single-float-matrix bit-matrix single-float-matrix)
  
  (frob ub8-matrix ub8-matrix ub8-matrix)
  (frob ub8-matrix bit-matrix ub8-matrix)

  (frob ub16-matrix ub16-matrix ub16-matrix)
  (frob ub16-matrix ub8-matrix ub16-matrix)
  (frob ub16-matrix bit-matrix ub16-matrix)

  (frob ub32-matrix ub32-matrix ub32-matrix)
  (frob ub32-matrix ub16-matrix ub32-matrix)
  (frob ub32-matrix ub8-matrix ub32-matrix)
  (frob ub32-matrix bit-matrix ub32-matrix)

  (frob sb8-matrix sb8-matrix sb8-matrix)
  (frob sb8-matrix bit-matrix sb8-matrix)

  (frob sb16-matrix sb16-matrix sb16-matrix)
  (frob sb16-matrix sb8-matrix sb16-matrix)
  (frob sb16-matrix bit-matrix sb16-matrix)

  (frob sb32-matrix sb32-matrix sb32-matrix)
  (frob sb32-matrix sb16-matrix sb32-matrix)
  (frob sb32-matrix sb8-matrix sb32-matrix)
  (frob sb32-matrix bit-matrix sb32-matrix)

  (frob fixnum-matrix fixnum-matrix fixnum-matrix)
  (frob fixnum-matrix bit-matrix fixnum-matrix)

  (frob bit-matrix bit-matrix bit-matrix))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-hprod ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-hprod! ,type-1 ,type-2 ,type-3 :suffix ,suffix))))

  (frob real-matrix double-float-matrix real-matrix)
  (frob real-matrix single-float-matrix real-matrix)
  (frob real-matrix ub8-matrix real-matrix)
  (frob real-matrix ub16-matrix real-matrix)
  (frob real-matrix ub32-matrix real-matrix)
  (frob real-matrix sb8-matrix real-matrix)
  (frob real-matrix sb16-matrix real-matrix)
  (frob real-matrix sb32-matrix real-matrix)
  (frob real-matrix real-matrix real-matrix)
  (frob real-matrix number-matrix real-matrix)
  (frob real-matrix bit-matrix real-matrix)

  (frob complex-matrix double-float-matrix complex-matrix)
  (frob complex-matrix single-float-matrix complex-matrix)
  (frob complex-matrix ub8-matrix complex-matrix)
  (frob complex-matrix ub16-matrix complex-matrix)
  (frob complex-matrix ub32-matrix complex-matrix)
  (frob complex-matrix sb8-matrix complex-matrix)
  (frob complex-matrix sb16-matrix complex-matrix)
  (frob complex-matrix sb32-matrix complex-matrix)
  (frob complex-matrix real-matrix complex-matrix)
  (frob complex-matrix complex-matrix complex-matrix)
  (frob complex-matrix number-matrix complex-matrix)
  (frob complex-matrix bit-matrix complex-matrix))

