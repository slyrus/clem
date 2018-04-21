
(in-package :clem)

(defgeneric mat-scale-fit-range (m q startr endr startc endc))
(defgeneric mat-scale-fit (m q))
(defgeneric mat-scale-fit-range! (m q startr endr startc endc))
(defgeneric mat-scale-fit! (m q))

(defmacro def-matrix-scale-fit (type-1 accumulator-type)
  (let ((element-type-1 (element-type (find-class `,type-1))))
    `(progn
       (defmethod mat-scale-fit-range
	   ((m ,type-1) q startr endr startc endc)
         (let ((qconv (coerce q ',element-type-1)))
           (declare (type ,(upgraded-array-element-type element-type-1) qconv))
           (destructuring-bind (mr mc) (dim m)
             (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
               (with-matrix-vals (m ,element-type-1 a)
                 (loop for i fixnum from startr to endr
                    do (loop for j fixnum from startc to endc
                          do (set-val-fit p i j (* (aref a i j) qconv)))))
               p))))
       
       (defmethod mat-scale-fit
	   ((m ,type-1) q)
	 (destructuring-bind (mr mc) (dim m)
	   (mat-scale-fit-range m q 0 (1- mr) 0 (1- mc)))))))


(defmacro def-matrix-scale-fit! (type-1)
  (let ((element-type-1 (element-type (find-class `,type-1))))
    `(progn
       (defmethod mat-scale-fit-range!
	   ((m ,type-1) q startr endr startc endc)
         (if (subtypep (type-of q) ',element-type-1)
             (let ((qconv (coerce q ',element-type-1)))
               (declare (type ,(upgraded-array-element-type element-type-1) qconv))
               (with-matrix-vals (m ,element-type-1 a)
                 (loop for i fixnum from startr to endr
                    do (loop for j fixnum from startc to endc
                          do (set-val-fit m i j (* (aref a i j) qconv))))))
             (with-matrix-vals (m ,element-type-1 a)
               (loop for i fixnum from startr to endr
                  do (loop for j fixnum from startc to endc
                          do (set-val-fit m i j (* (aref a i j) q))))))
         m)
       
       (defmethod mat-scale-fit!
	   ((m ,type-1) q)
	 (destructuring-bind (mr mc) (dim m)
	   (mat-scale-fit-range! m q 0 (1- mr) 0 (1- mc)))))))

(macrolet ((frob (type-1 type-2)
	     `(progn
                (def-matrix-scale-fit ,type-1 ,type-2)
                (def-matrix-scale-fit! ,type-1))))
  (frob double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix)
  (frob ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix)
  (frob sb8-matrix sb8-matrix)
  (frob sb16-matrix sb16-matrix)
  (frob sb32-matrix sb32-matrix)
  (frob bit-matrix bit-matrix)
  (frob fixnum-matrix fixnum-matrix)
  (frob real-matrix real-matrix)
  (frob integer-matrix integer-matrix)
  (frob complex-matrix complex-matrix)
  (frob t-matrix t-matrix))
