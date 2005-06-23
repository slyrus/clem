
(in-package :clem)
  
(defmacro def-matrix-mult (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))

    `(progn
       (defmethod ,(make-intern (concatenate 'string "mat-mult3-range" suffix))
	   ((m ,type-1) (n ,type-2) (p ,accumulator-type)
	    mstartr mendr mstartc mendc
	    nstartr nendr nstartc nendc)
	 (let ((mr (- mendr mstartr))
	       (mc (- mendc mstartc))
	       (nr (- nendr nstartr))
	       (nc (- nendc nstartc)))
	   (declare (type fixnum mr mc nr nc)
		    (optimize (speed 3) (safety 0)))
	   (when (eql mc nr)
	     (let ((a (clem::matrix-vals m))
		   (b (clem::matrix-vals n))
		   (c (clem::matrix-vals p))
		   (atemp (coerce 0 ',accumulator-element-type)))
	       (declare (type (simple-array ,element-type-1 (* *)) a)
			(type (simple-array ,element-type-2 (* *)) b)
			(type (simple-array ,accumulator-element-type (* *)) c)
			(type ,accumulator-element-type atemp))
	       (do ((k 0 (1+ k)))
		   ((> k mc))
		 (declare (type fixnum k))
		 (do ((i 0 (1+ i)))
		     ((> i mr))
		   (declare (type fixnum i))
		   (setf atemp (aref a i k))
		   (do ((j 0 (1+ j)))
		       ((> j nc))
		     (declare (type fixnum j))
		     (incf (aref c i j) (* atemp (aref b k j)))))))))
	 p)
       
       (defmethod ,(make-intern (concatenate 'string "mat-mult-range" suffix))
	   ((m ,type-1) (n ,type-2)
	   mstartr mendr mstartc mendc
	   nstartr nendr nstartc nendc)
	 (let ((mr (- mendr mstartr))
	       (nc (- nendc nstartc)))
	   (let ((p (make-instance ',accumulator-type :rows (1+ mr) :cols (1+ nc))))
	     (,(make-intern (concatenate 'string "mat-mult3-range" suffix))
	       m n p
	       mstartr mendr mstartc mendc
	       nstartr nendr nstartc nendc))))
       
       (defmethod ,(make-intern (concatenate 'string "mat-mult" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (destructuring-bind (nr nc) (dim n)
	     (mat-mult-range m n
			     0 (1- mr) 0 (1- mc)
			     0 (1- nr) 0 (1- nc)))))
       
       (defmethod ,(make-intern (concatenate 'string "mat-mult3" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (destructuring-bind (nr nc) (dim n)
	     (let ((p (make-instance ',accumulator-type :rows mr :cols nc)))
	       (,(make-intern (concatenate 'string "mat-mult3-range" suffix))
		 (mat-mult3-range m n p
				  0 (1- mr) 0 (1- mc)
				  0 (1- nr) 0 (1- nc))))))))))
    
       
(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(def-matrix-mult ,type-1 ,type-2 ,type-3 :suffix ,suffix)))
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix single-float-matrix))

