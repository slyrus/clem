;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-move.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro def-matrix-move (type-1 type-2)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(min (minval (find-class `,type-2)))
	(max (maxval (find-class `,type-2))))
    `(progn
       (defmethod matrix-move-range ((m ,type-1) (n ,type-2) startr endr startc endc)
	 (with-matrix-vals (m ,element-type-1 a)
	   (with-matrix-vals (n ,element-type-2 b)
	     (do ((i startr (1+ i)))
		 ((> i endr))
	       (declare (dynamic-extent i) (type fixnum i))
	       (do ((j startc (1+ j)))
		   ((> j endc))
		 (declare (dynamic-extent j) (type fixnum j))
		 (setf (aref b i j)
		       (maybe-truncate
			(aref a i j)
			,element-type-1 ,element-type-2))))))
	 n)
       (defmethod matrix-move-range-constrain ((m ,type-1) (n ,type-2) startr endr startc endc)
	 (with-matrix-vals (m ,element-type-1 a)
	   (with-matrix-vals (n ,element-type-2 b)
	     (do ((i startr (1+ i)))
		 ((> i endr))
	       (declare (dynamic-extent i) (type fixnum i))
	       (do ((j startc (1+ j)))
		   ((> j endc))
		 (declare (dynamic-extent j) (type fixnum j))
		 (setf (aref b i j) ,(if (eql element-type-1 element-type-2)
					 `(constrain ,min (aref a i j) ,max)
					 `(maybe-truncate (constrain ,min (aref a i j) ,max)
							  ,element-type-1 ,element-type-2)))))))
	 n)
       (defmethod matrix-move ((m ,type-1) (n ,type-2) &key constrain)
	 (destructuring-bind (mr mc) (dim m)
	   (cond (constrain
		  (matrix-move-range-constrain m n 0 (1- mr) 0 (1- mc)))
		 (t
		  (matrix-move-range m n 0 (1- mr) 0 (1- mc)))))))))

(defmacro def-matrix-add (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc)
	 (destructuring-bind (mr mc) (dim m)
	   (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
	     (with-matrix-vals (m ,element-type-1 a)
	       (with-matrix-vals (n ,element-type-2 b)
		 (with-matrix-vals (p ,accumulator-element-type c)
		   (do ((i startr (1+ i)))
		       ((> i endr))
		     (declare (dynamic-extent i) (type fixnum i))
		     (do ((j startc (1+ j)))
			 ((> j endc))
		       (declare (dynamic-extent j) (type fixnum j))
		       (setf (aref c i j)
			     (+ (aref a i j) (aref b i j))))))))
	     p)))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string "mat-add-range" suffix)) m n 0 (1- mr) 0 (1- mc)))))))
       
(defmacro def-matrix-add! (type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add!-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc)
	 (with-matrix-vals (m ,element-type-1 a)
	   (with-matrix-vals (n ,element-type-2 b)
	     (do ((i startr (1+ i)))
		 ((> i endr))
	       (declare (dynamic-extent i) (type fixnum i))
	       (do ((j startc (1+ j)))
		   ((> j endc))
		 (declare (dynamic-extent j) (type fixnum j))
		 (setf (aref a i j)
		       (+ (aref a i j) (aref b i j))))))
	   m))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add!" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string "mat-add!-range" suffix)) m n 0 (1- mr) 0 (1- mc))))
       
       )))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-move-element ,type-1 ,type-2)
		(def-matrix-move ,type-1 ,type-2)
		(def-matrix-add ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-add! ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix ub8-matrix double-float-matrix)
  (frob double-float-matrix ub16-matrix double-float-matrix)
  (frob double-float-matrix ub32-matrix double-float-matrix)
  (frob double-float-matrix sb8-matrix double-float-matrix)
  (frob double-float-matrix sb16-matrix double-float-matrix)
  (frob double-float-matrix sb32-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)

  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob single-float-matrix ub8-matrix single-float-matrix)
  (frob single-float-matrix ub16-matrix single-float-matrix)
  (frob single-float-matrix ub32-matrix single-float-matrix)
  (frob single-float-matrix sb8-matrix single-float-matrix)
  (frob single-float-matrix sb16-matrix single-float-matrix)
  (frob single-float-matrix sb32-matrix single-float-matrix)
  (frob single-float-matrix bit-matrix single-float-matrix)
  (frob single-float-matrix fixnum-matrix single-float-matrix)

  (frob ub8-matrix ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix ub32-matrix)

  (frob ub8-matrix bit-matrix ub8-matrix)
  (frob ub16-matrix bit-matrix ub16-matrix)
  (frob ub32-matrix bit-matrix ub32-matrix)

  (frob sb8-matrix bit-matrix sb8-matrix)
  (frob sb16-matrix bit-matrix sb16-matrix)
  (frob sb32-matrix bit-matrix sb32-matrix)
  
  (frob sb32-matrix ub8-matrix sb32-matrix)
  (frob sb32-matrix ub16-matrix sb32-matrix))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-move-element ,type-1 ,type-2)
		(def-matrix-move ,type-1 ,type-2)
		(def-matrix-add ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  (frob single-float-matrix double-float-matrix double-float-matrix)

  (frob ub8-matrix double-float-matrix double-float-matrix)
  (frob ub8-matrix single-float-matrix single-float-matrix)

  (frob ub16-matrix double-float-matrix double-float-matrix)
  (frob ub16-matrix single-float-matrix single-float-matrix)

  (frob ub32-matrix double-float-matrix double-float-matrix)
  (frob ub32-matrix single-float-matrix single-float-matrix)

  (frob sb8-matrix double-float-matrix double-float-matrix)
  (frob sb8-matrix single-float-matrix single-float-matrix)

  (frob sb16-matrix double-float-matrix double-float-matrix)
  (frob sb16-matrix single-float-matrix single-float-matrix)

  (frob sb32-matrix double-float-matrix double-float-matrix)
  (frob sb32-matrix single-float-matrix single-float-matrix)

  (frob fixnum-matrix double-float-matrix double-float-matrix)
  (frob fixnum-matrix single-float-matrix single-float-matrix)

  (frob bit-matrix double-float-matrix double-float-matrix)
  (frob bit-matrix single-float-matrix single-float-matrix))

