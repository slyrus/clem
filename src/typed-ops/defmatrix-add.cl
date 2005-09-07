;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-add.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

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
  (frob sb32-matrix ub16-matrix sb32-matrix)

  (frob real-matrix real-matrix real-matrix)
  (frob real-matrix double-float-matrix real-matrix)
  (frob real-matrix single-float-matrix real-matrix)
  (frob real-matrix integer-matrix real-matrix)

  (frob integer-matrix integer-matrix integer-matrix)

  (frob complex-matrix complex-matrix complex-matrix)
  (frob complex-matrix integer-matrix complex-matrix)
  (frob complex-matrix real-matrix complex-matrix))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
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


(defmacro def-matrix-add-scalar (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc)
         (declare (type ,type-2 n))
	 (destructuring-bind (mr mc) (dim m)
	   (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
	     (with-matrix-vals (m ,element-type-1 a)
               (with-matrix-vals (p ,accumulator-element-type c)
                 (do ((i startr (1+ i)))
                     ((> i endr))
                   (declare (dynamic-extent i) (type fixnum i))
                   (do ((j startc (1+ j)))
                       ((> j endc))
                     (declare (dynamic-extent j) (type fixnum j))
                     (setf (aref c i j)
                           (+ (aref a i j) n))))))
	     p)))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string "mat-add-range" suffix)) m n 0 (1- mr) 0 (1- mc)))))))
       
(defmacro def-matrix-add-scalar! (type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((element-type-1 (element-type (find-class `,type-1))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add!-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc)
         (declare (type ,type-2 n))
	 (with-matrix-vals (m ,element-type-1 a)
	     (do ((i startr (1+ i)))
		 ((> i endr))
	       (declare (dynamic-extent i) (type fixnum i))
	       (do ((j startc (1+ j)))
		   ((> j endc))
		 (declare (dynamic-extent j) (type fixnum j))
		 (setf (aref a i j)
		       (+ (aref a i j) n)))))
         m)
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add!" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string "mat-add!-range" suffix)) m n 0 (1- mr) 0 (1- mc))))
       
       )))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-add-scalar ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-add-scalar! ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  (frob double-float-matrix double-float double-float-matrix))

