;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-binary-op.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro def-binary-op (name op type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string name "-range" suffix))
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
			     (,op (aref a i j) (aref b i j))))))))
	     p)))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string name suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string name "-range" suffix)) m n 0 (1- mr) 0 (1- mc)))))))
       
(defmacro def-binary-op! (name op type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string name "!-range" suffix))
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
		       (,op (aref a i j) (aref b i j))))))
	   m))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string name "!" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string name "!-range" suffix)) m n 0 (1- mr) 0 (1- mc))))
       
       )))

(defgeneric mlogand-range (m1 m2 startr endr startc endc))
(defgeneric mlogand (m1 m2))
(defgeneric mlogand!-range (m1 m2 startr endr startc endc))
(defgeneric mlogand! (m1 m2))

(defgeneric mlogior-range (m1 m2 startr endr startc endc))
(defgeneric mlogior (m1 m2))
(defgeneric mlogior!-range (m1 m2 startr endr startc endc))
(defgeneric mlogior! (m1 m2))

(defgeneric mlogxor-range (m1 m2 startr endr startc endc))
(defgeneric mlogxor (m1 m2))
(defgeneric mlogxor!-range (m1 m2 startr endr startc endc))
(defgeneric mlogxor! (m1 m2))

(macrolet ((frob (name op type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-binary-op ,name ,op ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-binary-op! ,name ,op ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  ;; matand
  (frob "mlogand" logand bit-matrix bit-matrix bit-matrix)
  (frob "mlogand" logior ub8-matrix ub8-matrix ub8-matrix)
  (frob "mlogand" logior ub16-matrix ub16-matrix ub16-matrix)
  (frob "mlogand" logior ub32-matrix ub32-matrix ub32-matrix)
  (frob "mlogand" logior integer-matrix integer-matrix integer-matrix)
  
  ;; matior
  (frob "mlogior" logior bit-matrix bit-matrix bit-matrix)
  (frob "mlogior" logior ub8-matrix ub8-matrix ub8-matrix)
  (frob "mlogior" logior ub16-matrix ub16-matrix ub16-matrix)
  (frob "mlogior" logior ub32-matrix ub32-matrix ub32-matrix)
  (frob "mlogior" logior integer-matrix integer-matrix integer-matrix)
  
  ;; matxor
  (frob "mlogxor" logxor bit-matrix bit-matrix bit-matrix)
  (frob "mlogxor" logxor ub8-matrix ub8-matrix ub8-matrix)
  (frob "mlogxor" logxor ub16-matrix ub16-matrix ub16-matrix)
  (frob "mlogxor" logxor ub32-matrix ub32-matrix ub32-matrix)
  (frob "mlogxor" logxor integer-matrix integer-matrix integer-matrix))

