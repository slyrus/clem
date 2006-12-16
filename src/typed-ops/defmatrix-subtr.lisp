;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-subtr.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defgeneric %get-subtr-matrix-class (a b))
(defgeneric mat-subtr-range3 (m n p startr endr startc endc))

(defmacro def-matrix-subtr (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn

       (defmethod %get-subtr-matrix-class ((a ,type-1) (b ,type-2))
         ',accumulator-type)
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-subtr-range3" suffix))
	   ((m ,type-1) (n ,type-2) (p ,accumulator-type) startr endr startc endc)
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
                         (- (aref a i j) (aref b i j))))))))
         p))))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-subtr ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  
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
  (frob ub8-matrix ub8-matrix sb16-matrix)
  (frob ub16-matrix ub16-matrix ub16-matrix)
  (frob ub16-matrix ub16-matrix sb32-matrix)
  (frob ub32-matrix ub32-matrix ub32-matrix)
  (frob ub32-matrix ub32-matrix sb32-matrix)

  (frob ub8-matrix bit-matrix ub8-matrix)
  (frob ub16-matrix bit-matrix ub16-matrix)
  (frob ub32-matrix bit-matrix ub32-matrix)

  (frob sb8-matrix bit-matrix sb8-matrix)
  (frob sb8-matrix bit-matrix sb16-matrix)
  (frob sb16-matrix bit-matrix sb16-matrix)
  (frob sb32-matrix bit-matrix sb32-matrix)
  
  (frob sb32-matrix ub8-matrix sb32-matrix)
  (frob sb32-matrix ub16-matrix sb32-matrix)
  
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
  (frob bit-matrix double-float-matrix double-float-matrix)
  (frob bit-matrix single-float-matrix single-float-matrix)
  (frob bit-matrix bit-matrix bit-matrix))

(defgeneric mat-subtr-range (m n start endr startc endc &key matrix-class))

(defmethod mat-subtr-range ((m typed-mixin) (n typed-mixin) startr endr startc endc &key (matrix-class (%get-subtr-matrix-class m n)))
  (destructuring-bind (mr mc) (dim m)
    (let ((p (make-instance matrix-class :rows mr :cols mc)))
      (mat-subtr-range3 m n p startr endr startc endc))))

(defmethod mat-subtr :around ((m matrix) (n matrix) &key (matrix-class (%get-subtr-matrix-class m n)))
  (if (compute-applicable-methods #'mat-subtr-range (list m n 0 0 0 0))
      (destructuring-bind (mr mc) (dim m)
        (mat-subtr-range m n 0 (1- mr) 0 (1- mc) :matrix-class matrix-class))
      (call-next-method)))

(defgeneric mat-subtr!-range ( m n startr endr startc endc))
(defgeneric mat-subtr! (m n))

(defmacro def-matrix-subtr! (type-1 type-2 &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-subtr!-range" suffix))
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
		       (- (aref a i j) (aref b i j))))))
	   m))

       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-subtr!" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (mat-subtr!-range m n 0 (1- mr) 0 (1- mc)))))))

(macrolet ((frob (type-1 type-2 &key suffix)
	     `(progn
		(def-matrix-subtr! ,type-1 ,type-2 :suffix ,suffix))))
  
  (frob double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix)
  (frob double-float-matrix ub8-matrix)
  (frob double-float-matrix ub16-matrix)
  (frob double-float-matrix ub32-matrix)
  (frob double-float-matrix sb8-matrix)
  (frob double-float-matrix sb16-matrix)
  (frob double-float-matrix sb32-matrix)
  (frob double-float-matrix bit-matrix)
  (frob double-float-matrix fixnum-matrix)

  (frob single-float-matrix single-float-matrix)
  (frob single-float-matrix ub8-matrix)
  (frob single-float-matrix ub16-matrix)
  (frob single-float-matrix ub32-matrix)
  (frob single-float-matrix sb8-matrix)
  (frob single-float-matrix sb16-matrix)
  (frob single-float-matrix sb32-matrix)
  (frob single-float-matrix bit-matrix)
  (frob single-float-matrix fixnum-matrix)

  (frob ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix)

  (frob ub8-matrix bit-matrix)
  (frob ub16-matrix bit-matrix)
  (frob ub32-matrix bit-matrix)

  (frob sb8-matrix bit-matrix)
  (frob sb16-matrix bit-matrix)
  (frob sb32-matrix bit-matrix)
  
  (frob sb32-matrix ub8-matrix)
  (frob sb32-matrix ub16-matrix))

