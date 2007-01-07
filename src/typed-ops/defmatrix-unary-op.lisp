;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-unary-op.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro def-unary-op (name op type-1 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string name "-range" suffix))
	   ((m ,type-1)  startr endr startc endc)
	 (destructuring-bind (mr mc) (dim m)
	   (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
             (with-typed-mref (m ,element-type-1)
               (with-typed-mref (p ,accumulator-element-type)
                 (do ((i startr (1+ i)))
                     ((> i endr))
                   (declare (dynamic-extent i) (type fixnum i))
                   (do ((j startc (1+ j)))
                       ((> j endc))
                     (declare (dynamic-extent j) (type fixnum j))
                     (setf (mref p i j)
                           (,op (mref m i j)))))))
	     p)))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string name suffix))
	   ((m ,type-1))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string name "-range" suffix)) m 0 (1- mr) 0 (1- mc)))))))
       
(defmacro def-unary-op! (name op type-1 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((element-type-1 (element-type (find-class `,type-1))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string name "!-range" suffix))
	   ((m ,type-1) startr endr startc endc)
	 (with-typed-mref (m ,element-type-1)
           (do ((i startr (1+ i)))
               ((> i endr))
             (declare (dynamic-extent i) (type fixnum i))
             (do ((j startc (1+ j)))
                 ((> j endc))
               (declare (dynamic-extent j) (type fixnum j))
               (setf (mref m i j)
                     (,op (aref m i j)))))
	   m))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string name "!" suffix))
	   ((m ,type-1))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string name "!-range" suffix)) m 0 (1- mr) 0 (1- mc)))))))

(macrolet ((frob (name op type-1 type-2 &key suffix)
	     `(progn
		(def-unary-op ,name ,op ,type-1 ,type-2 :suffix ,suffix)
		(def-unary-op! ,name ,op ,type-1 ,type-2 :suffix ,suffix))))
  ;; mlognot
  (frob "mlognot" lognot integer-matrix integer-matrix)
  (frob "mlognot" lognot fixnum-matrix fixnum-matrix)
  (frob "mlognot" lognot sb8-matrix sb8-matrix)
  (frob "mlognot" lognot sb16-matrix sb16-matrix)
  (frob "mlognot" lognot sb32-matrix sb32-matrix))
