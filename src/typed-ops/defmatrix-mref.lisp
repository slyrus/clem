
(in-package :clem)

#+nil (declaim (inline mref))
  
(defmacro def-matrix-mref (type)
  (let ((element-type (element-type (find-class `,type)))
        (fast-mref-symbol (ch-util:make-intern
                           (concatenate 'string (symbol-name `,type) "-mref"))))
    `(progn
       (declaim (ftype (function (,type fixnum fixnum) ,element-type)
                       ,fast-mref-symbol))
       (declaim (inline ,fast-mref-symbol))
       (defun ,fast-mref-symbol (m row col)
	 (with-typed-matrix-vals (m ,element-type a)
	   (aref a row col)))
       
       (declaim (ftype (function (,element-type ,type fixnum fixnum) ,element-type)
                       (setf ,fast-mref-symbol)))
       (declaim (inline (setf ,fast-mref-symbol)))
       (defun (setf ,fast-mref-symbol) (v m row col)
	 (with-typed-matrix-vals (m ,element-type a)
	   (setf (aref a row col) v)))

       #+nil
       (defmethod mref ((m ,type) &rest indices)
	 (with-typed-matrix-vals (m ,element-type a)
	   (apply #'aref a indices)))

       #+nil
       (defmethod (setf mref) (v (m ,type) &rest indices)
	 (with-typed-matrix-vals (m ,element-type a)
	   (setf (apply #'aref a indices) v))))))

(macrolet ((frob (type)
	     `(def-matrix-mref ,type)))
  (frob double-float-matrix)
  (frob single-float-matrix)
  (frob ub8-matrix)
  (frob ub16-matrix)
  (frob ub32-matrix)
  (frob sb8-matrix)
  (frob sb16-matrix)
  (frob sb32-matrix)
  (frob fixnum-matrix)
  (frob real-matrix)
  (frob integer-matrix)
  (frob complex-matrix)
  (frob bit-matrix))

#+nil
(define-compiler-macro mref (m row col)
  (typecase m
    (double-float-matrix
     `(with-typed-matrix-vals (,m double-float a)
        (declare type (simple-array double-float (* *)) a)
        (aref a ,row ,col)))
    (t
     `(aref (matrix-vals ,m) ,row ,col))))

