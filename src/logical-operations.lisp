;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-binary-op.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(macrolet ((frob (name op type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-binary-op ,name ,op ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  ;; matand
  (frob "mlogand" logand bit-matrix bit-matrix bit-matrix)
  (frob "mlogand" logand ub8-matrix ub8-matrix ub8-matrix)
  (frob "mlogand" logand ub16-matrix ub16-matrix ub16-matrix)
  (frob "mlogand" logand ub32-matrix ub32-matrix ub32-matrix)
  (frob "mlogand" logand integer-matrix integer-matrix integer-matrix))



(macrolet ((frob (name op type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-binary-op ,name ,op ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  ;; matand
  (frob "mlogand" logand bit-matrix bit-matrix bit-matrix)
  (frob "mlogand" logand ub8-matrix ub8-matrix ub8-matrix)
  (frob "mlogand" logand ub16-matrix ub16-matrix ub16-matrix)
  (frob "mlogand" logand ub32-matrix ub32-matrix ub32-matrix)
  (frob "mlogand" logand integer-matrix integer-matrix integer-matrix)
  
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

(macrolet ((frob (name op type-1 type-2 &key suffix)
	     `(progn
		(def-unary-op ,name ,op ,type-1 ,type-2 :suffix ,suffix))))
  ;; mlognot
  (frob "mlognot" lognot integer-matrix integer-matrix)
  (frob "mlognot" lognot fixnum-matrix fixnum-matrix)
  (frob "mlognot" lognot sb8-matrix sb8-matrix)
  (frob "mlognot" lognot sb16-matrix sb16-matrix)
  (frob "mlognot" lognot sb32-matrix sb32-matrix))

(defmacro defmbitnor (name type-1 type-2 accumulator-type &key suffix)
  (let ((class-1 (find-class `,type-1))
        (class-2 (find-class `,type-2)))
    (let ((element-type-1 (element-type class-1))
          (element-type-2 (element-type class-2))
          (accumulator-element-type (element-type (find-class `,accumulator-type))))
      (let ((max (max (maxval class-1) (maxval class-2))))
        `(progn
           (defmethod ,(ch-util:make-intern (concatenate 'string name "-range" suffix))
               ((m ,type-1) (n ,type-2) startr endr startc endc)
             (destructuring-bind (mr mc) (dim m)
               (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
                 (with-typed-mref (m ,element-type-1)
                   (with-typed-mref (n ,element-type-2)
                     (with-typed-mref (p ,accumulator-element-type)
                       (do ((i startr (1+ i)))
                           ((> i endr))
                         (declare (dynamic-extent i) (type fixnum i))
                         (do ((j startc (1+ j)))
                             ((> j endc))
                           (declare (dynamic-extent j) (type fixnum j))
                           (setf (mref p i j)
                                 (logand ,max (lognor (mref m i j) (mref n i j)))))))))
                 p)))
       
           (defmethod ,(ch-util:make-intern (concatenate 'string name suffix))
               ((m ,type-1) (n ,type-2))
             (destructuring-bind (mr mc) (dim m)
               (,(ch-util:make-intern (concatenate 'string name "-range" suffix)) m n 0 (1- mr) 0 (1- mc)))))))))
       
(defmacro defmbitnor! (name type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((class-1 (find-class `,type-1))
        (class-2 (find-class `,type-2)))
    (let ((element-type-1 (element-type class-1))
          (element-type-2 (element-type class-2)))
      (let ((max (max (maxval class-1) (maxval class-2))))
        `(progn
           (defmethod ,(ch-util:make-intern (concatenate 'string name "!-range" suffix))
               ((m ,type-1) (n ,type-2) startr endr startc endc)
             (with-typed-mref (m ,element-type-1)
               (with-typed-mref (n ,element-type-2)
                 (do ((i startr (1+ i)))
                     ((> i endr))
                   (declare (dynamic-extent i) (type fixnum i))
                   (do ((j startc (1+ j)))
                       ((> j endc))
                     (declare (dynamic-extent j) (type fixnum j))
                     (setf (mref m i j)
                           (logand ,max (lognor (mref m i j) (mref n i j)))))))
               m))
       
           (defmethod ,(ch-util:make-intern (concatenate 'string name "!" suffix))
               ((m ,type-1) (n ,type-2))
             (destructuring-bind (mr mc) (dim m)
               (,(ch-util:make-intern (concatenate 'string name "!-range" suffix)) m n 0 (1- mr) 0 (1- mc))))
         
           )))))


(defun bitnor (integer1 integer2 andmask)
  (logand andmask (lognor integer1 integer2)))

(macrolet ((frob (name type-1 type-2 type-3 &key suffix)
	     `(progn
		(defmbitnor ,name ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(defmbitnor! ,name ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  ;; mbitnor
  (frob "mbitnor" bit-matrix bit-matrix bit-matrix)
  (frob "mbitnor" ub8-matrix ub8-matrix ub8-matrix)
  (frob "mbitnor" ub16-matrix ub16-matrix ub16-matrix)
  (frob "mbitnor" ub32-matrix ub32-matrix ub32-matrix))

