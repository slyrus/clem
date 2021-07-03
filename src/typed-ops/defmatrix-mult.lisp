;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-mult.cl
;;; author: cyrus harmon
;;;

(in-package :clem)
  
#+sbcl
(defun displace-to-1d-array (matrix)
  (sb-kernel:%array-data (clem::matrix-vals matrix)))

(defgeneric mat-mult3 (m n p))

(defmacro def-matrix-mult (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn

       #+sbcl
       (defmethod ,(make-intern (concatenate 'string "mat-mult3" suffix))
	   ((m ,type-1) (n ,type-2) (p ,accumulator-type))
	 (declare (optimize (speed 3) (safety 0)))
	 (let ((a (displace-to-1d-array m))
	       (b (displace-to-1d-array n))
	       (c (displace-to-1d-array p))
	       (atemp (coerce 0 ',accumulator-element-type)))
	   (declare (type (simple-array ,element-type-1 (*)) a)
		    (type (simple-array ,element-type-2 (*)) b)
		    (type (simple-array ,accumulator-element-type (*)) c)
		    (type ,accumulator-element-type atemp))
	   (let ((mr (rows m)) (mc (cols m))
		 (nr (rows n)) (nc (cols n))
                 (pr (rows p)) (pc (cols p)))
	     (declare (type fixnum mr mc nr nc pr pc))
	     (if (and (= mc nr) (= mr pr) (= nc pc))
                 (loop for k fixnum from 0 below mc
                    do (loop for i from 0 below mr
                          for aind = k then (+ aind mc)
                          do (setf atemp (aref a aind))
                            (loop for j fixnum from 0 below nc
                               for bind = (* k nc) then (1+ bind)
                               for cind = (* i nc) then (1+ cind)
                               do (incf (aref c cind)
                                        (the ,accumulator-element-type (* atemp (aref b bind)))))))
                 (error 'matrix-argument-error
                        :format-control
                        "Incompatible matrix dimensions in mat-mult3 (~S x ~S) * (~S x ~S) => (~S x ~S)."
                        :format-arguments (list mr mc nr nc pr pc))
                 )))
	 p)

       #-sbcl
       (defmethod ,(make-intern (concatenate 'string "mat-mult3" suffix))
	   ((m ,type-1) (n ,type-1) (p ,type-1))
	 (declare (optimize (speed 3) (safety 0)))
	 (let ((a (clem::matrix-vals m))
	       (b (clem::matrix-vals n))
	       (c (clem::matrix-vals p))
	       (atemp (coerce 0 ',accumulator-element-type)))
	   (declare (type (simple-array ,element-type-1 *) a)
		    (type (simple-array ,element-type-2 *) b)
		    (type (simple-array ,accumulator-element-type *) c)
		    (type ,accumulator-element-type atemp))
	   (let ((mr (rows m)) (mc (cols m))
		 (nr (rows n)) (nc (cols n))
                 (pr (rows n)) (pc (cols n)))
	     (declare (type fixnum mr mc nr nc pr pc))
	     (if (and (= mc nr) (= mr pr) (= nc pc))
                 (do ((k 0 (the fixnum (1+ k))))
                     ((>= k mc))
                   (declare (type fixnum k))
                   (do ((i 0 (the fixnum (1+ i))))
                       ((>= i mr))
                     (declare (type fixnum i))
                     (setf atemp (aref a i k))
                     (do ((j 0 (the fixnum (1+ j))))
                         ((>= j nc))
                       (declare (type fixnum j))
                       (incf (aref c i j) (the ,accumulator-element-type (* atemp (aref b k j)))))))
                 (error 'matrix-argument-error
                        :format-control
                        "Incompatible matrix dimensions in mat-mult3 (~S x ~S) * (~S x ~S) => (~S x ~S)."
                        :format-arguments (list mr mc nr nc pr pc)))))
	 p)
       
       (defmethod ,(make-intern (concatenate 'string "mat-mult" suffix))
	   ((m ,type-1) (n ,type-2))
	 (declare (optimize (speed 3) (safety 0)))
	 (let ((mr (rows m))
	       (nc (cols n)))
	   (declare (type fixnum mr nc))
           (if (= (cols m) (rows n))
               (let ((p (make-instance ',accumulator-type
                                       :rows (the fixnum mr)
                                       :cols (the fixnum nc))))
                 (mat-mult3 m n p))
               (error 'matrix-argument-error
                 :format-control
                 "Incompatible matrix dimensions in mat-mult (~S x ~S) * (~S x ~S)."
                 :format-arguments (list (rows m) (cols m) (rows n) (cols n)))))))))


;;; need to think about which mat-mult type combinations are needed
;;; here. add more as apporpriate.
(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(def-matrix-mult ,type-1 ,type-2 ,type-3 :suffix ,suffix)))
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob ub8-matrix ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix ub32-matrix)
  (frob sb8-matrix sb8-matrix sb32-matrix)
  (frob sb16-matrix sb16-matrix sb32-matrix)
  (frob sb32-matrix sb32-matrix sb32-matrix)
  (frob fixnum-matrix fixnum-matrix fixnum-matrix)
  (frob bit-matrix bit-matrix bit-matrix))

