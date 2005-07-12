;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-transform.cl
;;; author: cyrus harmon
;;; time-stamp: Thu Jul  7 09:21:21 2005
;;;

(in-package :clem)

(defmacro def-matrix-transform (type-1 type-2 transform-type)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(transform-element-type (element-type (find-class `,transform-type))))
    (let ((zero (coerce 0 `,transform-element-type))
	  (one (coerce 1 `,transform-element-type)))
      `(progn
	 (defmethod %transform-matrix ((m ,type-1) (n ,type-2) (xfrm ,transform-type)
				      &key (background ,zero)
                                       (interpolation :nearest-neighbor))
	   (let ((mr (rows m)) (mc (cols m))
		 (nr (rows n)) (nc (cols n)))
	     (declare (type fixnum mr mc nr nc)
                      (type ,element-type-2 background)
		      (optimize (speed 3) (safety 0)))
	     (let  ((inv-xfrm (clem::invert-matrix xfrm))
		    (coord1 (make-instance ',transform-type :rows 3 :cols 1))
		    (coord2 (make-instance ',transform-type :rows 3 :cols 1)))
	       (let ((a (clem::matrix-vals m))
		     (b (clem::matrix-vals n))
		     (c (clem::matrix-vals coord1))
		     (d (clem::matrix-vals coord2))
		     (g (clem::matrix-vals inv-xfrm)))
		 (declare (type (simple-array ,element-type-1 (* *)) a)
			  (type (simple-array ,element-type-2 (* *)) b)
			  (type (simple-array ,transform-element-type (* *)) c d g)
                          (optimize (speed 3) (safety 0)))
		 (setf (aref c 2 0) ,one)
                 (dotimes (i nr)
                   (declare (type fixnum i))
                   (setf (aref c 0 0) (coerce i ',transform-element-type))
                   (dotimes (j nc)
                     (declare (type fixnum j))
                     (setf (aref c 1 0) (coerce j ',transform-element-type))
		     ;;; the slow(er) way to do this is:
                     ;;;   (setf (aref d 0 0) ,zero
                     ;;;         (aref d 1 0) ,zero
                     ;;;         (aref d 2 0) ,zero)
		     ;;; (clem::mat-mult3 inv-xfrm coord1 coord2)
                     ;;;
		     ;;; but since we don't need the full matrix multiply,
		     ;;; based on what we know is in the affine
		     ;;; transformation matrix, we can get away with
		     ;;; fewer operations (Foley et al., 1996, p. 213)
                     (setf (aref d 0 0) (+ (* (aref c 0 0) (aref g 0 0))
                                           (* (aref c 1 0) (aref g 0 1))
                                           (aref g 0 2))
                           (aref d 1 0) (+ (* (aref c 0 0) (aref g 1 0))
                                           (* (aref c 1 0) (aref g 1 1))
                                           (aref g 1 2)))
                     ;;; this does nearest neighbor interpolation
                     ;;; we should also offer a way to do bilinear,
                     ;;; bicubic, etc... interpolation
                     (case interpolation
                       ((:bilinear :bi-linear)
                        (if (and
                             (< ,(coerce most-negative-fixnum `,transform-element-type)
                                (aref d 0 0)
                                ,(coerce most-positive-fixnum `,transform-element-type))
                             (< ,(coerce most-negative-fixnum `,transform-element-type)
                                (aref d 1 0)
                                ,(coerce most-positive-fixnum `,transform-element-type)))
                            (multiple-value-bind (l ry)
                                (floor (the (,transform-element-type
                                             ,(coerce most-negative-fixnum `,transform-element-type)
                                             ,(coerce most-positive-fixnum `,transform-element-type))
                                         (aref d 0 0)))
                              (declare (type fixnum l)
                                       (type ,transform-element-type ry))
                              (multiple-value-bind (k rx)
                                  (floor (the (,transform-element-type
                                             ,(coerce most-negative-fixnum `,transform-element-type)
                                             ,(coerce most-positive-fixnum `,transform-element-type))
                                           (aref d 1 0)))
                                (declare (type fixnum k)
                                         (type ,transform-element-type rx))
                            ;;; add a case for the border here!
                                (cond
                                  ((and (< -1 l mr)
                                        (< -1 k mc))
                                   (let ((l1 (min (1+ l) (1- mr)))
                                         (k1 (min (1+ k) (1- mc))))
                                     (declare (type fixnum l1 k1))
                                     (setf (aref b i j)
                                           (maybe-truncate
                                            (+ (aref a l k)
                                               (* ry (- (aref a l1 k)
                                                        (aref a l k)))
                                               (* rx (- (aref a l k1)
                                                        (aref a l k)))
                                               (* rx ry
                                                  (- (+ (aref a l k)
                                                        (aref a l1 k1))
                                                     (+ (aref a l1 k)
                                                        (aref a l k1)))))
                                            ,transform-element-type
                                            ,element-type-2
                                            ))))
                                  (t
                                   (setf (aref b i j) background)))))
                            (setf (aref b i j) background)))
                       (t
                        (let ((oldx (the fixnum (round (aref d 0 0))))
                              (oldy (the fixnum (round (aref d 1 0)))))
                          (declare (type fixnum oldx oldy))
                          (if (and (< -1 oldx mr)
                                   (< -1 oldy mc))
                              (setf (aref b i j) (aref a oldx oldy))
                              (setf (aref b i j) background))))))))))
	   n)))))

(macrolet ((frob (type-1 type-2 type-3)
	     `(def-matrix-transform ,type-1 ,type-2 ,type-3)))
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob ub8-matrix ub8-matrix double-float-matrix)
  (frob ub16-matrix ub16-matrix double-float-matrix)
  (frob ub32-matrix ub32-matrix double-float-matrix)
  (frob sb8-matrix sb8-matrix double-float-matrix)
  (frob sb16-matrix sb16-matrix double-float-matrix)
  (frob sb32-matrix sb32-matrix double-float-matrix)
  (frob fixnum-matrix fixnum-matrix double-float-matrix)
  (frob bit-matrix bit-matrix double-float-matrix))

