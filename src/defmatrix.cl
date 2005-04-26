;;;; File: defmatrix.cl
;;;; Author: Cyrus Harmon
;;;; Time-stamp: <2005-04-26 12:47:09 sly>
;;;; 
;;;; This file contains definitions for typed matrices. Typed
;;;; matrices have elements that are of a single type (although
;;;; this type can have mutliple subtypes; even a t-matrix where
;;;; each of the items is of type t can offer substantial
;;;; performance gains, at least on SBCL).
;;;;

(in-package :clem)

;;; forward class definitions to keep things going for the moment
;;; these should go away once the MOP stuff is done.
(defclass integer-matrix () ())
(defclass float-matrix () ())

;;; Also taken from KMR's clsql package
(declaim (inline delistify))
(defun delistify (list)
  "Some MOPs, like openmcl 0.14.2, cons attribute values in a list."
  (if (and (listp list) (null (cdr list)))
      (car list)
      list))

(defmacro with-typed-matrix-vals ((m element-type specialized-array a) &body body)
  `(let ((,a (matrix-vals ,m)))
     ,(when specialized-array
	    `(declare 
	      (dynamic-extent ,a)
	      (type (simple-array ,element-type (* *)) ,a)))
     ,@body))

(defmacro with-untyped-matrix-vals ((m element-type specialized-array a) &body body)
  (declare (ignore element-type specialized-array))
  `(let ((,a (matrix-vals ,m)))
     ,@body))

(defmacro with-matrix-vals ((m element-type a) &body body)
  `(if (equal (quote ,element-type) (element-type (class-of ,m)))
       (with-typed-matrix-vals (,m ,element-type t ,a)
	 ,@body)
       (with-untyped-matrix-vals (,m ,element-type nil ,a)
	 ,@body)))
       
	 
(defmacro with-map-range (m element-type specialized-array startr endr startc endc (a i j) &body body)
  `(with-matrix-vals (,m ,element-type ,a)
     (do ((,i ,startr (1+ ,i)))
	 ((> ,i ,endr))
       (declare (dynamic-extent ,i) (type fixnum ,i))
       (do ((,j ,startc (1+ ,j)))
	   ((> ,j ,endc))
	 (declare (dynamic-extent ,j) (type fixnum ,j))
	 ,@body))))

(defmacro with-map-range-vals (v startr endr startc endc (i j) &body body)
  `(do ((,i ,startr (1+ ,i)))
       ((> ,i ,endr))
     (declare (dynamic-extent ,i) (type fixnum ,i))
     (do ((,j ,startc (1+ ,j)))
	 ((> ,j ,endc))
       (declare (dynamic-extent ,j) (type fixnum ,j))
       ,@body)))

(defmacro with-mat-scalar-op-range (m n p element-type specialized-array startr endr startc endc (a b c i j) &body body)
;  `(let ((,a (matrix-vals ,m))
;	 (,b (matrix-vals ,n))
;	 (,c (matrix-vals ,p)))
;     ,(when specialized-array
;	    `(declare (type (simple-array ,element-type (* *)) ,a ,b ,c)))

  `(with-matrix-vals (,m ,element-type ,a)
     (with-matrix-vals (,n ,element-type ,b)
       (with-matrix-vals (,p ,element-type ,c)
	 (do ((,i ,startr (1+ ,i)))
	     ((> ,i ,endr))
	   (declare (dynamic-extent ,i) (type fixnum ,i))
	   (do ((,j ,startc (1+ ,j)))
	       ((> ,j ,endc))
	     (declare (dynamic-extent ,j) (type fixnum ,j))
	     ,@body))))))
     

(defmacro defmatrixtype (type direct-superclasses &key 
			 (element-type 'double-float)
			 (accumulator-type 'double-float)
			 (initial-element 0)
			 (specialized-array nil)
			 (integral nil)
			 minval maxval
			 (val-format "~4,9F"))
  (unless direct-superclasses (setf direct-superclasses '(typed-matrix)))
  `(progn
     
     (defclass ,type ,direct-superclasses
       ((element-type :allocation :class :accessor element-type :initform ',element-type)
	(specialized-array :allocation :class :accessor specialized-array-p :initform ',specialized-array)
	(minval :accessor minval :allocation :class :initform ,minval)
	(maxval :accessor maxval :allocation :class :initform ,maxval)
	(initial-element :accessor initial-element :initarg :initial-element :initform ,initial-element)
	(val-format :accessor val-format :initform ,val-format))
       (:metaclass standard-matrix-class)
       (:element-type ,(delistify element-type)))
     
     (defmethod mref ((m ,type) (row fixnum) (col fixnum))
       (with-typed-matrix-vals (m ,element-type t a)
	 (aref a row col)))

     (defmethod (setf mref) (v (m ,type) (row fixnum) (col fixnum))
       (with-typed-matrix-vals (m ,element-type t a)
	 (setf (aref a row col) v)))

     (defmethod ,(make-intern (strcat "array->" (symbol-name type))) ((a array))
       (array->matrix a :matrix-class ',type))
     
     (defmethod sum-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce 0 ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type ,specialized-array startr endr startc endc (a i j)
	   (incf acc (aref a i j)))
	 acc))

     (defmethod sum-square-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce 0 ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type ,specialized-array startr endr startc endc (a i j)
	   (incf acc (* (aref a i j) (aref a i j))))
	 acc))
     
     (defmethod min-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type ,specialized-array startr endr startc endc (a i j)
	   (when (< (aref a i j) acc)
	     (setf acc (aref a i j))))
	 acc))

     (defmethod max-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type ,specialized-array startr endr startc endc (a i j)
	   (when (> (aref a i j) acc)
	     (setf acc (aref a i j))))
	 acc))
     
     (defmethod sample-variance-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce 0 ',accumulator-type)))
;	 (declare (type (or (unsigned-byte 32) single-float double-float) acc))
	 (let ((mu (mean-range m startr endr startc endc)))
;	   (declare (type (or (unsigned-byte 32) single-float double-float) mu))
	   (let ((musq (* mu mu)))
;	     (declare (type (or (unsigned-byte 32) single-float double-float) musq))
	     (with-map-range m ,element-type ,specialized-array startr endr startc endc (a i j)
	       (incf acc (- (* (aref a i j) (aref a i j)) musq)))))
	 (util:double-float-divide acc (1- (count-range startr endr startc endc)))))
     
     (defmethod set-val ((m ,type) i j v &key (coerce t))
      (declare (fixnum i j))
       (setf (aref (matrix-vals m) i j) 
	     (if coerce
		 (coerce v (element-type m))
		 v)))

     (defmethod fit ((m ,type) v)
       (declare (ignore m))
       ,(if maxval
	    `(if (> v ,maxval)
		 ,maxval
		 ,(if minval `(if (< v ,minval) ,minval v) `v))
	    (if minval `(if (< v ,minval) ,minval v) `v)))
     
       
     (defmethod set-val-fit ((m ,type) i j v &key (truncate nil))
       (set-val m i j (coerce 
		       ,(if (and minval maxval)
			    `(cond ((< v ,minval) ,minval)
				   ((> v ,maxval) ,maxval)
				   (t (if truncate (truncate v) v)))
			    `(if truncate (truncate v) v))
		       (element-type m))))
     
     (defmethod map-set-val ((m ,type) f)
       (destructuring-bind (rows cols) (mapcar #'1- (dim m))
	 (declare (dynamic-extent rows cols) (fixnum rows cols))
	 (with-map-range m ,element-type ,specialized-array 0 rows 0 cols (a i j)
	   (setf (aref a i j) (funcall f (aref a i j)))))
       m)
     
     (defmethod ,(make-intern (strcat "random-" (symbol-name type))) (rows cols &key (max nil))
       (let ((a (make-instance ',type :rows rows :cols cols)))
	 (map-set-val-fit a #'(lambda (x) (declare (ignore x))
				      (random
				       (if max (coerce max ',element-type)
					   (if ,maxval ,maxval 255)))))
	 a))
     
     (defmethod mat-add ((m ,type) (n ,type))
       (and (equal (dim m) (dim n))
	    (destructuring-bind (mr mc) (dim m)
	      (let ((p (mat-copy-proto m)))
		(with-mat-scalar-op-range m n p ,element-type ,specialized-array 0 (- mr 1) 0 (- mc 1) (a b c i j)
		  (setf (aref c i j)
			(fit p (+ (aref a i j) (aref b i j)))))
		p))))
     
     (defmethod mat-add! ((m ,type) (n ,type))
       (and (equal (dim m) (dim n))
	    (destructuring-bind (mr mc) (dim m)
	      (with-mat-scalar-op-range m n m ,element-type ,specialized-array 0 (- mr 1) 0 (- mc 1) (a b c i j)
		(setf (aref c i j)
		      (fit m (+ (aref a i j) (aref b i j)))))
	      m)))
     
     (defmethod mat-subtr ((m ,type) (n ,type))
       (and (equal (dim m) (dim n))
	    (destructuring-bind (mr mc) (dim m)
	      (let ((p (mat-copy-proto m)))
		(with-mat-scalar-op-range m n p ,element-type
		    ,specialized-array
		    0 (- mr 1) 0 (- mc 1) (a b c i j)
		  (setf (aref c i j)
			(fit p (- (aref a i j) (aref b i j)))))
		p))))
     
     (defmethod mat-subtr! ((m ,type) (n ,type))
       (and (equal (dim m) (dim n))
	    (destructuring-bind (mr mc) (dim m)
	      (with-mat-scalar-op-range m n m ,element-type ,specialized-array 0 (- mr 1) 0 (- mc 1) (a b c i j)
		(setf (aref c i j)
		      (fit m (- (aref a i j) (aref b i j)))))
	      m)))
     
     (defmethod normalize ((u ,type) &key (normin) (normax))
       (let ((min (min-val u))
	     (max (max-val u))
	     (nmin (if normin normin (if ,minval ,minval 0)))
	     (nmax (if normax normax (if ,maxval ,maxval 255))))
	 (let ((slope (double-float-divide (- nmax nmin)  (- max min))))
	   (map-set-val-fit u #'(lambda (x) (+ nmin (* slope (- x min))))))))
     
     (defmethod discrete-convolve ((u ,type) (v ,type)
				   &key (truncate nil) (norm-v t)
				   (matrix-class nil))
       (declare (optimize (speed 3) (safety 0) (space 0) (debug 3)))
  ;;; ur, uc, vr, vc are the number of rows and columns in u and v
       (destructuring-bind (ur uc) (dim u)
	 (declare (type fixnum ur uc))
	 (destructuring-bind (vr vc) (dim v)
	   (declare (type fixnum vr vc))
      ;;; need a new matrix z to hold the values of the convolved matrix
      ;;; dim z should be dim u + dim v - 1
	   (let ((zr (+ ur vr (- 1)))
		 (zc (+ uc vc (- 1))))
	     (declare (type fixnum zr zc))
	     (unless matrix-class
	  (setf matrix-class (class-of u)))
	(with-typed-matrix-vals (u ,element-type t uval)
	  (with-typed-matrix-vals (v ,element-type t vval)
	    (let ((z (make-instance matrix-class :rows zr :cols zc))
		  (zero-element (coerce 0 ',element-type))
		  (vsum (sum v)))
	      (declare (type ,accumulator-type vsum))
	      (with-typed-matrix-vals (z ,element-type t zval)
		(dotimes (i zr)
		  (declare (type fixnum i))
		  (let ((ustartr (max 0 (- i vr -1)))
			(uendr (min (- ur 1) i))
			(vstartr (- vr (max (- vr i) 1)))
			(vendr (- vr (min (- zr i) vr))))
		    (declare (type fixnum ustartr uendr vstartr vendr))
		    (dotimes (j zc)
		      (declare (type fixnum j))
		      (let ((ustartc (max 0 (- j vc -1)))
			    (uendc (min (- uc 1) j))
			    (vstartc (- vc (max (- vc j) 1)))
			    (vendc (- vc (min (- zc j) vc)))
			    (acc zero-element))
			(declare (type fixnum ustartc uendc vstartc vendc)
				 (type ,accumulator-type acc))
			(let ((normval zero-element)
			      (normp))
			  (when (and norm-v (or (not (= vendr vendc 0))
						(< vstartr (- vr 1))
						(< vstartc (- vc 1))))
			    (let ((rsum (sum-range v vendr vstartr vendc vstartc)))
			      (declare (type ,accumulator-type rsum))
			      (unless (= rsum zero-element)
				(setf normval (/ vsum rsum))
				(setf normp t))))
			  (do ((urow ustartr (1+ urow))
			       (vrow vstartr (1- vrow)))
			      ((> urow uendr))
			    (declare (fixnum urow vrow))
			    (do ((ucol ustartc (1+ ucol))
				 (vcol vstartc (1- vcol)))
				((> ucol uendc))
			      (declare (fixnum ucol vcol))
			      (incf acc (* (aref uval urow ucol) (aref vval vrow vcol)))))
			  (when normp (setf acc (* acc normval)))
			  ,(if integral
			       `(setf (aref zval i j) (truncate acc))
			       `(setf (aref zval i j) acc))
			  ))))))
	      z)))))))

     (cond ((member (find-class 'integer-matrix) (sb-mop::class-precedence-list (find-class ',type)))
	    (defmethod scalar-divide-row ((m ,type)  k q)
	      (with-typed-matrix-vals (m ,element-type ,specialized-array a)
		(dotimes (j (cols m))
		  (setf (aref a k j) (fit m (truncate (/ (aref a k j) q))))))
	      m)
	    (defmethod scalar-mult-row ((m ,type) k q)
	      (with-typed-matrix-vals (m ,element-type ,specialized-array a)
		(dotimes (j (cols m))
		  (setf (aref a k j) (fit m (truncate (* (aref a k j) q))))))
	      m))
	   ((member (find-class 'float-matrix) (sb-mop::class-precedence-list (find-class ',type)))
	    (defmethod scalar-divide-row ((m ,type)  k q)
	      (with-typed-matrix-vals (m ,element-type ,specialized-array a)
		(dotimes (j (cols m))
		  (setf (aref a k j) (fit m (/ (aref a k j) q)))))
	      m)
	    (defmethod scalar-mult-row ((m ,type) k q)
	      (with-typed-matrix-vals (m ,element-type ,specialized-array a)
		(dotimes (j (cols m))
		  (setf (aref a k j) (fit m (* (aref a k j) q)))))
	      m)))
     
     ))

