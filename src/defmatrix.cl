;;;; File: defmatrix.cl
;;;; Author: Cyrus Harmon
;;;; Time-stamp: <2005-06-14 10:43:25 sly>
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
;(defclass integer-matrix () ())
;(defclass float-matrix () ())

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
	 
(defmacro with-map-range (m element-type startr endr startc endc (a i j) &body body)
  `(with-matrix-vals (,m ,element-type ,a)
     (do ((,i ,startr (1+ ,i)))
	 ((> ,i ,endr))
       (declare (dynamic-extent ,i) (type fixnum ,i))
       (do ((,j ,startc (1+ ,j)))
	   ((> ,j ,endc))
	 (declare (dynamic-extent ,j) (type fixnum ,j))
	 ,@body))))

;;; FIXME
;;; Change with-matrix-range-do to take matrix-type instead of element-type
;;; and specialized-array and use the metaclass to get this information.
;;; Can't just do this at runtime, unfortunately.

(defmacro with-matrix-range-do (matrix-class m n p startr endr startc endc (a b c i j) &body body)
  (let ((mat-class (if (typep matrix-class 'class)
		       matrix-class
		       (find-class matrix-class))))
    (let ((element-type (element-type mat-class)))
      `(with-matrix-vals (,m ,element-type ,a)
	 (with-matrix-vals (,n ,element-type ,b)
	   (with-matrix-vals (,p ,element-type ,c)
	     (do ((,i ,startr (1+ ,i)))
		 ((> ,i ,endr))
	       (declare #-sbcl (dynamic-extent ,i) (type fixnum ,i))
	       (do ((,j ,startc (1+ ,j)))
		   ((> ,j ,endc))
		 (declare #-sbcl (dynamic-extent ,j) (type fixnum ,j))
		 ,@body))))))))


(defmacro defmatrix-method (method (&rest args) &body body)
  `(defmethod ,method ,args
     ,@body))


;;; for the moment we're ignoring the specialized-array flag in
;;; theory, we could use this to allow for arrays of non-specialized
;;; types like integer, real or number, but for now we don't call
;;; any of these macros to generate specialized functions so
;;; we just get the generic version. consider removing this flag.
(defmacro defmatrixtype (type direct-superclasses &key 
			 (element-type 'double-float)
			 (accumulator-type 'double-float)
			 (initial-element 0)
			 (specialized-array nil)
			 minval maxval
			 (val-format "~4,9F"))
  (declare (ignore specialized-array))
  (unless direct-superclasses (setf direct-superclasses '(matrix)))
  `(progn
     (defclass ,type ,direct-superclasses
       ((initial-element :accessor initial-element :initarg :initial-element :initform ,initial-element))
       (:metaclass standard-matrix-class)
       (:element-type ,(delistify element-type))
       (:accumulator-type ,(delistify accumulator-type))
       (:val-format ,(delistify val-format))
       (:minval ,(if (symbolp minval) (symbol-value minval) minval))
       (:maxval ,(if (symbolp maxval) (symbol-value minval) maxval)))))

(defmacro defmatrixfuncs (type &key 
			  (element-type 'double-float)
			  (accumulator-type 'double-float)
			  (specialized-array nil)
			  (integral nil)
			  minval maxval)
  `(progn
     (defmethod mref ((m ,type) (row fixnum) (col fixnum))
       (with-typed-matrix-vals (m ,element-type t a)
	 (aref a row col)))

     (defmethod (setf mref) (v (m ,type) (row fixnum) (col fixnum))
       (with-typed-matrix-vals (m ,element-type t a)
	 (setf (aref a row col) v)))

     (defmethod ,(make-intern (concatenate 'string "array->" (symbol-name type))) ((a array))
       (array->matrix a :matrix-class ',type))
     
     (defmethod sum-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce 0 ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type startr endr startc endc (a i j)
	   (incf acc (aref a i j)))
	 acc))

     (defmethod sum-square-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce 0 ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type startr endr startc endc (a i j)
	   (incf acc (* (aref a i j) (aref a i j))))
	 acc))
     
     (defmethod min-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type startr endr startc endc (a i j)
	   (when (< (aref a i j) acc)
	     (setf acc (aref a i j))))
	 acc))

     (defmethod max-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-type)))
	 (declare (type ,accumulator-type acc))
	 (with-map-range m ,element-type startr endr startc endc (a i j)
	   (when (> (aref a i j) acc)
	     (setf acc (aref a i j))))
	 acc))
     
     (defmethod sample-variance-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
       (let ((acc (coerce 0 ',accumulator-type)))
	 (let ((mu (mean-range m startr endr startc endc)))
	   (let ((musq (* mu mu)))
	     (with-map-range m ,element-type startr endr startc endc (a i j)
	       (incf acc (- (* (aref a i j) (aref a i j)) musq)))))
	 (double-float-divide acc (1- (count-range startr endr startc endc)))))
     
     (defmethod set-val ((m ,type) i j v &key (coerce t))
      (declare (fixnum i j))
       (setf (aref (matrix-vals m) i j) 
	     (if coerce
		 (coerce v (element-type (class-of m)))
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
		       (element-type (class-of m)))))
     
     (defmethod map-set-val ((m ,type) f)
       (destructuring-bind (rows cols) (mapcar #'1- (dim m))
	 (declare (dynamic-extent rows cols) (fixnum rows cols))
	 (with-map-range m ,element-type 0 rows 0 cols (a i j)
	   (setf (aref a i j) (funcall f (aref a i j)))))
       m)
     
     (defmethod ,(make-intern (concatenate 'string "random-" (symbol-name type))) (rows cols &key (max nil))
       (let ((a (make-instance ',type :rows rows :cols cols)))
	 (map-set-val-fit a #'(lambda (x) (declare (ignore x))
				      (random
				       (if max (coerce max ',element-type)
					   (if ,maxval ,maxval 255)))))
	 a))
     
     (defmethod mat-subtr! ((m ,type) (n ,type))
       (and (equal (dim m) (dim n))
	    (destructuring-bind (mr mc) (dim m)
	      (with-matrix-range-do ,type m n m 0 (- mr 1) 0 (- mc 1) (a b c i j)
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

     (cond ((and (find-class 'integer-matrix nil)
		 (member (find-class 'integer-matrix) (sb-mop::class-precedence-list (find-class ',type))))
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
	   ((and (find-class 'float-matrix nil)
		 (member (find-class 'float-matrix) (sb-mop::class-precedence-list (find-class ',type))))
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


(defmacro def-move-element (type-1 type-2)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod move-element
	   ((m ,type-1) i1 j1 (n ,type-2) i2 j2)
	 (with-matrix-vals (m ,element-type-1 a)
	   (with-matrix-vals (n ,element-type-2 b)
	     (setf (aref b i2 j2)
		   ,(if (eql element-type-1 element-type-2)
			`(aref a i1 j1)
			`(coerce (aref a i1 j1) ',element-type-2)))))))))

(defmacro maybe-coerce (val type-1 type-2)
  (if (eql type-1 type-2)
      val
      `(coerce ,val ',type-2)))

(defmacro maybe-truncate (val type-1 type-2)
  (if (and (subtypep type-2 'integer)
	   (subtypep type-1 'float))
      `(nth-value 0 (truncate ,val))
      `(maybe-coerce ,val ,type-1 ,type-2)))
	   
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
					   `(coerce (constrain ,min (aref a i j) ,max)
						    ',element-type-2)))))))
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
       (defmethod ,(make-intern (concatenate 'string "mat-add-range" suffix))
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
       
       (defmethod ,(make-intern (concatenate 'string "mat-add" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(make-intern (concatenate 'string "mat-add-range" suffix)) m n 0 (1- mr) 0 (1- mc)))))))
       
(defmacro def-matrix-add! (type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod ,(make-intern (concatenate 'string "mat-add!-range" suffix))
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
       
       (defmethod ,(make-intern (concatenate 'string "mat-add!" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(make-intern (concatenate 'string "mat-add!-range" suffix)) m n 0 (1- mr) 0 (1- mc))))
       
       )))

(defmacro def-matrix-subtr (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(make-intern (concatenate 'string "mat-subtr-range" suffix))
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
			     (- (aref a i j) (aref b i j))))))))
	     p)))
       
       (defmethod ,(make-intern (concatenate 'string "mat-subtr" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (mat-subtr-range m n 0 (1- mr) 0 (1- mc)))))))

(defmacro def-matrix-subtr! (type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod ,(make-intern (concatenate 'string "mat-subtr!-range" suffix))
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

       (defmethod ,(make-intern (concatenate 'string "mat-subtr!" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (mat-subtr!-range m n 0 (1- mr) 0 (1- mc)))))))
