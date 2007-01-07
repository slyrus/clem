
(in-package :clem)

;;; slow version

(defmethod mlog ((u matrix) &optional base)
  (map-set-val-copy u #'(lambda (x) (apply #'log x (when base base)))))

;;; faster versions

(defmacro def-matrix-log (type-1 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mlog-range" suffix))
	   ((m ,type-1) startr endr startc endc &optional base)
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
                           (apply #'log (mref m i j) (when base (list base))))))))
             p)))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mlog" suffix))
	   ((m ,type-1) &optional base)
	 (destructuring-bind (mr mc) (dim m)
	   (apply #',(ch-util:make-intern (concatenate 'string "mlog-range" suffix))
                  m 0 (1- mr) 0 (1- mc) (when base (list base))))))))

(defmacro def-matrix-log! (type-1 &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mlog-range!" suffix))
	   ((m ,type-1) startr endr startc endc &optional base)
         (with-typed-mref (m ,element-type-1)
           (do ((i startr (1+ i)))
               ((> i endr))
             (declare (dynamic-extent i) (type fixnum i))
             (do ((j startc (1+ j)))
                 ((> j endc))
               (declare (dynamic-extent j) (type fixnum j))
               (setf (mref m i j) (apply #'log (mref m i j) (when base (list base)))))))
         m)
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mlog!" suffix))
	   ((m ,type-1) &optional base)
	 (destructuring-bind (mr mc) (dim m)
	   (apply #',(ch-util:make-intern (concatenate 'string "mlog-range!" suffix))
                  m 0 (1- mr) 0 (1- mc) (when base (list base))))))))

(macrolet ((frob (type-1 type-2 &key suffix)
	     `(progn
		(def-matrix-log ,type-1 ,type-2 :suffix ,suffix)
		(def-matrix-log! ,type-1 :suffix ,suffix))))
  (frob double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix)
  (frob complex-matrix complex-matrix)
  (frob number-matrix number-matrix)
  (frob t-matrix t-matrix))

(macrolet ((frob (type-1 type-2 &key suffix)
	     `(progn
		(def-matrix-log ,type-1 ,type-2 :suffix ,suffix))))
  (frob ub8-matrix single-float-matrix)
  (frob ub16-matrix single-float-matrix)
  (frob ub32-matrix single-float-matrix)
  (frob sb8-matrix number-matrix)
  (frob sb16-matrix number-matrix)
  (frob sb32-matrix number-matrix)
  (frob bit-matrix single-float-matrix)
  (frob fixnum-matrix single-float-matrix)
  (frob real-matrix number-matrix)
  (frob integer-matrix number-matrix))

