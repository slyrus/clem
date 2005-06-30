;;;; 
;;;; to debug this stuff do something like the following:
;;;;

#+debug-def-matrix
(ensure-directories-exist "gensrc")

#+debug-def-matrix
(with-open-file (src "gensrc/single-float-matrix.cl"
		     :direction :output :if-exists :supersede)
  (print '(in-package :clem) src)
  (mapcar #'(lambda (x) (print x src))
	  (cdr (macroexpand
		'(defmatrixtype single-float-matrix () :element-type single-float
		  :accumulator-type single-float
		  :initial-element 0d0
		  :minval most-negative-single-float
		  :maxval most-positive-single-float
		  :specialized-array t)))))

#+debug-def-matrix
(let ((*print-right-margin* 132))
  (with-open-file (src "gensrc/unsigned-byte-matrix.cl"
		       :direction :output :if-exists :supersede)
    (print '(in-package :clem) src)
    (mapcar #'(lambda (x) (print x src))
	    (cdr (macroexpand
		  '(defmatrixtype unsigned-byte-matrix (integer-matrix) :element-type (unsigned-byte 8)
		    :accumulator-type (unsigned-byte 32)
		    :initial-element 0
		    :minval 0 :maxval 255
		    :specialized-array t))))))

#+debug-def-matrix
(with-open-file (src "gensrc/float-matrix.cl"
		     :direction :output :if-exists :supersede)
  (print '(in-package :clem) src)
  (mapcar #'(lambda (x) (print x src))
	  (cdr (macroexpand
		'(defmatrixtype float-matrix (real-matrix))))))

#+debug-def-matrix
(with-open-file (src "gensrc/real-matrix.cl"
		     :direction :output :if-exists :supersede)
  (print '(in-package :clem) src)
  (mapcar #'(lambda (x) (print x src))
	  (cdr (macroexpand
		'(defmatrixtype real-matrix (t-matrix)
		  :element-type real :accumulator-type real)))))

#+debug-def-matrix
(with-open-file (src "gensrc/integer-matrix.cl"
		 :direction :output :if-exists :supersede)
  (print '(in-package :clem) src)
  (mapcar #'(lambda (x) (print x src))
	  (cdr (macroexpand
		'(defmatrixtype integer-matrix (t-matrix)
		  :element-type integer :accumulator-type integer
		  :specialized-array t)))))

#+debug-def-matrix
(let ((*print-right-margin* 132))
  (with-open-file (src "gensrc/t-matrix.cl"
		   :direction :output :if-exists :supersede)
    (print '(in-package :clem) src)
    (mapcar #'(lambda (x) (print x src))
	    (cdr (macroexpand
		  '(defmatrixtype t-matrix ()
		    :element-type t :accumulator-type t))))))

#+debug-def-matrix
(let ((*print-right-margin* 132))
  (with-open-file (src "gensrc/double-float-matrix.cl"
		       :direction :output :if-exists :supersede)
    (print '(in-package :clem) src)
    (mapcar #'(lambda (x) (print x src))
	    (cdr (macroexpand
		  '(defmatrixtype double-float-matrix (float-matrix) :element-type double-float
		    :accumulator-type double-float
		    :initial-element 0d0
		    :minval most-negative-double-float
		    :maxval most-positive-double-float
		    :specialized-array t))))))
