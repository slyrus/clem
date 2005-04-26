
(in-package :clem)

(defmatrixtype t-matrix ()
  :element-type t
  :accumulator-type t)

(defmatrixtype real-matrix (t-matrix)
  :element-type t
  :accumulator-type t)

(defmatrixtype float-matrix (real-matrix)
  :element-type t
  :accumulator-type t
  :val-format "~4,9F")

(defmatrixtype integer-matrix (real-matrix)
  :element-type t
  :accumulator-type t
  :integral t
  :val-format "~d"
  :specialized-array t)

(defmatrixtype bit-matrix () :element-type (unsigned-byte 1)
	       :accumulator-type (signed-byte 32)
	       :minval 0
	       :maxval 1
	       :val-format "~d"
	       :integral t
	       :specialized-array t)

(defmatrixtype signed-byte-matrix (integer-matrix)
  :element-type (signed-byte 8)
  :accumulator-type (unsigned-byte 32)
  :minval (- (expt 2 7))
  :maxval (- (expt 2 7) 1)
  :val-format "~d"
  :integral t
  :specialized-array t)

(defmatrixtype unsigned-byte-matrix (integer-matrix)
  :element-type (unsigned-byte 8)
  :accumulator-type (unsigned-byte 32)
  :minval 0
  :maxval (- (expt 2 8) 1)
  :val-format "~d"
  :integral t
  :specialized-array t)

(defmatrixtype signed-word-matrix (integer-matrix) :element-type (signed-byte 16)
	       :accumulator-type (unsigned-byte 32)
	       :minval (- (expt 2 15))
	       :maxval (- (expt 2 15) 1)
	       :integral t
	       :specialized-array t)

(defmatrixtype unsigned-word-matrix (integer-matrix) :element-type (unsigned-byte 16)
	       :accumulator-type (unsigned-byte 32)
	       :minval 0
	       :maxval (- (expt 2 16) 1)
	       :val-format "~d"
	       :integral t
	       :specialized-array t)

(defmatrixtype signed-long-matrix (integer-matrix) :element-type (signed-byte 32)
	       :accumulator-type (unsigned-byte 32)
	       :minval (- (expt 2 31))
	       :maxval (- (expt 2 31) 1)
	       :val-format "~d"
	       :integral t
	       :specialized-array t)

(defmatrixtype unsigned-long-matrix (integer-matrix) :element-type (unsigned-byte 32)
	       :accumulator-type (unsigned-byte 32)
	       :minval 0
	       :maxval (- (expt 2 32) 1)
	       :val-format "~d"
	       :integral t
	       :specialized-array t)

(defmatrixtype fixnum-matrix (integer-matrix) :element-type fixnum
	       :accumulator-type (unsigned-byte 32)
	       :minval most-negative-fixnum
	       :maxval most-positive-fixnum
	       :val-format "~d"
	       :integral t
	       :specialized-array t)

(defmatrixtype single-float-matrix (float-matrix) :element-type single-float
	       :accumulator-type single-float
	       :initial-element 0f0
	       :minval most-negative-single-float
	       :maxval most-positive-single-float
	       :specialized-array t)

(defmatrixtype double-float-matrix (float-matrix) :element-type double-float
	       :accumulator-type double-float
	       :initial-element 0d0
	       :minval most-negative-double-float
	       :maxval most-positive-double-float
	       :specialized-array t)

;;;; to debug this stuff do something like the following:

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
		    :integral t
		    :specialized-array t))))))

(with-open-file (src "gensrc/float-matrix.cl"
		     :direction :output :if-exists :supersede)
  (print '(in-package :clem) src)
  (mapcar #'(lambda (x) (print x src))
	  (cdr (macroexpand
		'(defmatrixtype float-matrix (real-matrix))))))

(with-open-file (src "gensrc/real-matrix.cl"
		     :direction :output :if-exists :supersede)
  (print '(in-package :clem) src)
  (mapcar #'(lambda (x) (print x src))
	  (cdr (macroexpand
		'(defmatrixtype real-matrix (t-matrix)
		  :element-type real :accumulator-type real)))))

(with-open-file (src "gensrc/integer-matrix.cl"
		     :direction :output :if-exists :supersede)
  (print '(in-package :clem) src)
  (mapcar #'(lambda (x) (print x src))
	  (cdr (macroexpand
		'(defmatrixtype integer-matrix (t-matrix)
		  :element-type integer :accumulator-type integer
		  :specialized-array t)))))

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
