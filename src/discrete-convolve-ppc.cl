
(in-package :clem)

#+openmcl
(defmethod discrete-convolve-ppc ((uin matrix) (vin matrix) &key (truncate) (norm-v t))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((u (if (equal (storage-type uin) 'double-float) uin (copy-to-double-float-matrix uin)))
	(v (if (equal (storage-type vin) 'double-float) vin (copy-to-double-float-matrix vin))))
  ;;; ur, uc, vr, vc are the number of rows and columns in u and v
  (destructuring-bind (ur uc) (dim u)
    (declare (dynamic-extent ur uc) (fixnum ur uc))
;    (print (list ur uc))
    (destructuring-bind (vr vc) (dim v)
      (declare (fixnum vr vc) (dynamic-extent vr vc))
;      (print (list vr vc))
      ;;; need a new matrix z to hold the values of the convolved matrix
      ;;; dim z should be dim u + dim v - 1
      (let ((zr (+ ur vr (- 1)))
	    (zc (+ uc vc (- 1))))
	(declare (fixnum zr zc) (dynamic-extent zr zc))
	(let ((z (make-instance (class-of u) :rows zr :cols zc))
	      (uval (matrix-vals u))
	      (vval (matrix-vals v))
	      (vsum (sum v)))
	  (dotimes (i zr)
	    (let ((ustartr (max 0 (- i vr -1)))
		  (uendr (min (- ur 1) i))
		  (vstartr (- vr (max (- vr i) 1)))
		  (vendr (- vr (min (- zr i) vr))))
	      (dotimes (j zc)
		(let ((ustartc (max 0 (- j vc -1)))
		      (uendc (min (- uc 1) j))
		      (vstartc (- vc (max (- vc j) 1)))
		      (vendc (- vc (min (- zc j) vc)))
		      (acc 0d0))
		  (let ((normval (if (and norm-v (or (not (= vendr vendc 0))
						     (< vstartr (- vr 1))
						     (< vstartc (- vc 1))))
				     (let ((rsum (sum-range v vendr vstartr vendc vstartc)))
				       (if (not (= rsum 0))
					   (/ vsum rsum)
					 0))
				   nil)))

		    (CCL::WITH-TEMP-DOUBLE-FLOATS (temp!)
						  (CCL::%SET-DOUBLE! temp! 0)
		    (do ((urow ustartr (1+ urow))
			 (vrow vstartr (1- vrow)))
			((> urow uendr))
		      (declare (fixnum urow vrow))
		      (declare (dynamic-extent urow vrow))
		      (do ((ucol ustartc (1+ ucol))
			   (vcol vstartc (1- vcol)))
			  ((> ucol uendc))
			(declare (fixnum ucol vcol))
			(declare (dynamic-extent ucol vcol))
			(let ((uv (val u urow ucol))
			      (vv (if (val v vrow vcol))))
			  (if normval
			      (CCL::%SET-DOUBLE! temp! (+ temp! (* uv vv normval)))
			    (CCL::%SET-DOUBLE! temp! (+ temp! (* uv vv)))))))
		    (setf acc (CCL::%COPY-FLOAT temp!)))
		    (if truncate
			(set-val z i j (fit-unsigned-byte acc))
		      (set-val z i j acc)))))))
	  z))))))
