
(in-package :clem)

(defun %mat-mult-3-df-bogus (a b c
		      mstartr mendr mstartc mendc
		      nstartr nendr nstartc nendc)
  (declare (type fixnum mstartr mendr mstartc mendc
		 nstartr nendr nstartc nendc)
	   (dynamic-extent mstartr mendr mstartc mendc
			   nstartr nendr nstartc nendc)
	   (type (simple-array double-float (* *)) a b c)
	   (optimize (speed 3) (safety 0)))
  (when (eql (- mendc mstartc) (- nendr nstartr))
    (let ((ai 0) (ci 0) (atemp 0d0) (ctemp 0d0))
      (declare (type fixnum ai ci)
	       (type double-float atemp ctemp)
	       (dynamic-extent ai ci atemp ctemp))
      (do ((k mstartc (1+ k)))
	  ((> k mendc))
	(declare (type fixnum k) (dynamic-extent k))
	(setf ci 0)
	(do ((i mstartr (1+ i)))
	    ((> i mendr))
	  (declare (type fixnum i) (dynamic-extent i))
	  (setf ai (array-row-major-index a i k)
		atemp (row-major-aref a ai)
		ctemp 0d0)
	  (do ((j nstartc (1+ j)))
	      ((> j nendc))
	    (declare (type fixnum j) (dynamic-extent j))
	    (setf (row-major-aref c ci) (+ (row-major-aref c ci) (the double-float (* atemp (aref b k j)))))
	    (incf ci)))))))



(defun %mat-mult-3-sf (a b c
		      mstartr mendr mstartc mendc
		      nstartr nendr nstartc nendc)
  (declare (type fixnum mstartr mendr mstartc mendc
		 nstartr nendr nstartc nendc)
	   (dynamic-extent mstartr mendr mstartc mendc
			   nstartr nendr nstartc nendc)
	   (type (simple-array single-float (* *)) a b c)
	   (optimize (speed 3) (safety 0)))
  (when (eql (- mendc mstartc) (- nendr nstartr))
    (let ((ai 0) (ci 0) (atemp 0s0) (ctemp 0s0))
      (declare (type fixnum ai ci)
	       (type single-float atemp ctemp)
	       (dynamic-extent ai ci atemp ctemp))
      (do ((k mstartc (1+ k)))
	  ((> k mendc))
	(declare (type fixnum k) (dynamic-extent k))
	(setf ci 0)
	(do ((i mstartr (1+ i)))
	    ((> i mendr))
	  (declare (type fixnum i) (dynamic-extent i))
	  (setf ai (array-row-major-index a i k)
		atemp (row-major-aref a ai)
		ctemp 0s0)
	  (do ((j nstartc (1+ j)))
	      ((> j nendc))
	    (declare (type fixnum j) (dynamic-extent j))
	    (setf (row-major-aref c ci) (+ (row-major-aref c ci) (the single-float (* atemp (aref b k j)))))
	    (incf ci)))))))

(defun mat-mult-3-sf (m n p
		      mstartr mendr mstartc mendc
		      nstartr nendr nstartc nendc)
  (when (eql (- mendc mstartc) (- nendr nstartr))
    (%mat-mult-3-sf (clem::matrix-vals m) (clem::matrix-vals n) (clem::matrix-vals p)
		    mstartr mendr mstartc mendc
		    nstartr nendr nstartc nendc)
    p))


(defun mat-mult-df (m n
		       mstartr mendr mstartc mendc
		       nstartr nendr nstartc nendc)
  (let ((mr (- mendr mstartr))
	(mc (- mendc mstartc))
	(nr (- nendr nstartr))
	(nc (- nendc nstartc)))
    (declare (type fixnum mr mc nr nc))
    (when (eql mc nr)
      (let ((p (make-instance 'double-float-matrix :rows (1+ mr) :cols (1+ nc))))
;;	(declare (optimize (speed 3) (safety 0)))

	(let ((a (clem::matrix-vals m))
	      (b (clem::matrix-vals n))
	      (c (clem::matrix-vals p)))
	  (declare (type (simple-array double-float (* *)) a))
	  (declare (type (simple-array double-float (* *)) b))
	  (declare (type (simple-array double-float (* *)) c))
	  
	  (do ((k 0 (1+ k)))
	      ((> k mc))
	    (declare (type fixnum k))
	    (do ((i 0 (1+ i)))
		((> i mr))
	      (declare (type fixnum i))
	      (do ((j 0 (1+ j)))
		  ((> j nc))
		(declare (type fixnum j))
		(incf (aref c i j) (* (aref a i k) (aref b k j)))))))
	p))))

(defun %mat-mult-3-df-bad-cache (a b c
		      mstartr mendr mstartc mendc
		      nstartr nendr nstartc nendc)
  (declare (type fixnum mstartr mendr mstartc mendc
		 nstartr nendr nstartc nendc)
	   (type (simple-array double-float (* *)) a b c)
	   (optimize (speed 3) (safety 0)))
  (when (eql (- mendc mstartc) (- nendr nstartr))
    (let ((acc 0d0))
      (declare (type double-float acc))
      (do ((i mstartr (1+ i)))
	  ((> i mendr))
	(declare (type fixnum i))
	(do ((j nstartc (1+ j)))
	    ((> j nendc))
	  (declare (type fixnum j))
	  (do ((k mstartc (1+ k)))
	      ((> k mendc))
	    (declare (type fixnum k))
	    (incf acc (the double-float (* (aref a i k) (aref b k j)))))
	  (setf (aref c i j) acc)
	  (setf acc 0d0))))))

(defun %mat-mult-3-df (a b c
		       mstartr mendr mstartc mendc
		       nstartr nendr nstartc nendc)
  (declare (type fixnum mstartr mendr mstartc mendc
		 nstartr nendr nstartc nendc)
	   (type (simple-array double-float (* *)) a b c)
	   (optimize (speed 3) (safety 0) (space 0)))
  (when (eql (- mendc mstartc) (- nendr nstartr))
    (let ((atemp 0d0))
      (declare (type double-float atemp))
      (do ((i mstartr (1+ i)))
	  ((> i mendr))
	(declare (type fixnum i))
	(do ((k mstartc (1+ k)))
	    ((> k mendc))
	  (declare (type fixnum k))
	  (setf atemp (aref a i k))
	  (do ((j nstartc (1+ j)))
	      ((> j nendc))
	    (declare (type fixnum j))
	    (incf (aref c i j) (the double-float (* atemp (aref b k j))))))))))

(defun %mat-mult-3-df-row-major (a b c
		      mstartr mendr mstartc mendc
		      nstartr nendr nstartc nendc)
  (declare (type fixnum mstartr mendr mstartc mendc
		 nstartr nendr nstartc nendc)
	   (type (simple-array double-float (* *)) a b c)
	   (optimize (speed 3) (safety 0) (space 0)))
  (when (eql (- mendc mstartc) (- nendr nstartr))
    (let ((ci 0) (atemp 0d0))
      (declare (type fixnum ci)
	       (type double-float atemp))
      (do ((k mstartc (1+ k)))
	  ((> k mendc))
	(declare (type fixnum k))
	(setf ci 0)
	(do ((i mstartr (1+ i)))
	    ((> i mendr))
	  (declare (type fixnum i))
	  (setf atemp (aref a i k))
	  (do ((j nstartc (1+ j)))
	      ((> j nendc))
	    (declare (type fixnum j))
	    (incf (row-major-aref c ci) (the double-float (* (aref b k j) atemp)))
	    (incf ci)))))))

(defparameter *max-block-size* 32)
(declaim (type fixnum *max-block-size*))

(defun %mat-mult-3-df-with-blocks (a b c
				   mstartr mendr mstartc mendc
				   nstartr nendr nstartc nendc)
  (declare (type fixnum mstartr mendr mstartc mendc
		 nstartr nendr nstartc nendc)
	   (type (simple-array double-float (* *)) a b c)
	   (optimize (speed 3) (safety 0)))
  (if (and (> (- mendr mstartr) *max-block-size*)
	   (> (- mendc mstartc) *max-block-size*)
	   (> (- nendr nstartr) *max-block-size*)
	   (> (- nendc nstartc) *max-block-size*))
      (let ((mblock-row-end (+ mstartr (ash (- mendr mstartr -1) -1) -1))
	    (mblock-col-end (+ mstartc (ash (- mendc mstartc -1) -1) -1))
	    (nblock-row-end (+ nstartr (ash (- nendr nstartr -1) -1) -1))
	    (nblock-col-end (+ nstartc (ash (- nendc nstartc -1) -1) -1)))

	;;; m_11 * n_11
	(%mat-mult-3-df-with-blocks a b c
		       mstartr mblock-row-end
		       mstartc mblock-col-end
		       nstartr nblock-row-end
		       nstartc nblock-col-end)

	;;; m_11 * n_12
	(%mat-mult-3-df-with-blocks a b c
		       mstartr mblock-row-end
		       mstartc mblock-col-end
		       nstartr nblock-row-end
		       (1+ nblock-col-end) nendc)

	;;; m_21 * n_11
	(%mat-mult-3-df-with-blocks a b c
		       (1+ mblock-row-end) mendr
		       mstartc mblock-col-end
		       nstartr nblock-row-end
		       nstartc nblock-col-end)

 	;;; m_21 * n_12
	(%mat-mult-3-df-with-blocks a b c
			(1+ mblock-row-end) mendr
			mstartc mblock-col-end
			nstartr nblock-row-end
			(1+ nblock-col-end) nendc)

	;;; m_12 * n_21
	(%mat-mult-3-df-with-blocks a b c
		       mstartr mblock-row-end
		       (1+ mblock-col-end) mendc
		       (1+ nblock-row-end) nendr
		       nstartc nblock-col-end)

	;;; m_12 * n_22
	(%mat-mult-3-df-with-blocks a b c
		       mstartr mblock-row-end
		       (1+ mblock-col-end) mendc
		       (1+ nblock-row-end) nendr
		       (1+ nblock-col-end) nendc)

 	;;; m_22 * n_21
	(%mat-mult-3-df-with-blocks a b c
		       (1+ mblock-row-end) mendr
		       (1+ mblock-col-end) mendc
		       (1+ nblock-row-end) nendr
		       nstartc nblock-col-end)

	;;; m_22 * n_22
	(%mat-mult-3-df-with-blocks a b c
			(1+ mblock-row-end) mendr
			(1+ mblock-col-end) mendc
			(1+ nblock-row-end) nendr
			(1+ nblock-col-end) nendc)

	)
      (progn
	(%mat-mult-3-df-row-major a b c
			mstartr mendr
			mstartc mendc
			nstartr nendr
			nstartc nendc))))

(defun mat-mult-3-df (m n p
		      mstartr mendr mstartc mendc
		      nstartr nendr nstartc nendc)
  (when (eql (- mendc mstartc) (- nendr nstartr))
    ;;    (%mat-mult-3-df (clem::matrix-vals m) (clem::matrix-vals n) (clem::matrix-vals p)
    (%mat-mult-3-df-with-blocks (clem::matrix-vals m) (clem::matrix-vals n) (clem::matrix-vals p)
			 mstartr mendr mstartc mendc
			 nstartr nendr nstartc nendc)
    p))
