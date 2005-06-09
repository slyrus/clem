

(in-package :clem-test)

(defparameter *test-matrix-size* 256)

;;; basic make-instance tests

(defun test/make-instance/integer/1 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:integer-matrix :cols size :rows size :initial-element #xFF)))
    m))

(defun test/make-instance/bit/1 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:bit-matrix :cols size :rows size)))
    m))

(defun test/make-instance/bit/2 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    m))

(defun test/make-instance/unsigned-byte/1 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size)))
    m))

(defun test/make-instance/unsigned-byte/2 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 255)))
    m))

(defun test/make-instance/signed-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:signed-byte-matrix :cols size :rows size)))
    m))

(defun test/make-instance/unsigned-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-word-matrix :cols size :rows size)))
    m))

(defun test/make-instance/signed-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:signed-word-matrix :cols size :rows size)))
    m))

(defun test/make-instance/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size)))
    m))

(defun test/make-instance/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size)))
    m))

;;; simple arithmetic tests

(defun test/mat-add/unsigned-byte/unsigned-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add!/unsigned-byte/unsigned-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add! m n)))
      p)))

(defun test/mat-add/unsigned-byte/unsigned-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:unsigned-word-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add!/unsigned-byte/unsigned-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:unsigned-word-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add! m n)))
      p)))

(defun test/mat-add/unsigned-byte/unsigned-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:unsigned-long-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add!/unsigned-byte/unsigned-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:unsigned-long-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add! m n)))
      p)))

;;;
;;; What should we do about overflow?
;;; There are three obvious possibilities:
;;; 1. fit the value to the type (which is what we do now)
;;; 2. throw an error
;;; 3. promote to a type to which the value will fit
;;;
;;; I have no idea how best to deal with these three possibilities.
;;;
(defun test/mat-add/unsigned-byte/overflow (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/signed-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:signed-byte-matrix :cols size :rows size :initial-element 64))
	(n (make-instance 'clem:signed-byte-matrix :cols size :rows size :initial-element 63)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/unsigned-byte/signed-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:signed-byte-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/unsigned-byte/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/unsigned-byte/bit-2 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (time (let ((p (clem:mat-add m n)))
	    (dotimes (i 32) (clem:mat-add! p n))))))


;;; double-float tests

(defun test/mat-add/double-float/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/unsigned-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/unsigned-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:unsigned-word-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/unsigned-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:unsigned-long-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add!/double-float/unsigned-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:unsigned-long-matrix :cols size :rows size :initial-element #xffffffff)))
    (let ((p (time (clem:mat-add! m n))))
      p)))

(defun test/mat-add/double-float/signed-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:signed-byte-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/signed-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:signed-word-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/signed-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:signed-long-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/fixnum (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:fixnum-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add!/double-float/fixnum (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:fixnum-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add! m n))))
      p)))


;;; single-float tests

(defun test/mat-add/single-float/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/unsigned-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:unsigned-byte-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/unsigned-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:unsigned-word-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/unsigned-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:unsigned-long-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add!/single-float/unsigned-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:unsigned-long-matrix :cols size :rows size :initial-element #xffffffff)))
    (let ((p (time (clem:mat-add! m n))))
      p)))

(defun test/mat-add/single-float/signed-byte (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:signed-byte-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/signed-word (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:signed-word-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/signed-long (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:signed-long-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/fixnum (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:fixnum-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add!/single-float/fixnum (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:fixnum-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add! m n))))
      p)))
