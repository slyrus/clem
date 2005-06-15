

(in-package :clem-test)

;;; I need *my-pacakge* so the intern statement below doesn't intern
;;; the symbol into CL-USER and I put it up here to remind myself of
;;; this fact in case I switch packages
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *my-package* :clem-test))

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

(defun test/make-instance/ub8/1 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size)))
    m))

(defun test/make-instance/ub8/2 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 255)))
    m))

(defun test/make-instance/sb8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb8-matrix :cols size :rows size)))
    m))

(defun test/make-instance/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub16-matrix :cols size :rows size)))
    m))

(defun test/make-instance/sb16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb16-matrix :cols size :rows size)))
    m))

(defun test/make-instance/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size)))
    m))

(defun test/make-instance/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size)))
    m))

;;; simple arithmetic tests

(defun test/mat-add/ub8/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add-inline/ub8/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem::mat-add-inline m n))))
      p)))

(defun test/mat-add!/ub8/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add! m n)))
      p)))

(defun test/mat-add!-inline/ub8/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 127)))
    (declare (inline clem::mat-add!-inline))
    (let ((p (clem::mat-add!-inline m n)))
      p)))

(defun test/mat-add/ub8/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add-inline/ub8/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem::mat-add-inline m n))))
      p)))

(defun test/mat-add!/ub8/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add! m n)))
      p)))

(defun test/mat-add!-inline/ub8/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem::mat-add!-inline m n))))
      p)))

(defun test/mat-add/ub8/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add!/ub8/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 127)))
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
(defun test/mat-add/ub8/overflow (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/sb8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb8-matrix :cols size :rows size :initial-element 64))
	(n (make-instance 'clem:sb8-matrix :cols size :rows size :initial-element 63)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/ub8/sb8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:sb8-matrix :cols size :rows size :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/ub8/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test/mat-add/ub8/bit-2 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (time (let ((p (clem:mat-add m n)))
	    (dotimes (i 32) (clem:mat-add! p n))))))


;;; double-float tests

(defun test/mat-add/double-float/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add-inline/double-float/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (time (clem::mat-add-inline m n))))
      p)))

(defun test/mat-add!/double-float/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (clem:mat-add! m n)))
      p)))

(defun test/mat-add!-inline/double-float/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (clem::mat-add!-inline m n)))
      p)))

(defun test/mat-add/double-float/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add!/double-float/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element #xffffffff)))
    (let ((p (time (clem:mat-add! m n))))
      p)))

(defun test/mat-add/double-float/sb8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:sb8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/sb16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:sb16-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/double-float/sb32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0))
	(n (make-instance 'clem:sb32-matrix :cols size :rows size :initial-element 1)))
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

(defun test/mat-add/single-float/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add!/single-float/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element #xffffffff)))
    (let ((p (time (clem:mat-add! m n))))
      p)))

(defun test/mat-add/single-float/sb8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:sb8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/sb16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:sb16-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add/single-float/sb32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0))
	(n (make-instance 'clem:sb32-matrix :cols size :rows size :initial-element 1)))
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



(defun test/mat-add/ub32/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem:mat-add m n))))
      p)))

(defun test/mat-add-inline/ub32/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem::mat-add-inline m n))))
      p)))

(defun test/mat-add!/ub32/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem:mat-add! m n))))
      p)))

(defun test/mat-add!-inline/ub32/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 128))
	(n (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem::mat-add!-inline m n))))
      p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mat-scale
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test/mat-scale/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (time (clem::mat-scale m 2.0d0))))
      p)))

(defun test/mat-scale/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0)))
    (let ((p (time (clem::mat-scale m 2.0s0))))
      p)))

(defun test/mat-scale/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem::mat-scale m 2))))
      p)))

(defun test/mat-scale/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 255)))
    (let ((p (time (clem::mat-scale m 255))))
      p)))

(defun test/mat-scale/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 256)))
    (let ((p (time (clem::mat-scale m 256))))
      p)))


(defun test/mat-scale/sb8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem::mat-scale m 2))))
      p)))

(defun test/mat-scale/sb16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb16-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem::mat-scale m 127))))
      p)))

(defun test/mat-scale/sb32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb32-matrix :cols size :rows size :initial-element 256)))
    (let ((p (time (clem::mat-scale m 256))))
      p)))

(defun test/mat-scale/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem::mat-scale m 0))))
      p)))

(defun test/mat-scale/fixnum (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:fixnum-matrix :cols size :rows size :initial-element 256)))
    (let ((p (time (clem::mat-scale m 256))))
      p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mat-scale!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test/mat-scale!/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.0d0)))
    (let ((p (time (clem::mat-scale! m 2.0d0))))
      p)))

(defun test/mat-scale!/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.0s0)))
    (let ((p (time (clem::mat-scale! m 2.0s0))))
      p)))

(defun test/mat-scale!/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem::mat-scale! m 2))))
      p)))

(defun test/mat-scale!/ub16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub16-matrix :cols size :rows size :initial-element 255)))
    (let ((p (time (clem::mat-scale! m 255))))
      p)))

(defun test/mat-scale!/ub32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub32-matrix :cols size :rows size :initial-element 256)))
    (let ((p (time (clem::mat-scale! m 256))))
      p)))


(defun test/mat-scale!/sb8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb8-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem::mat-scale! m 2))))
      p)))

(defun test/mat-scale!/sb16 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb16-matrix :cols size :rows size :initial-element 127)))
    (let ((p (time (clem::mat-scale! m -127))))
      p)))

(defun test/mat-scale!/sb32 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:sb32-matrix :cols size :rows size :initial-element 256)))
    (let ((p (time (clem::mat-scale! m 256))))
      p)))

(defun test/mat-scale!/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem::mat-scale! m 0))))
      p)))

(defun test/mat-scale!/fixnum (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:fixnum-matrix :cols size :rows size :initial-element 256)))
    (let ((p (time (clem::mat-scale! m 256))))
      p)))


;;;; hprod test

;;; double-floats
(defun test/mat-hprod/double-float/double-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.25d0))
	(n (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.25d0)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))

(defun test/mat-hprod/double-float/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.25d0))
	(n (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.25s0)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))

(defun test/mat-hprod/double-float/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.25d0))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 2)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))

(defun test/mat-hprod/double-float/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:double-float-matrix :cols size :rows size :initial-element 1.25d0))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 0)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))



;;; single-floats
(defun test/mat-hprod/single-float/single-float (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.25s0))
	(n (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.25s0)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))

(defun test/mat-hprod/single-float/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.25s0))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 2)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))

(defun test/mat-hprod/single-float/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:single-float-matrix :cols size :rows size :initial-element 1.25s0))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 0)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))


;;; ub8's
(defun test/mat-hprod/ub8/ub8 (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 12))
	(n (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 2)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))

(defun test/mat-hprod/ub8/bit (&key (size *test-matrix-size*))
  (let ((m (make-instance 'clem:ub8-matrix :cols size :rows size :initial-element 12))
	(n (make-instance 'clem:bit-matrix :cols size :rows size :initial-element 1)))
    (let ((p (time (clem::mat-hprod m n))))
      p)))


(macrolet ((frob (type-1 val-1 type-2 val-2)
	     (let ((funcname (intern
			      (string-upcase
			       (concatenate 'string "test/mat-hprod/"
					    (symbol-name type-1)
					    "/"
					    (symbol-name type-2)))
			      *my-package*))
		   (m1 (intern (string-upcase (concatenate 'string (symbol-name type-1) "-matrix"))))
		   (m2 (intern (string-upcase (concatenate 'string (symbol-name type-2) "-matrix")))))
	       `(defun ,funcname (&key (size *test-matrix-size*))
		  (let ((m (make-instance ',m2
					  :cols size :rows size
					  :initial-element ,val-1))
			(n (make-instance ',m1
					  :cols size :rows size
					  :initial-element ,val-2)))
		    (let ((p (time (clem::mat-hprod m n))))
		      p))))))
  (frob double-float 1.25d0 double-float 1.25d0))
	   