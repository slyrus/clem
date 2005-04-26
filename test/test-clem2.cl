

(in-package :clem-test)


;;; basic make-instance tests

(defun test-make-instance/integer/1 ()
  (let ((m (make-instance 'clem::integer-matrix :cols 256 :rows 256 :initial-element #xFF)))
    m))

(defun test-make-instance/bit/1 ()
  (let ((m (make-instance 'clem:bit-matrix :cols 256 :rows 256)))
    m))

(defun test-make-instance/bit/2 ()
  (let ((m (make-instance 'clem:bit-matrix :cols 256 :rows 256 :initial-element 1)))
    m))

(defun test-make-instance/unsigned-byte/1 ()
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols 256 :rows 256)))
    m))

(defun test-make-instance/unsigned-byte/2 ()
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols 256 :rows 256 :initial-element 255)))
    m))

(defun test-make-instance/signed-byte ()
  (let ((m (make-instance 'clem:signed-byte-matrix :cols 256 :rows 256)))
    m))

(defun test-make-instance/unsigned-word ()
  (let ((m (make-instance 'clem:unsigned-word-matrix :cols 256 :rows 256)))
    m))

(defun test-make-instance/signed-word ()
  (let ((m (make-instance 'clem:signed-word-matrix :cols 256 :rows 256)))
    m))

(defun test-make-instance/single-float ()
  (let ((m (make-instance 'clem:single-float-matrix :cols 256 :rows 256)))
    m))

(defun test-make-instance/double-float ()
  (let ((m (make-instance 'clem:double-float-matrix :cols 256 :rows 256)))
    m))

;;; simple arithmetic tests

(defun test-mat-add/unsigned-byte/1 ()
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols 256 :rows 256 :initial-element 128))
	(n (make-instance 'clem:unsigned-byte-matrix :cols 256 :rows 256 :initial-element 127)))
    (let ((p (clem:mat-add m n)))
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
(defun test-mat-add/unsigned-byte/2 ()
  (let ((m (make-instance 'clem:unsigned-byte-matrix :cols 256 :rows 256 :initial-element 128))
	(n (make-instance 'clem:unsigned-byte-matrix :cols 256 :rows 256 :initial-element 128)))
    (let ((p (clem:mat-add m n)))
      p)))

(defun test-mat-add/signed-byte/1 ()
  (let ((m (make-instance 'clem:signed-byte-matrix :cols 256 :rows 256 :initial-element 128))
	(n (make-instance 'clem:signed-byte-matrix :cols 256 :rows 256 :initial-element 127)))
    (let ((p (clem:mat-add m n)))
      p)))
