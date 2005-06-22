
(defpackage #:clem-test-system (:use #:asdf #:cl))
(in-package #:clem-test-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :clem-test-cl-source-file
;;;;
(defclass clem-test-cl-source-file (cl-source-file) ())

(defmethod source-file-type ((c clem-test-cl-source-file) (s module)) "cl")

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod asdf::output-files :around ((operation compile-op) (c clem-test-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))


;;;; 2005-06-14
;;;; hackery for putting stuff in the asdf registry
;;;; there has to be a better way to do this. I don't know
;;;; it.
(defvar *registry-directories*
  (list (make-pathname :directory "/usr/local/share/lisp")
        (make-pathname :directory "/bobo/share/lisp")
	(merge-pathnames
	 (make-pathname :directory (list :relative :up))
	 (make-pathname :directory (pathname-directory *load-truename*)))))

(defun add-registry-path (path)
  (dolist (dir *registry-directories*)
    (let ((p (merge-pathnames
              (make-pathname :directory (cons :relative (if (not (listp path)) (list path) path)))
              dir)))
      (when (probe-file p)
        (pushnew p asdf:*central-registry* :test 'equal)
        (return-from add-registry-path p)))))

(mapcar #'(lambda (x) (add-registry-path x))
	'("util"))
;;;;



(defsystem :clem-test
  :version "20050614.1"
  :depends-on (util clem)
  :components
  ((:module :test
	    :components
	    ((:clem-test-cl-source-file "defpackage")
	     (:clem-test-cl-source-file "test-clem" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-clem2" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-clem3" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-defmatrix" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "bench-matrix" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-hprod" :depends-on ("defpackage"))
	     ))))

