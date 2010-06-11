
(asdf:defsystem :clem-test
  :name "clem-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.4.8"
  :depends-on (clem)
  :components
  ((:module :test
	    :components
	    ((:cl-source-file "defpackage")
             (:cl-source-file "testharness" :depends-on ("defpackage"))
	     (:cl-source-file "test-clem" :depends-on ("defpackage" "testharness"))
	     (:cl-source-file "test-clem2" :depends-on ("defpackage" "testharness"))
	     (:cl-source-file "test-clem3" :depends-on ("defpackage" "testharness"))
	     (:cl-source-file "test-defmatrix" :depends-on ("defpackage" "testharness"))
	     (:cl-source-file "test-transform" :depends-on ("defpackage" "testharness"))
	     (:cl-source-file "test-convolve" :depends-on ("defpackage" "testharness"))
	     (:cl-source-file "bench-matrix" :depends-on ("defpackage" "testharness"))
	     (:cl-source-file "test-hprod" :depends-on ("defpackage" "testharness"))))))

