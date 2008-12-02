
(asdf:defsystem :clem-test
  :name "clem-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :depends-on (ch-util clem)
  :components
  ((:module :test
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "test-clem" :depends-on ("defpackage"))
	     (:cl-source-file "test-clem2" :depends-on ("defpackage"))
	     (:cl-source-file "test-clem3" :depends-on ("defpackage"))
	     (:cl-source-file "test-defmatrix" :depends-on ("defpackage"))
	     (:cl-source-file "test-transform" :depends-on ("defpackage"))
	     (:cl-source-file "test-convolve" :depends-on ("defpackage"))
	     (:cl-source-file "bench-matrix" :depends-on ("defpackage"))
	     (:cl-source-file "test-hprod" :depends-on ("defpackage"))))))

