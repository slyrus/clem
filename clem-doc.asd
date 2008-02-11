
(asdf:operate 'asdf:load-op :ch-asdf)
(asdf:operate 'asdf:load-op :smarkup)

(defpackage #:clem-doc-system (:use #:cl #:asdf #:ch-asdf #:smarkup))
(in-package #:clem-doc-system)

#.(smarkup::enable-quote-reader-macro)

(defsystem :clem-doc
  :name "clem-doc"
  :author "Cyrus Harmon" 
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (ch-asdf ch-bib ch-util clem smarkup)
  :components
  ((:static-file "make-tinaa-docs" :pathname #p"make-tinaa-docs.lisp")
   (:module
    :doc
    :components
    ((:object-from-file :clem-sexp
                        :pathname #p"clem.sexp")
     (:filtered-object :clem-filtered-sexp
                       :filters (:lisp :smarkup-metadata :ref)
                       :depends-on (:clem-sexp)
                       :input-object :clem-sexp)

     (:filtered-object :clem-html-filtered-sexp
                       :filters (:html-metadata)
                       :depends-on (:clem-filtered-sexp)
                       :input-object :clem-filtered-sexp)

     (:filtered-object :clem-pdf-filtered-sexp
                       :filters (:html-metadata)
                       :depends-on (:clem-filtered-sexp)
                       :input-object :clem-filtered-sexp)

     (:object-cl-pdf-file :clem-pdf
                          :pathname #p"clem.pdf"
                          :depends-on (:clem-pdf-filtered-sexp)
                          :input-object :clem-pdf-filtered-sexp)

     (:object-xhtml-file :clem-xhtml
                         :pathname #p"clem.xhtml"
                         :depends-on (:clem-html-filtered-sexp)
                         :input-object :clem-html-filtered-sexp)

     (:object-from-file :clem-performance-sexp
                        :pathname #p"clem-performance.sexp")
     (:filtered-object :clem-performance-filtered-sexp
                       :filters (:lisp :smarkup-metadata :ref)
                       :depends-on (:clem-performance-sexp)
                       :input-object :clem-performance-sexp)
     (:object-cl-pdf-file :clem-performance-pdf
                          :pathname #p"clem-performance.pdf"
                          :depends-on (:clem-performance-filtered-sexp)
                          :input-object :clem-performance-filtered-sexp)
     
     (:filtered-object :clem-performance-html-filtered-sexp
                       :filters (:html-metadata)
                       :depends-on (:clem-performance-filtered-sexp)
                       :input-object :clem-performance-filtered-sexp)
     (:object-xhtml-file :clem-performance-xhtml
                         :pathname #p"clem-performance.xhtml"
                         :depends-on (:clem-performance-html-filtered-sexp)
                         :input-object :clem-performance-html-filtered-sexp)

     (:css-file :simple)
     (:tinaa-directory :tinaa)))))

