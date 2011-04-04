
(asdf:operate 'asdf:load-op :asdf-objects)
(asdf:operate 'asdf:load-op :smarkup)

(defpackage #:clem-doc-system (:use #:cl #:asdf #:asdf-objects #:smarkup))
(in-package #:clem-doc-system)

(defclass clem-mixin () ())

(defmethod perform ((op compile-op) (c clem-mixin))
  (let ((*default-pathname-defaults*
         (component-pathname (component-parent c))))
    (call-next-method)))

(defmethod perform ((op load-op) (c clem-mixin))
  (let ((*default-pathname-defaults*
         (component-pathname (component-parent c))))
    (call-next-method)))

(defmethod perform ((op asdf-objects:generate-op) (c clem-mixin))
  (let ((*default-pathname-defaults*
         (component-pathname (component-parent c))))
    (call-next-method)))

(defclass my-filtered-object (clem-mixin smarkup:filtered-object) ())

(defclass my-object-latex-file (clem-mixin smarkup:object-latex-file) ())

(defclass my-object-cl-pdf-file (clem-mixin smarkup:object-cl-pdf-file) ())

(defsystem :clem-doc
  :name "clem-doc"
  :author "Cyrus Harmon" 
  :version "0.4.8"
  :licence "BSD"
  :depends-on (asdf-objects clem smarkup)
  :components
  ((:static-file "make-tinaa-docs" :pathname #p"make-tinaa-docs.lisp")
   (:module
    :doc
    :components
    ((:smarkup-object-from-file :clem-sexp
                                :pathname #p"clem.sexp")
     (:my-filtered-object :clem-filtered-sexp
                          :filters (:lisp :smarkup-metadata :ref)
                          :depends-on (:clem-sexp)
                          :input-object :clem-sexp)
     
     (:my-filtered-object :clem-html-filtered-sexp
                          :filters (:html-metadata)
                          :depends-on (:clem-filtered-sexp)
                          :input-object :clem-filtered-sexp)
     
     (:my-filtered-object :clem-pdf-filtered-sexp
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

     (:smarkup-object-from-file :clem-performance-sexp
                                :pathname #p"clem-performance.sexp")
     (:my-filtered-object :clem-performance-filtered-sexp
                          :filters (:lisp :smarkup-metadata :ref)
                          :depends-on (:clem-performance-sexp)
                          :input-object :clem-performance-sexp)
     (:object-cl-pdf-file :clem-performance-pdf
                          :pathname #p"clem-performance.pdf"
                          :depends-on (:clem-performance-filtered-sexp)
                          :input-object :clem-performance-filtered-sexp)
     
     (:my-filtered-object :clem-performance-html-filtered-sexp
                          :filters (:html-metadata)
                          :depends-on (:clem-performance-filtered-sexp)
                          :input-object :clem-performance-filtered-sexp)
     (:object-xhtml-file :clem-performance-xhtml
                         :pathname #p"clem-performance.xhtml"
                         :depends-on (:clem-performance-html-filtered-sexp)
                         :input-object :clem-performance-html-filtered-sexp)

     (:css-file :simple)
     (:static-file "clem-bib" :pathname #p"clem.bib")
     (:module :tinaa)))))

