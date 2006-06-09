(#.
 (cons
  :p
  (progn
    (in-package :clem)
    (defparameter smarkup::*document-thesis* nil)
    (smarkup::setup-headings)
    nil))
 
 (:smarkup-metadata
  (:copyright
   "Copyright 2006, Cyrus Harmon. All Rights Reserved.")
  (:title "clem: A common-lisp matrix package")
  (:author "Cyrus L. Harmon")
  (:bibtex-database
   "(\"asdf:/ch-bib/lisp\" \"asdf:/ch-bib/bio\")")
  (:bibtex-style "Science"))
 (:html-metadata (:htmlcss "simple.css") )
 
 (:lisp-silent 
  "(asdf:operate 'asdf:load-op 'clem)"
  "(setf smarkup::*baseline-skip* \"14pt\")"
  "(setf smarkup::*par-skip* \"0pt\")")
 
 (:h1 "Abstract")
 
 (:p "CLEM is an open-source Common Lisp library for the
 representation and manipulation of matrices. CLEM is designed to
 be a flexible and extensible system for the representation of
 arbitrary 2-dimensional matrices.")
 
 (:h1 "Introduction")

 (:p "The Common Lisp language"
     (:bibcite "steele1990common")
     " offers a rich, dynamic environment for programming and
data analysis. Common Lisp contains a powerful object system, the
Common Lisp Object System (CLOS)"
     (:BIBCITE "keene89object")
     ", and most modern implementations support a protocol for
the generation not just of new classes and objects, but to extend
the object system itself using the Meta-object Protocol"
     (:BIBCITE "kiczales91art")
     ".")

 (:P "CLEM uses CLOS and the Meta-object protocol (MOP) to define a"
     (:CODE "standard-matrix-class")
     " that serves as the metaclass for classes that represent
matrices with elements of specific types. The typed matrices can
represent matrices containing values of specific types in the
Common Lisp type system, starting with type "
     (:CODE "t")
     " as the most general data type, and becoming more restrictive by using more specific types such"
     (:CODE "double-float")
     ", "
     (:CODE "fixnum")
     ", or "
     (:CODE "(unsigned-byte 8)")
     ". By using the most specific type that can represent the values of a given matrix, the lisp system can optimize for better performance and memory usage requirements. For example, a"
     (:CODE "bit-matrix")
     " will use 1 bit per matrix element, rather than 32-bits on 32-bit systems for a "
     (:CODE "t-matrix")
     ".")

 (:H1 "Defining CLEM Classes and Making CLEM Instances")

 (:H2 "Creating CLEM Instances with make-instance")

 (:P "The following code creates a 16-row by 16-column matrix of type"
     (:CODE "double-float-matrix")
     " and assigns it to the dynamic variable"
     (:CODE "*m1*")
     ".")
 (:LISP 
  "(defparameter *m1*
 (make-instance 'clem:double-float-matrix :rows 16 :cols 16))"
  "*m1*")

 (:P "The default is to only show the first 7 and the last rows
 and columns of each matrix. The number of rows and columns can
 be changed by setting the "
     (:CODE "*matrix-print-row-limit*")
     " and"
     (:CODE "*matrix-print-col-limit*")
     " variables.")

 (:H2 "standard-matrix-class")
 (:H2 "CLEM Matrix Types")

 (:H3 "Number matrices")

 (:P "The most general class of numerical matrix is the number matrix.")

 (:H3 "Integer Matrices")

 (:H3 "Floating-point Matrices")

 (:H3 "Complex-value Matrices")

 (:H1 "Working with CLEM Matrices")

 (:H2 "Matrix Dimensions and Values")

 (:H2 "Typed matrix operations")

 (:H2 "Matrix Copying")

 (:H2 "matrix-move")

 (:H1 "Matrix Arithmetic")

 (:H2 "Matrix Addition and Subtraction")

 (:H2 "Matrix Multiplication")

 (:H2 "Hadamard Product")

 (:H2 "Scalar Arithmetic")

 (:H2 "Other Mathematical Functions")

 (:P "Discuss mat-log, mat-abs, min, and max.")

 (:H1 "Matrix Operations")

 (:H2 "Matrix Inversion")

 (:H2 "Matrix Normalization")

 (:H2 "Discrete Convolution")

 (:H3 "Derivatives")

 (:H3 "Gradient Magnitude")

 (:H3 "Gaussian Blur")

 (:H2 "Affine Transformations")

 (:H3 "Interpolation")

 (:H2 "Morphological Operations")

 (:H3 "Dilation and Erosion")

 (:H3 "Variance")

 (:H3 "Thresholding")

 (:H1 "CLEM Implementation Details")

 (:H2 "Type-specific matrix functions")

 (:P "The general strategy has been to 1) make things work and
 then make them work quickly.  To this end, I have been writing
 functions for matrix operations in a general manner first and
 then recoding type-specific versions to make certain operations
 go faster.  This is done via liberal use of macros to generate
 type-specific functions and methods for matrix operations that
 go much faster than the general versions.")

 (:P "The convention is that a generic function such as sum-range
 will have a generic version that works with all matrices and
 type specific versions thaqt work with specific matrices. g In
 order to support these functions there may be internal methods,
 prefixed with a %, that implement certain type-specific
 functionality.  Macros that generate the code used for the
 type-specific methods will be prefixed with a %%.  In theory,
 the %%-macros can be called from other code that need to
 generate in-place code where the overhead of the method-call to
 the %-method would be too expensive. This convention is not yet
 widely enforced and certainly untested. Hopefully this situation
 will improve.")

 (:H2 "Hacking the SBCL compiler to improve performance")

 (:BIBLIOGRAPHY))
