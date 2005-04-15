
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
 #+sbcl
  (if (find-package 'sb-mop)
      (pushnew :clem-sbcl-mop cl:*features*)
      (pushnew :clem-sbcl-pcl cl:*features*)))

(defpackage #:clem
  (:use #:cl #:asdf #:util #+clem-sbcl-mop #:sb-mop)
  (:export
   #:matrix
   #:dim
   #:rows
   #:cols
   #:val
   #:set-val
   #:print-range
   #:print-matrix
   #:transpose
   #:mat-mult
   #:mat-copy-into
   #:mat-add
   #:mat-subtr
   #:swap-rows
   #:swap-cols
   #:map-col
   #:map-row
   #:invert-matrix

   #:horzcat
   #:vertcat
   #:pad-matrix

   #:scalar-mult-col
   #:scalar-mult-row
   #:scalar-divide-col
   #:scalar-divide-row
   #:scalar-double-float-divide-col
   #:scalar-double-float-divide-row
   #:scalar-single-float-divide-col
   #:scalar-single-float-divide-row

   #:random-matrix
   #:zero-matrix

   #:mat-square
   #:mat-square!
   #:mat-sqrt
   #:mat-sqrt!

   #:normalize
   #:norm-0-255
   #:norm-0-1

   #:map-matrix

   #:sum-range
   #:sum
   #:sum-square-range
   #:sum-square
   
   #:max-val
   #:min-val

   ;;; temporary (maybe) matrix conversion utility functions
   #:copy-to-unsigned-byte-matrix
   #:copy-to-double-float-matrix
   #:copy-to-fixnum-matrix


   ;;; matrix operations
   
   #:discrete-convolve

   #:gaussian-blur
   #:gaussian-blur-word

   #:dilate
   #:erode

   #:threshold

   #:x-derivative
   #:y-derivative
   #:gradmag

   #:variance-window
   #:sample-variance-window

   ;;; from matrixutils.cl
   #:array->matrix
   #:trim-one
   
   ;;; matrix constructor functions
   #:unsigned-byte-matrix

   ;;; typed-matrix stuff
   #:typed-matrix
   #:map-matrix-fit

   #:defmatrixtype

   #:signed-byte-matrix
   #:array->signed-byte-matrix
   #:random-signed-byte-matrix

   #:unsigned-byte-matrix
   #:array->unsigned-byte-matrix
   #:random-unsigned-byte-matrix

   #:signed-word-matrix
   #:array->signed-word-matrix
   #:random-signed-word-matrix

   #:unsigned-word-matrix
   #:array->unsigned-word-matrix
   #:random-unsigned-word-matrix

   #:signed-long-matrix
   #:array->signed-long-matrix
   #:random-signed-long-matrix

   #:unsigned-long-matrix
   #:array->unsigned-long-matrix
   #:random-unsigned-long-matrix

   #:fixnum-matrix
   #:array->fixnum-matrix
   #:random-fixnum-matrix

   #:single-float-matrix
   #:array->single-float-matrix
   #:random-single-float-matrix

   #:double-float-matrix
   #:array->double-float-matrix
   #:random-double-float-matrix

   #:t-matrix
   #:array->t-matrix
   #:random-t-matrix

   #:bit-matrix
   #:array->bit-matrix
   #:random-bit-matrix
   ))

