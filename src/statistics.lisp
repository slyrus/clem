;;; statistics.lisp
;;; macros, functions and methods for matrix element access
;;;
;;; Copyright (c) 2004-2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :clem)

;;; slow fallback methods

(defmethod mean-range ((m matrix) startr endr startc endc)
  (double-float-divide (sum-range m startr endr startc endc)
		(count-range startr endr startc endc)))

(defmethod mean ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (mean-range m 0 (- mr 1) 0 (- mc 1))))

(defmethod variance-range ((m matrix) startr endr startc endc)
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((mu (mean-range m startr endr startc endc)))
    (let ((musq (* mu mu)))
      (let ((ssr (sum-square-range m startr endr startc endc)))
	(let ((cr (count-range startr endr startc endc)))
	  (declare (fixnum cr))
	  (- (double-float-divide ssr cr)
	     musq))))))

(defmethod variance ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (variance-range m 0 (- mr 1) 0 (- mc 1))))

(defmethod sample-variance-range ((m matrix) startr endr startc endc)
  (let* ((acc 0)
	 (mu (mean-range m startr endr startc endc))
	 (musq (* mu mu)))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (incf acc (- (* v v) musq))))
    (double-float-divide acc (1- (count-range startr endr startc endc)))))

(defmethod sample-variance ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (sample-variance-range m 0 (- mr 1) 0 (- mc 1))))

