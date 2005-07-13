;;;
;;; File: interpolation.cl
;;; Description: interpolation for the clem matrix package
;;; Author: Cyrus Harmon
;;; Time-stamp: "2005-07-12 16:26:47 sly"
;;;

(in-package :clem)


(defmacro bilinear-interpolate
    (g00 g01 g10 g11 a b)
  `(+ ,g00
      (* ,a (- ,g10 ,g00))
      (* ,b (- ,g01 ,g00))
      (* ,a ,b (- (+ ,g00 ,g11)
                (+ ,g10 ,g01)))))

(declaim (inline quadratic-kernel))
(defun quadratic-kernel (s)
  (declare (type double-float s)
           (optimize (speed 3) (safety 0)))
  (cond ((<= -0.5d0 s 0.5d0)
         (+ (* -2d0 (* s s)) 1d0))
        ((<= -1.5d0 s 1.5d0)
         (+ (* s s) (- (/ (* 5.0d0 (abs s)) 2.0d0)) 1.5d0))
        (t 0.0d0)))

(defmacro quadratic-interpolate
    (g00 g01 g02
     g10 g11 g12 
     g20 g21 g22 a b)
  `(let ((a0 (quadratic-kernel (- -1, a)))
         (a1 (quadratic-kernel (- ,a)))
         (a2 (quadratic-kernel (- 1 ,a)))
         (b0 (quadratic-kernel (- -1 ,b)))
         (b1 (quadratic-kernel (- ,b)))
         (b2 (quadratic-kernel (- 1 ,b))))
     (+ (* a0 (+ (* b0 ,g00)
                 (* b1 ,g01)
                 (* b2 ,g02)))
        (* a1 (+ (* b0 ,g10)
                 (* b1 ,g11)
                 (* b2 ,g12)))
        (* a2 (+ (* b0 ,g20)
                 (* b1 ,g21)
                 (* b2 ,g22))))))

