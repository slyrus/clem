
(in-package :clem)

;;; matrix move generic functions

(defgeneric matrix-move-range (m n startr1 endr1 startc1 endc1
                                 startr2 endr2 startc2 endc2))

(defgeneric matrix-move-range-constrain
    (m n startr1 endr1 startc1 endc1
       startr2 endr2 startc2 endc2))

(defgeneric matrix-move (m n &key constrain))

(defgeneric mat-mult-3-block (m n p))


;;; log generic functions

(defgeneric mat-log-range (m startr endr startc endc &optional base))

(defgeneric mat-log-range! (m startr endr startc endc &optional base))

(defgeneric mat-log (m &optional base))

(defgeneric mat-log! (m &optional base))

;;; hadamard product

(defgeneric mat-hprod-range (m n startr endr startc endc))

(defgeneric mat-hprod (m n))

(defgeneric mat-hprod-range! (m n startr endr startc endc))

(defgeneric mat-hprod! (m n))

;;; add

(defgeneric mat-add-range (m n startr endr startc endc))

(defgeneric mat-add-range! (m n startr endr startc endc))

(defgeneric mat-add! (m n))

;;; abs

(defgeneric mat-abs-range (m startr endr startc endc))

(defgeneric mat-abs-range! (m startr endr startc endc))

;;; transformation

(defgeneric %transfrom-matrix (m n xfrm &key background interpolation))

;;; discrete convolution

(defgeneric %discrete-convolve (u v z &key norm-v))

(defgeneric %separable-discrete-convolve (m h1 h2 z1 z2 &key truncate norm-v matrix-class))
