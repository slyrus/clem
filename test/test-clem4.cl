
(defparameter m1 (clem-test::make-instance 'clem:double-float-matrix :initial-element 2.0d0 :rows 1024 :cols 1024))
(defparameter m2 (clem-test::make-instance 'clem:double-float-matrix :initial-element 2.0d0 :rows 1024 :cols 1024))
(defparameter m3 (clem-test::make-instance 'clem:double-float-matrix :initial-element 0.0d0 :rows 1024 :cols 1024))

(time
 (let ((irowblock 32)
       (icolblock 32)
       (jrowblock 32)
       (jcolblock 32)
       (kblock 32)
       (ilimit 512)
       (jlimit 512)
       (klimit 512))
   (declare (type fixnum
		  irowblock icolblock jrowblock jcolblock kblock
		  ilimit jlimit klimit))
   (do ((k 0 (+ k icolblock)))
       ((>= k klimit))
     (do ((i 0 (+ i iblock)))
	 ((>= i ilimit))
       (do ((j 0 (+ j jblock)))
	   ((>= j jlimit))
	 (clem::mat-mult-3-df m1 m2 m3
			      i (+ i (1- iblock)) i (+ i (1- iblock))
			      j (+ j (1- jblock)) j (+ j (1- jblock))
			      k (+ k (1- kblock)) k (+ k (1- kblock))))))))

