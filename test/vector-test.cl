(let ((rv (array->row-vector #(1 2 3 4 5)))
      (cv (array->col-vector #(1 2 3 4 5))))
  (print rv)
  (print-matrix rv)
  (print 'bogus)
  (print (transpose rv))
  (print-matrix (transpose rv))

  (print cv)
  (print-matrix cv)
  (print (transpose cv))
  (print-matrix (transpose cv))

  (print-matrix (mat-mult rv cv))
  (print-matrix (mat-mult cv rv))

  )


(let ((cv (array->col-vector #(1 2 3 4)))
      (rv (array->row-vector #(1 2 3))))
  (print-matrix cv)
  (print-matrix (transpose rv))
  (print (dim cv))
  (print-matrix (mat-mult cv (transpose cv))))
