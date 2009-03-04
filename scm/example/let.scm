;;; inner scope: 10
;;; outer scope: 42


(let ((x 42))
  (let ((x 10))
    (print "inner scope:" x))
  (print "outer scope:" x))
