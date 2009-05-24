;;; inner scope: 10
;;; outer scope: 42


(let ((x 42))
  (let ((x 10))
    (trace "inner scope:" x))
  (trace "outer scope:" x))
