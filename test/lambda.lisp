
(progn
  (setq a 100)
  (defun times-elm (n xs)  
    (mapcar (function (lambda (x) (* x n))) xs))  
  (setq r (times-elm 3 (list 1 (+ 5 6) a)) )  ; '(1 (+ 5 6) a) だとエラー: (+ 5 6) is not of type NUMBER
  (print r)  ;=> (3 33 300)
  )
