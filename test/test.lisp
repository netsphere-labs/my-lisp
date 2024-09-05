
((data "quoted data" 123 4.5)
 (data (!@#x (4.5) "(more\" \\data)"))
 (1 . (2 . (3 4)))
 `(a ,(+ 3 4) c)
)

;; (read) は一つだけ読み込むことに注意
;; (with-open-file (f "test.lisp") (read f))
; ((DATA "quoted data" 123 4.5) (DATA (|!@#X| (4.5) "(more\" \\data)")) (1 2 3 4)
;  `(A ,(+ 3 4) C))


