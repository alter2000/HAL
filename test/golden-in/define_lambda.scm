(define app (lambda (arg1 arg2) (append arg1 arg2)))
(define appendWord (lambda (arg) (app arg "talk")))
(define res (appendWord "small"))
res
