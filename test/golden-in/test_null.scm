(define (not x)            (if x #f #t))
(define (null? obj)        (if (eq? obj '()) #t #f))
