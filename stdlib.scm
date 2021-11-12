;; Conditionals {{{
;; if x then y else z
(define (if x y z)
  (cond (x y) (#t z)))

(define (and x y)
  (if x (if y #t #f)
        #f))

(define (or x y)
  (cond (x #t)
        (y #t)
        (#t #f)))

(define (not x)
  if x #f #t)

(define (null? x)
  (eq? x '()))
;; }}}
;; List manipulation {{{
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define (zip x y)
  (cond ((and (null? x) (null? y)) '())
        ((and (not (atom? x)) (not (atom? y)))
              (cons (list (car x) (car y))
                    (zip  (cdr x) (cdr y))))))

(define (find k v)
  (if (eq? k (caar v))
    (cadar v)
    (zip x (cdr v))))

(define (foldl fn acc xs)
  (if (null? xs)
      acc
      (foldl fn (fn acc (car xs)) (cdr xs))))

(define (foldr fn acc xs)
  (if (null? xs)
      acc
      (fn (car xs) (foldr fn acc (cdr xs)))))

(define (unfold fn init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold fn (fn init) pred))))

(define (length xs)
  (foldl (lambda (x y) (+ x 1))
         0 xs))

(define (reverse xs)
  (foldl (flip cons) '() xs))

(define (map fn xs)
  (foldr (lambda (x y)
           (cons (fn x) y))
         '() xs))

(define (filter pred xs)
  (foldr (lambda (x y)
           (if (pred x)
             (cons x y)
             y))
         '() xs))

;; }}}

(define (curry fn x)
  (lambda (y) (fn x y)))

(define (flip fn)
  (lambda (x y) (fn y x)))

(define (fact x)
  (if (eq? x 1)
    1
    (* x (fact (- x 1)))))
