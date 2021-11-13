;; Conditionals {{{
;; if x then y else z
(define (if x y z) (cond (x y) (#t z)))
(define (not x) (if x #f #t))
(define (and x y) (if x (if y #t #f) #f))
(define (or x y)
  (cond (x #t)
        (y #t)
        (#t #f)))
(define (null? x) (eq? x '()))
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
    (zip k (cdr v))))

(define (fold-left fn acc xs)
  (if (null? xs)
      acc
      (fold-left fn (fn acc (car xs)) (cdr xs))))

(define (fold-right fn acc xs)
  (if (null? xs)
      acc
      (fn (car xs) (fold-right fn acc (cdr xs)))))

(define (unfold fn init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold fn (fn init) pred))))

(define (length xs)
  (fold-left (lambda (x y) (+ x 1))
         0 xs))

(define (reverse xs)
  (fold-left (flip cons) '() xs))

(define (map fn xs)
  (fold-right (lambda (x y)
           (cons (fn x) y))
         '() xs))

(define (filter pred xs)
  (fold-right (lambda (x y)
           (if (pred x)
             (cons x y)
             y))
         '() xs))

;; }}}
;; Arithmetics {{{
;; only '<' is defined
(define (>= x y) (not (< x y)))
(define (<= x y) (or (< x y) (eq? x y)))
(define (> x y) (not (<= x y)))
(define (even? x) (eq? 0 (mod x 2)))
(define (abs x) (if (< x 0) (- x) x))
;; }}}
;; On functions {{{
(define (curry fn x)
  (lambda (y) (fn x y)))

(define (flip fn)
  (lambda (x y) (fn y x)))
;; }}}
;; car/cdr combos {{{
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (caddar x) (car (cdr (cdr (car x)))))
;; }}}
;; Big bois {{{
(define (fact x)
  (if (eq? x 1)
    1
    (* x (fact (- x 1)))))

(define (fib x)
  (cond ((eq? x 0) (0))
        ((eq? x 1) (1))
        (#t (+ (fib (- x 1))
               (fib (- x 2))))))


(define (merge-lists xs ys)
  (cond ((null? xs) ys)
        ((null? ys) xs)
        ((< (car xs) (car ys))
            (cons (car xs) (merge-lists (cdr xs) ys)))
        (#t (cons (car ys) (merge-lists xs (cdr ys))))))

(define (split-half lst xs ys)
  (cond ((null? lst) (cons xs ys))
        ((null? (cdr lst))
         (split-half (cdr lst)
                     (cons (car lst) xs)
                     ys))
        (#t (split-half (cddr lst)
                        (cons (car lst) xs)
                        (cons (cadr lst) ys)))))

(define (merge-sort xs)
  (cond ((null? xs) '())
        ((null? (cdr xs)) xs)
        (#t (let ((xss . (split-half xs '() '()))
                  (merge-lists (merge-sort (car xss))
                               (merge-sort (cdr xss))))))))


;; biggest boi
(define (eval expr env)
  (cond ((atom? expr) (find expr env))

        ((atom? (car expr))
         (cond
           ((eq? (car expr) 'quote)
            (cadr expr))
           ((eq? (car expr) 'atom?)
            (atom? (eval (cadr expr) env)))
           ((eq? (car expr) 'eq?)
            (eq? (eval (cadr expr) env)
                 (eval (caddr expr) env)))
           ((eq? (car expr) 'car)
            (car (eval (cadr expr) env)))
           ((eq? (car expr) 'cdr)
            (cdr (eval (cadr expr) env)))
           ((eq? (car expr) 'cons)
            (cons (eval (cadr expr) env)
                  (eval (caddr expr) env)))
           ((eq? (car expr) 'cond)
            (eval-cond (cdr expr) env))
           (#t (eval (cons (find (car expr) env)
                           (cdr expr))
                     env))
           ))

    ))
;; }}}
