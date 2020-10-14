(define (not x)
  (if x #f #t))

(define nil '())

(define (null? obj) 
  (if (eqv? obj '()) #t #f))

(define (list . objs) objs)

(define (id obj) obj)

(define (flip f) (lambda (a b) (f b a)))

(define (curry f a1) (lambda (a2) (apply f (cons a1 (list a2)))))
(define (compose f g) (lambda arg) f (apply g arg))

(define zero? (curry = 0))
(define positive? (curry > 0))
(define negative? (curry < 0))

(define (odd? n) (= (mod n 2) 1))
(define (even? n) (= (mod n 2) 0))

(define (foldr f end lst)
  (if (null? lst) end
      (f (car lst) 
         (foldr f end (cdr lst)))))

(define (foldl f acc lst)
  (if (null? lst) acc
      (foldl f (f acc (car lst)) (cdr lst))))

(define fold foldl)

(define reduce foldr)

(define (unfold f init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold f (f init) pred))))

(define (sum . lst) (foldl + 0 lst))
(define (product . lst) (foldl * 1 lst))
(define (and . lst) (foldl && #t lst))

(define (or . lst) (foldl || #f lst))

(define (max fst . rst)
  (foldl (lambda (old new)
           (if (> old new) old new)) fst rst))

(define (min fst rst)
  (foldl (lambda (old new)
           (if (< old new) old new)) fst rst))

(define (length list)
  (foldl (lambda (x y) (+ x 1)) 0 list))

(define (reverse list)
  (foldl (flip cons) nil list))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map f lst)
  (foldr
    (lambda (x y) (cons (f x) y))
    nil lst))

(define (filter pred lst)
  (foldr
    (lambda (x y) 
      (if (pred x) (cons x y) y)) nil lst))
