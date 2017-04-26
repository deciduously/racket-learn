#lang racket
(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (λ (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (λ (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (λ (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (λ (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (λ (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (λ (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (λ (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (λ (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (λ (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiInsertR
  (λ (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons (car lat) (cons new (multiInsertR new old (cdr lat)))))
      (else (cons (car lat) (multiInsertR new old (cdr lat)))))))

(define multiInsertL
  (λ (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cons (car lat) (multiInsertL new old (cdr lat)))))
      (else (cons (car lat) (multiInsertL new old (cdr lat)))))))

(define multisubst
  (λ (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
  (λ (n)
    (+ n 1)))

(define sub1
  (λ (n)
    (- n 1)))

(define o+
  (λ (m n)
    (cond
      ((zero? n) m)
      (else (o+ (add1 m) (sub1 n)))))) ; the book says (add1 (o+ x) (sub1 y)) but addition is associative, right?

(define o-
  (λ (m n)
    (cond
      ((zero? n) m)
      (else (o- (sub1 m) (sub1 n))))))

(define addtup
  (λ (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (λ (m n)
    (cond
      ((zero? n) 0)
      (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (λ (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote ()))
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (λ (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (λ (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (λ (n m)
    (cond
      ((o> m n) #f)
      ((o< m n) #f)
      (else #t))))

(define oexpt
  (λ (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (oexpt n (sub1 m)))))))