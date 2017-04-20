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
      ((not (member? old lat)) lat)
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (λ (new old lat)
    (cond
      ((not (member? old lat)) lat)
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (λ (new old lat)
    (cond
      ((not (member? old lat)) lat)
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (λ (new o1 o2 lat)
    (cond
      ((not (or (member? o1 lat) (member? o2 lat))) lat)
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (λ (a lat)
    (cond
      ((not (member? a lat)) lat)
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))