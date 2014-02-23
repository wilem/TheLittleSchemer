(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; 空心的加号打不出来，加圆圈来表示
(define ⊕
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (⊕ n (sub1 m)))))))

(define ⊖
  (lambda (n m) 
    (cond 
      ((zero? m) n) 
      (else (sub1 (⊖ n (sub1 m)))))))

; 可以使用unicode码，特殊符号等作为函数名字
; http://zh.wikipedia.org/wiki/Wikipedia:%E6%A0%BC%E5%BC%8F%E6%89%8B%E5%86%8C/%E6%95%B0%E5%AD%A6#.E5.B8.B8.E7.94.A8Unicode.E6.95.B0.E5.AD.A6.E7.AC.A6.E5.8F.B7
; × ⊕
(define ×
   (lambda (n m)
     (cond
       ((zero? m) 0)
       (else (⊕ n (× n (sub1 m)))))))

; 以上的算术运算效率都不高，在racket中运行 5 * 5000 要半天!!!

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (⊕ (car tup1) (car tup2))
             (tup+
              (cdr tup1) (cdr tup2)))))))

; ⋖ ⋗
(define ⋗
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (⋗ (sub1  n) (sub1  m))))))

(define ⋖
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (⋖ (sub1  n) (sub1  m))))))

(define ≡
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define ↑
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (× n (↑ n (sub1 m)))))))

(define ÷
  (lambda (n m)
    (cond
      ((⋖ n m) 0)
      (else (add1 (÷ (⊖ n m) m))))))

; length of list
(define len
  (lambda (lat)
    (cond
      (( null? lat) 0)
      (else (add1 (len (cdr lat)))))))

; atom at nth pos in list
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; remove nth ele in list.
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1  n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1  n)
                           (cdr lat)))))))

; number? is a primitive function
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat))
               (no-nums (cdr lat)))
              (else (cons (car lat)
                          (no-nums
                           (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((number? (car lat))
          (cons (car lat)
                (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(define eqan?
  (lambda (al a2)
    (cond
      ((and (number? a1) (number? a2))
       (= al a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? al a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? (car lat) a)
          (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

; 
(define rember
  (lambda (s l)
           (cond
             ((null? l) (quote ()))
             ((equal? (car l) s) (cdr l))
             (else (cons (car l)
                         (rember s (cdr l)))))))

(define xyz '(x y z))

(define foo (rember 'y xyz))

foo

; chapter 6
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and
        (atom? (car (cdr aexp))) ; +-*/
        (numbered? (car aexp)) ; l
        (numbered? (car (cdr (cdr aexp))))))))) ; r

(define aexp '(3 + 4 * (5 * 6)))
(numbered? aexp)

aexp
(car aexp) ; 1
(car (cdr aexp)) ; 2
(car (cdr (cdr aexp))) ; 3
(car (cdr (cdr (cdr aexp)))) ; 4
(car (cdr (cdr (cdr (cdr aexp))))) ; 5
