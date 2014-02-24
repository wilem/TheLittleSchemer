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

;
; chapter 7
;
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))
; p53
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) a)
          (multirember a (cdr lat)))
         (else (cons (car lat)
                     (multirember a
                                  (cdr lat)))))))))

; p112
(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
                  (makeset2
                   (multirember (car lat)
                                (cdr lat)))))
      )))

; p113
; check if set1 is a subset of set2.
(define subset-2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (cond ; delete this line
              ((member? (car set1) set2)
               (subset-2? (cdr set1) set2))
              (else #f)
              ))  ; delete this line
      )))

; p114
(define subset-1?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset-1? (cdr set1) set2))
      (else #f)
      )))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))
       )
      
      )))

(makeset2 '(a b b c d d e f f))

"foo:"
(define foo '(1 2 3))
foo
"bar:"
(define bar '(3 2 1 0 -1))
bar
"Is set foo a subset of set bar?"
(subset? foo bar)

; p114
(define eqset-1?
  (lambda (set1 set2)
    (cond
      ((subset? set1 set2)
       (subset? set2 set1))
      (else #f)
      )))

; p115
(define eqset?
  (lambda (set1 set2)
    (cond
      (else (and (subset? set1 set2)
                 (subset? set2 set1)))
      )))

(define intersect-1?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (cond
         ((member? (car set1) set2) #t)
         (else
          (intersect-1? (cdr set1) set2)
          )
         )
       )
      )
    )
  )

(define intersect-2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or
        (member? (car set1) set2)
        (intersect-2? (cdr set1) set2)
        )
       )
      )
    ))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))
       )
      )
      ))

"Is set foo equal to set bar?"
(eqset? foo bar)

"Is foo intersect with bar?"
(intersect? foo bar)

; p116
; make the intersect set of set1 and set2.
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2))
       )
      (else (intersect (cdr set1) set2))
      )
    ))

; make union of set1 and set2.
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))
       )
      )
    ))

"union of foo and bar is:"
(union foo bar)

; p117
; returns all the atoms in set1 that are not in set2.
(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else
       (cons (car set1))
       )
      )
    )
  )

; return intersection of all sets.
(define intersectall
  (lambda (l-set) ; list of set.
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set))))
      )
    ))

(define list-of-set
  (list '(6 pears and)
        '(3 peaches and 6 peppers)
        '(8 pears and 6 plums)
        '(and 6 prunes with some apples)))
"list of set is:"
list-of-set
"intersection of set in the list of set:"
(intersectall list-of-set)


; p118
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f)
      )
    ))

; p119
; pair
(define first
  (lambda (p)
    (cond
      (else (car p)))
    ))

(define second
  (lambda (p)
    (cond
      (else (car (cdr p))))
    ))

(define build
  (lambda (s1 s2)
    (cond
      (else (cons s1
                  (cons s2 (quote ())))))
    ))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; relation and function.
; rel: relation, fun: function.
"relation:"
'((apples peaches) (pumpkin pie))
"relation:"
'((4 3) (4 2) (7 6) (6 2) (3 4))

; defined at chap3/p44:
; The function firsts takes one argument,
; a list, which is either a null list
; or contains only non-empty lists.
; It builds another list composed of
; the first S-expression of each internal list.
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l))))
      )
    ))

; p120
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((1 2) (2 3)))

; reverse the relations.
(define revrel-1
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons
             (build (second (car rel))
                    (first (car rel)))
             (revrel-1 (cdr rel))
             ))
      )
    ))

; p121
; reverse pair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))
    ))

; reverse relations:
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel))
                  (revrel (cdr rel))))
      )
    ))

"rel1:"
(define rel1 '((1 2) (2 3) (3 4)))
rel1
"reversion of rel1:"
(revrel rel1)

; seconds: second atom of all pairs.
(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (second (car l))
                  (seconds (cdr l))))
      )
    ))

; p121
; one-to-one?
; monotone?
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
"fun1:"
(define fun1 '((chocolate chip) (doughy cookie)))
fun1
"Is fun1 a fullfun?"
(fullfun? fun1)

; cookies
(define cookies
  (lambda ()
    (bake
     (quote (350 degrees))
     (quote (12 minutes))
     (mix
      (quote (walnuts 1 cup))
      (quote (chocolate-chips 16 ounces))
      (mix
       (mix
        (quote (flour 2 cups))
        (quote (oatmeal 2 cups))
        (quote (salt .5 teaspoon))
        (quote (baking-powder 1 teaspoon))
        (quote (baking-soda 1 teaspoon)))
       (mix
        (quote (eggs 2 large))
        (quote (vanilla 1 teaspoon))
        (cream
         (quote (butter 1 cup))
         (quote (sugar 2 cups)))
        )
       )
      )
     )))


;
; chap8
; p125

; difference between eq? and equal?
;

(define rember-f-1
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      (else
       (cond
         ((test? (car l) a) (cdr l)) ; (funcall test? ...)
         (else (cons (car l)
                     (rember-f-1 test? a (cdr l))
                     ))
         )
       )
      )
    ))


;
(define rember-f-1
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ;((test? (car l) a) (cdr l)) ; (funcall test? ...)
      ((apply test? (list (car l) a)) (cdr l))
      (else (cons (car l)
                  (rember-f-1 test? a (cdr l))))
      )
    ))

; if test? is eq?, rember-f acts just like rember.
(rember-f-1 = 1 '(2 1 1 0))

;> (define x '(1 2 3))
;> x
;(1 2 3)
;> (equal? x '(1 2 3))
;#t
;> (equal? x (list 1 2 3))
;#t
;> (equal? x (quote (1 2 3)))
;#t
;> (eq? x (quote (1 2 3)))
;#f
;

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq-0 (eq?-c 0))
(apply eq-0 (list 1)) ; provide list of arguments.
(apply eq-0 '(1))
(define eq-211 (eq?-c (list 2 1 1)))
;(setq eq-211 (eq?-c (list 2 1 1))) ;setq undefined???
(define l211 '(2 1 1))
(apply eq-211 '(l211))

(define k 'salad)
(define eq?-salad (eq?-c k))
(define y 'salad)
" what is (eq?-salad y)"
" where y is salad"
(apply eq?-salad (list y))
" ((eq?-c 'salad) y):"
((eq?-c 'salad) y)

; p128
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l)) ; (funcall test? ...)
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))
                    ))
        )
      ))
  )

(define test? eq?)
(define rember-eq? (rember-f test?))
(define a 'tuna)
(define l '(tuna salad is good))
;(rember-eq? a l)
;((rember-f test?) a l)
((rember-f eq?) a l)

(define l '(equal? eq? eqan? eqlist? eqpair?))
(define a 'eq?) ; a is not the function eq?
((rember-f eq?) a l)

; p130
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else
         (cons (car l)
               ((insertL-f test?) new old
                                  (cdr l))
               )
         )
        )
      )))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else
         (cons (car l)
               ((insertR-f test?) new old
                                  (cdr l))
               )
         )
        )
      )))

; p131
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; p132
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else
         (cons (car l)
               ((insert-g seq) new old
                               (cdr l))
               )
         )
        )
      )))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

"insert c on the left of d in list (a b d d)"
(insertL 'c 'd '(a b d d))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(subst 'b 'x '(a x x d e))

; p133
; remove old from l.
(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(yyy 'b '(a b b c))


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp)
            (quote +))
       (⊕ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp)
            (quote x ))
       (× (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      (else
       (↑ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) +)
      ((eq? x (quote ×)) ×)
      (else ↑)
      )
    ))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function
         (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))
        )
       )
      )
    ))

; helper function here.
(define operator
  (lambda (nexp)
    (first nexp)
    ))

(define 1st-sub-exp
  (lambda (nexp)
    (second nexp)
    ))

(define 2nd-sub-exp
  (lambda (nexp)
    (third nexp)
    ))

;(value '(⊕ (× 2 3) (↑ 2 3))))
(× 2 3)
(↑ 2 3)
(⊕ 6 8)
(⊕ (× 2 3) (↑ 2 3))
(value '(⊕ (× 2 3) (↑ 2 3)))
