(define (interpret program ds) 
  (define (end a) 
    (if (eq? 'end (vector-ref program a)) 
        a 
        (end (+ 1 a)))) 
  (define (cnt wc ds rs articles) 
    (if (not (eq? wc (vector-length program))) 
        (if (integer? (vector-ref program wc)) 
            (cnt (+ 1 wc) (cons (vector-ref program wc) ds) rs articles) 
            (if (assq (vector-ref program wc) proc) 
                (apply cnt (apply (cdr (assq (vector-ref program wc) proc)) (list wc ds rs articles)));; Измененное состояние интерпретатора 
                (cnt (cadr (assq (vector-ref program wc) articles)) ds (cons (+ 1 wc) rs) articles))) 
        ds)) 

  (define proc 
    (list 
     (cons 'define (lambda (wc ds rs articles)(list (+ 1 (end wc)) ds rs (cons (cons (vector-ref program (+ 1 wc)) (cons (+ wc 2) '())) articles)))) 
     (cons '+ (lambda (wc ds rs articles) (list (+ 1 wc) (cons (+ (cadr ds) (car ds)) (cddr ds)) rs articles))) 
     (cons '- (lambda (wc ds rs articles) (list (+ 1 wc) (cons (- (cadr ds) (car ds)) (cddr ds)) rs articles))) 
     (cons '* (lambda (wc ds rs articles) (list (+ 1 wc) (cons (* (car ds) (cadr ds)) (cddr ds)) rs articles))) 
     (cons '/ (lambda (wc ds rs articles) (list (+ 1 wc) (cons (quotient (cadr ds) (car ds)) (cddr ds)) rs articles))) 
     (cons 'mod (lambda (wc ds rs articles) (list (+ 1 wc) (cons (remainder (cadr ds) (car ds)) (cddr ds)) rs articles))) 
     (cons 'neg (lambda (wc ds rs articles) (list (+ 1 wc) (cons (* -1 (car ds)) (cdr ds)) rs articles))) 
     (cons '= (lambda (wc ds rs articles) (list (+ 1 wc) (cons (if (= (car ds) (cadr ds)) -1 0) (cddr ds)) rs articles))) 
     (cons '> (lambda (wc ds rs articles) (list (+ 1 wc) (cons (if (> (cadr ds) (car ds)) -1 0) (cddr ds)) rs articles))) 
     (cons '< (lambda (wc ds rs articles) (list (+ 1 wc) (cons (if (< (cadr ds) (car ds)) -1 0) (cddr ds)) rs articles))) 
     (cons 'not (lambda (wc ds rs articles) (list (+ 1 wc) (cons (not (car ds)) (cdr ds)) rs articles))) 
     (cons 'and (lambda (wc ds rs articles) (list (+ 1 wc) (cons (and (cadr ds) (car ds)) (cddr ds)) rs articles))) 
     (cons 'or (lambda (wc ds rs articles) (list (+ 1 wc) (cons (or (cadr ds) (car ds)) (cddr ds)) rs articles))) 
     (cons 'drop (lambda (wc ds rs articles) (list (+ 1 wc) (cdr ds) rs articles))) 
     (cons 'swap (lambda (wc ds rs articles) (list (+ 1 wc) (append (list (cadr ds) (car ds)) (cddr ds)) rs articles))) 
     (cons 'dup (lambda (wc ds rs articles) (list (+ 1 wc) (append (list (car ds) (car ds)) (cdr ds)) rs articles))) 
     (cons 'over (lambda (wc ds rs articles) (list (+ 1 wc) (append (list (cadr ds) (car ds) (cadr ds)) (cddr ds)) rs articles))) 
     (cons 'rot (lambda (wc ds rs articles) (list (+ 1 wc) (append (list (caddr ds) (cadr ds) (car ds)) (cdddr ds)) rs articles))) 
     (cons 'depth (lambda (wc ds rs articles) (list (+ 1 wc) (append (list (length ds)) ds) rs articles))) 
     (cons 'if (lambda (wc ds rs articles) (list (if (eq? -1 (car ds)) (+ 1 wc) (searchendif wc)) (cdr ds) rs articles))) 
     (cons 'endif (lambda (wc ds rs articles) (list (+ 1 wc) ds rs articles))) 
     (cons 'end (lambda (wc ds rs articles) (list (car rs) ds (cdr rs) articles))) 
     (cons 'exit (lambda (wc ds rs articles) (list (car rs) ds (cdr rs) articles)))))


  (define (searchendif wc) 
    (if (eq? 'endif (vector-ref program wc)) 
        wc 
        (searchendif (+ 1 wc)))) 

  (define (proga lst a b c) 
    (if (= c 2) 
        lst 
        (proga (reverse (list-tail lst (+ 2 a))) (- (length lst) (+ b 1)) a (+ 1 c)))) 

  (cnt 0 ds '() '()))








(interpret #(   define abs 
                    dup 0 < 
                    if neg endif 
                end 
                 9 abs 
                -9 abs      ) (quote ()))

(interpret #(   define =0? dup 0 = end
define <0? dup 0 < end
define signum
=0? if exit endif
<0? if drop -1 exit endif
drop
1
end
0 signum
-5 signum
10 signum       ) (quote ()))



(interpret #(   define -- 1 - end
define =0? dup 0 = end
define =1? dup 1 = end
define factorial
=0? if drop 1 exit endif
=1? if drop 1 exit endif
dup --
factorial
*
end
0 factorial
1 factorial
2 factorial
3 factorial
4 factorial     ) (quote ()))



(interpret #(   define =0? dup 0 = end
define =1? dup 1 = end
define -- 1 - end
define fib
=0? if drop 0 exit endif
=1? if drop 1 exit endif
-- dup
-- fib
swap fib
+
end
define make-fib
dup 0 < if drop exit endif
dup fib
swap --
make-fib
end
10 make-fib     ) (quote ()))



(interpret #(   define =0? dup 0 = end
define gcd
=0? if drop exit endif
swap over mod
gcd
end
90 99 gcd
234 8100 gcd    ) '())