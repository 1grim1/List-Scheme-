(define (check-frac program)
  (cnt 0 program 0))
(define (cnt wc program a)
  (define digits
    (list (list #\0) (list #\1) (list #\2) (list #\3) (list #\4) (list #\5) (list #\6) (list #\7) (list #\8) (list #\9)))
  (define ops
    (list  (list '#\+) (list '#\-) (list '#\*) (list '#\/)))
  (define space
    (list '(#\newline) '(#\tab) '(#\space)))
  (if (= wc (string-length program))
      (= wc (string-length program))
      (if (and (eq? (string-ref program wc) #\/) (eq? (string-ref program (+ 1 wc)) #\-))
          #f
          (if (assq (string-ref program wc) digits)
              (cnt (+ 1 wc) program a)
              (if (assq (string-ref program wc) ops)
                  (cnt (+ 1 wc) program a)
                  (assq (string-ref program wc) ops))))))
(define (scan-frac program)
  (if (check-frac program)
      (scanf 0 program '())
      #f))
(define (scanf wc program xs)
  (define digits
    (list (list #\0) (list #\1) (list #\2) (list #\3) (list #\4) (list #\5) (list #\6) (list #\7) (list #\8) (list #\9)  (list '#\-) (list '#\/)))
  (if (= wc (string-length program))
      (list->string xs)
      (if (assq (string-ref program wc) digits)
          (scanf (+ 1 wc) program (append xs (list (string-ref program wc))))
          (scanf (+ 1 wc) program xs))))
(define (scan-many-fracs program)
  (cc 0 program '()))
(define (cc wc program xs)
  (define digits
    (list (list #\0) (list #\1) (list #\2) (list #\3) (list #\4) (list #\5) (list #\6) (list #\7) (list #\8) (list #\9)  (list '#\-) (list '#\/)))
  (define space
    (list '(#\newline) '(#\tab) '(#\space)))
  (if (= wc (string-length program))
          (list->string xs)
          (if (and (not (= wc 0)) (assq (string-ref program wc) space) (assq (string-ref program (- wc 1)) digits))
              (cc (+ 1 wc) program (append xs (list #\ )))
              (if (assq (string-ref program wc) digits)
                  (cc (+ 1 wc) program (append xs (list (string-ref program wc))))
                  (cc (+ 1 wc) program xs)))))
      
(define (parse program)

  (define (body wc s)
    (if (>= wc (vector-length program))
        #f
        (if(eq? (vector-ref program wc) 'end)
           (list s)
           (if (eq? (vector-ref program wc) 'if)
               (body (endif wc) (append s (bodyif wc '())))
               (body (+ 1 wc) (append s (list (vector-ref program wc))))))))
  (define (end wc)
    (if (>= wc (vector-length program))
        #f
        (if(eq? (vector-ref program wc) 'end)
           (+ 1 wc)
           (end (+ 1 wc)))))
  (define (endif wc)
    (if (>= wc (vector-length program))
        #f
        (if (eq? (vector-ref program wc) 'endif)
            (+ 1 wc)
            (endif (+ 1 wc)))))
  (define (bodyif wc s)
    (if (eq? (vector-ref program wc) 'endif)
        (list s)
        (bodyif (+ 1 wc) (append s (list (vector-ref program wc))))))
    

  
  (define (cnt wc xs ls)
    (if (not (number? wc))
        wc
        (if  (= wc (vector-length program))
             (cons xs (list ls))
             (if (eq? (vector-ref program wc) 'if)
                 (cnt (endif wc) xs (append ls (list (append (list 'if) (bodyif (+ 1 wc) '())))))
                 (if (eq? (vector-ref program wc) 'define)
                     (cnt (end wc) (append xs (list (append (list (vector-ref program (+ 1 wc))) (body (+ 2 wc) ls)))) ls)
                     (cnt (+ 1 wc) xs (append ls (list (vector-ref program wc)))))))))
  (cnt 0 '() '()))
(parse #( define -- 1 - end 
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
          4 factorial ))