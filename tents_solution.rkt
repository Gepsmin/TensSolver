#lang scheme
;2017400222

(define kare (lambda (x) (* x x)))

(define toplam (lambda (liste) (if (null? liste) 0 (+ (car liste) (toplam (cdr liste))))))

(define n-th (lambda (liste n) (if (eq? 0 n) (car liste) (n-th (cdr liste) (- n 1)))))

(define factorial (lambda (x) (if (eq? x 1) 1 (* x (factorial(- x 1))))))

(define list '(1 2 3))

(define ln (lambda (list) (if (null? list) 0 (+ 1 (ln(cdr list))))))

(define sum (lambda (ls) (if (null? ls) 0 (+ (car ls) (sum(cdr ls))))))

(define reduce-list(lambda (lst base f) (if (null? lst)
                                     base
                                     (f (car lst) (reduce-list (cdr lst) base f)))))

(define g '((2 0 2 1 2 0 0 0)
              (0 1 2 2 0 0 0 0)
              (0 1 0 0 0 0 0 2)
              (0 0 0 0 0 0 0 0)
              (0 0 0 0 0 0 0 0)))

(define set-row (lambda (row c v)
                   (if (= c 0)
                       (cons v (cdr row))
                       (cons (car row) (set-row(cdr row) (- c 1) v)))))

(define set-grid (lambda (grid r c v)
                   (if (= r 0)
                       (cons (set-row (car grid) c v) (cdr grid))
                       (cons (car grid) (set-grid (cdr grid) (- r 1) c v)))))

(define RETURN-FIRST-NOT-FALSE (lambda (f lst)
                                 (if (null? lst)
                                     #f
                                     (if (f (car lst))
                                         (f (car lst))
                                         (RETURN-FIRST-NOT-FALSE f (cdr lst))))))




(define ADJACENT (lambda (pos1 pos2)
                   (if (or (> (abs (- (car pos1) (car pos2))) 1)
                           (> (abs (- (cadr pos1) (cadr pos2))) 1))
                               #f
                               #t)))

(define NEIGHBOR-LIST (lambda (lst)
                        (cons (cons (car lst) (cons (+ (cadr lst) 1) '()))
                              (cons (cons (+ (car lst) 1) (cons (cadr lst) '()))
                                    (cons (cons (car lst) (cons (- (cadr lst) 1) '()))
                                          (cons (cons (- (car lst) 1) (cons (cadr lst) '())) '()))))))

(define SIZEX  10)
(define SIZEY  10)

(define is-first-null (lambda (lst) (if (null? (car lst)) #t #f)))

(define NEIGHBOR (lambda (grid lst)
                        (cons (if (>= (cadr lst) (list-length (car grid))) '() (cons (car lst) (cons (+ (cadr lst) 1) '())))
                              (cons (if (>= (car lst) (list-length grid)) '() (cons (+ (car lst) 1) (cons (cadr lst) '())))
                                    (cons (if (< (cadr lst) 2) '() (cons (car lst) (cons (- (cadr lst) 1) '())))
                                          (cons (if (< (car lst) 2) '() (cons (- (car lst) 1) (cons (cadr lst) '()))) '()))))))

(define around (lambda (grid lst)
                 (cons (if (or (< (car lst) 2) (< (cadr lst) 2)) '() (cons (- (car lst) 1) (cons (- (cadr lst) 1) '())))
                       (cons (if (or (< (car lst) 2) (>= (cadr lst) (list-length (car grid)))) '() (cons (- (car lst) 1) (cons (+ (cadr lst) 1) '()) ))
                             (cons (if (or (>= (car lst) (list-length grid)) (< (cadr lst) 2)) '() (cons (+ (car lst) 1) (cons (- (cadr lst) 1) '())))
                                   (cons (if (or (>= (car lst) (list-length grid)) (>= (cadr lst) (list-length (car grid)))) '() (cons (+ (car lst) 1) (cons (+ (cadr lst) 1) '())))
                                         (cons (if (>= (cadr lst) (list-length (car grid))) '() (cons (car lst) (cons (+ (cadr lst) 1) '())))
                              (cons (if (>= (car lst) (list-length grid)) '() (cons (+ (car lst) 1) (cons (cadr lst) '())))
                                    (cons (if (< (cadr lst) 2) '() (cons (car lst) (cons (- (cadr lst) 1) '())))
                                          (cons (if (< (car lst) 2) '() (cons (- (car lst) 1) (cons (cadr lst) '()))) '()))))))))))



(define inside (lambda (el1 lst) (if (null? lst)
                                     #f
                                     (if (equal? el1 (car lst))
                                         #t
                                         (inside el1 (cdr lst))))))

(define ADJACENT-WITH-LIST (lambda (lst lstlst) (if (null? lstlst) #f
                                                    (if (inside (car lstlst) (NEIGHBOR lst))
                                                        #t
                                                        (ADJACENT-WITH-LIST lst (cdr lstlst))))))




(define nth (lambda (lst n) (if (null? lst)
                                #f
                                (if (eq? n 1)
                                    (car lst)
                                    (nth (cdr lst) (- n 1)))) ))

(define REPLACE-NTH (lambda (lst n value) (if (null? lst)
                                              '()
                                              (if (eq? n 1)
                                                  (cons value (cdr lst))
                                                  (cons (car lst)(REPLACE-NTH (cdr lst) (- n 1) value))))))




(define make-row (lambda (x) (if (eq? x 0)
                               '()
                               (cons 2 (make-row (- x 1))))))

(define make-grid (lambda (x y) (if (eq? y 0)
                                    '()
                                    (cons (make-row x) (make-grid x (- y 1))))))


(define list-total (lambda (lst) (if (null? lst)
                                     0
                                     (+ (car lst) (list-total (cdr lst))))))

(define list-length (lambda (lst) (if (null? lst)
                                      0
                                      (+ 1 (list-length (cdr lst))))))



(define number-check (lambda (lst) (if (and (eq? (list-total (car lst)) (list-total (cadr lst)))
                                            (<= (list-total (car lst)) (list-length (caddr lst))))
                                       #t
                                       #f)))

(define number-check-second (lambda (lst) (if (and (eq? (list-total (car lst)) (list-total (cadr lst)))
                                            (eq? (list-total (car lst)) (list-length (caddr lst))))
                                       #t
                                       #f)))
                                            
(define remove-nth (lambda (lst n) (if (eq? n 1)
                                       (cdr lst)
                                       (cons (car lst) (remove-nth (cdr lst) (- n 1))))))

(define remove-element (lambda (lst element) (if (equal? element (car lst))
                                                 (cdr lst)
                                                 (cons (car lst) (remove-element (cdr lst) element)))))


(define forbid-place (lambda (grid lst) (if (null? lst)
                                            grid
                                            (if (eq? (find-grid-position grid (cons (cadr lst) (cons (car lst) '()))) 0)
                                                (REPLACE-NTH grid (car lst) (REPLACE-NTH (nth grid (car lst)) (cadr lst) 2))
                                                grid))))
                                                

(define forbid-places (lambda (grid lst) (if (null? lst)
                                             grid
                                             (forbid-places (forbid-place grid (car lst)) (cdr lst)))))

(define put-tent (lambda (grid lst) (forbid-places (REPLACE-NTH grid (car lst) (REPLACE-NTH (nth grid (car lst)) (cadr lst) 3)) (around grid lst))))

(define put-tents (lambda (grid lst) (if (null? lst)
                                         grid
                                         (put-tents (put-tent grid (car lst)) (cdr lst)))))

                                         
(define put-tree (lambda (grid lst) (REPLACE-NTH grid (car lst) (REPLACE-NTH (nth grid (car lst)) (cadr lst) 1))))

(define put-trees (lambda (grid lst) (if (null? lst)
                                         grid
                                         (put-trees (put-tree grid (car lst)) (cdr lst)))))


(define change-input (lambda (input lst) (if (null? lst)
                                             input
                                             (change-input (REPLACE-NTH
                                                            (REPLACE-NTH input 2 (REPLACE-NTH (cadr input) (cadr (car lst)) (- (nth (cadr input) (cadr (car lst))) 1)))
                                                            1 (REPLACE-NTH (car input) (car (car lst)) (- (nth (car input) (car (car lst))) 1)))
                                                           (cdr lst))))) 


(define put-zero (lambda (grid lst) (if (eq? (nth (nth grid (car lst)) (cadr lst)) 2)
                                        (REPLACE-NTH grid (car lst) (REPLACE-NTH (nth grid (car lst)) (cadr lst) 0))
                                        grid
                                        )))

(define put-zeros (lambda (grid lst) (if (null? lst)
                                         grid
                                         (if (null? (car lst))
                                             (put-zeros grid (cdr lst))
                                             (put-zeros (put-zero grid (car lst)) (cdr lst))))))

(define make-space (lambda (grid coordinate) (put-zeros grid (NEIGHBOR grid coordinate))))

(define make-possible-second (lambda (grid x y) (if (> y (list-length (car grid)))
                                                    grid
                                                    (if (eq? (find-grid-position grid (cons y (cons x '()))) 1)
                                                        (make-possible-second (make-space grid (cons x (cons y '()))) x (+ y 1))
                                                        (make-possible-second grid x (+ y 1))))))

(define make-possible-first (lambda (grid x) (if (> x (list-length grid))
                                                 grid
                                                 (make-possible-first (make-possible-second grid x 1) (+ x 1)))))
                                                    

(define make-possible (lambda (grid) (make-possible-first grid 1)))

(define find-zero-locations-row (lambda (lst row col) (if (null? lst)
                                                          '()
                                                          (if (eq? (car lst) 0)
                                                              (cons (cons row (cons col '())) (find-zero-locations-row (cdr lst) row (+ col 1)))
                                                              (find-zero-locations-row (cdr lst) row (+ col 1))))))


(define find-zero-locations-column (lambda (grid row col) (if (> row (list-length grid))
                                                              '()
                                                              (if (eq? (find-grid-position grid (cons col (cons row '()))) 0)
                                                                  (cons (cons row (cons col '())) (find-zero-locations-column grid (+ row 1) col))
                                                                  (find-zero-locations-column grid (+ row 1) col)))))

(define must-place-row (lambda (grid lst row) (if (>= (list-length (car lst)) row)
                                                  (if (eq? (nth (car lst) row) 0)
                                                      (must-place-row grid lst (+ row 1))
                                                      (if (eq? (nth (car lst) row) (count-row-zeros (nth grid row)))
                                                          (cons (put-tents grid (find-zero-locations-row (nth grid row) row 1))(cons (change-input lst (find-zero-locations-row (nth grid row) row 1)) '()) )
                                                          (must-place-row grid lst (+ row 1))))
                                                  #f)))




(define possible (lambda (grid input row col) (if (> row (list-length grid))
                                                  '()
                                                  (if (> col (list-length (car grid)))
                                                      (possible grid input (+ row 1) 1)
                                                      (if (and (eq? (find-grid-position grid (cons col (cons row '()))) 0)
                                                               (and (> (nth (car input) row) 0)
                                                                          (> (nth (cadr input) col) 0)))
                                                          (cons (cons row (cons col '())) (possible grid input row (+ col 1)))
                                                          (possible grid input row (+ col 1)))))))

(define is-any-tent (lambda (grid coordinates) (if (null? coordinates)
                                                   #f
                                                   (if (null? (car coordinates))
                                                       (is-any-tent grid (cdr coordinates))
                                                       (if (eq? 3 (find-grid-position grid (cons (cadr (car coordinates)) (cons (car (car coordinates)) '()))))
                                                           #t
                                                           (is-any-tent grid (cdr coordinates)))
                                                    ))))



(define tree-check (lambda (grid lst) (is-any-tent grid (NEIGHBOR grid lst))))

(define find-tents (lambda (grid row col) (if (> row (list-length grid))
                                              '()
                                              (if (> col (list-length (car grid)))
                                                  (find-tents grid (+ row 1) 1)
                                                  (if (eq? (find-grid-position grid (cons col (cons row '()))) 3)
                                                      (cons (cons row (cons col '())) (find-tents grid row (+ col 1)))
                                                      (find-tents grid row (+ col 1)))))))

(define trees-check (lambda (trees grid) (if (null? trees)
                                             #t
                                             (if (tree-check grid (car trees))
                                                 (trees-check (cdr trees) grid)
                                                 #f)
                                             )))
                                               

(define tent-check (lambda (grid lst) (not (is-any-tent grid (around grid lst)))))


(define tents-check-first (lambda (grid tents) (if (null? tents)
                                                   #t
                                                   (if (tent-check grid (car tents))
                                                       (tents-check-first grid (cdr tents))
                                                       #f))))
                           
(define tents-check (lambda (grid) (tents-check-first grid (find-tents grid 1 1))))

(define final-test (lambda (input grid) (if (trees-check (caddr input) grid)
                                            (if (tents-check grid)
                                                #t
                                                #f)
                                            #f
                                            )))


(define must-place-column (lambda (grid lst col) (if (>= (list-length (cadr lst)) col)
                                                     (if (eq? (nth (cadr lst) col) 0)
                                                         (must-place-column grid lst (+ col 1))
                                                         (if (eq? (nth (cadr lst) col) (count-zeros-column grid col))
                                                             (cons (put-tents grid (find-zero-locations-column grid 1 col)) (cons (change-input lst (find-zero-locations-column grid 1 col)) '()))
                                                             (must-place-column grid lst (+ col 1))))
                                                     #f)))

(define try (lambda (s input grid possibles)
              (if (null? possibles)
                  #f
                  (if (s  (change-input input (cons (car possibles) '())) (put-tent grid (car possibles)))
                      (s  (change-input input (cons (car possibles) '())) (put-tent grid (car possibles)))
                      (try s input grid (cdr possibles))))))


(define solution (lambda (lst grid) (if (is-false grid lst)
                                               (if (is-finished lst)
                                                   (if (final-test lst grid)
                                                       (find-tents grid 1 1)
                                                       #f)
                                                   (if (must-place-row grid lst 1) 
                                                       (solution (cadr (must-place-row grid lst 1)) (car (must-place-row grid lst 1)))
                                                       (if (must-place-column grid lst 1)
                                                           (solution (cadr (must-place-column grid lst 1)) (car (must-place-column grid lst 1)))

                                                           (try solution lst grid (possible grid lst 1 1))
                                                           
                                                        )))
                                               #f)))


(define TENTS-SOLUTION (lambda (lst) (solution lst (make-possible (put-trees (make-grid (list-length (cadr lst)) (list-length (car lst))) (caddr lst)))) ))


(define find-x (lambda (lst) (list-length (cadr lst))))

(define find-y (lambda (lst) (list-length (car lst))))

(define line-test (lambda (length lst) (if (null? lst)
                                           #t
                                           (if (< length (- (* (car lst) 2) 1))
                                               #f
                                               (line-test length (cdr lst))))))

(define count-row-zeros (lambda (row) (if (null? row)
                                          0
                                          (if (eq? (car row) 0)
                                              (+ (count-row-zeros (cdr row)) 1)
                                              (count-row-zeros (cdr row))))))

(define count-zeros (lambda (grid) (if (null? grid)
                                       0
                                       (+ (count-row-zeros (car grid)) (count-zeros (cdr grid))))))

(define count-zeros-column (lambda (grid col) (if (null? grid)
                                                  0
                                                  (if (eq? (nth (car grid) col) 0)
                                                      (+ (count-zeros-column (cdr grid) col) 1)
                                                      (count-zeros-column (cdr grid) col)))))

(define zero-check (lambda (grid input) (if (< (count-zeros grid) (list-total (car input)))
                                            #f
                                            #t)))

(define is-any-negative (lambda (lst) (if (null? lst)
                                          #f
                                          (if (eq? (car lst) -1)
                                              #t
                                              (is-any-negative (cdr lst))))))

(define input-check (lambda (input) (if (is-any-negative (car input))
                                        #f
                                        (if (is-any-negative (cadr input))
                                            #f
                                            #t))))

(define find-grid-position (lambda (grid coordinate) (nth (nth grid (cadr coordinate)) (car coordinate))))
     
(define is-finished (lambda (lst) (if (eq? (list-total (car lst)) 0)
                                      (if (eq? (list-total (cadr lst)) 0)
                                          #t
                                          #f)
                                      #f)))
                      
(define is-false (lambda (grid input) (if (number-check input)
                                          (if (and (line-test (find-x input) (car input))
                                                   (line-test (find-y input) (cadr input)))
                                              (if (zero-check grid input)
                                                  (if (input-check input)
                                                      #t
                                                      #f)
                                                  
                                                  #f)
                                              #f)
                                          #f
                                          )))




  

                   