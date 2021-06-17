#lang scheme
;2019400252

(define (RETURN-FIRST-NOT-FALSE f lis)(cond ((null? lis) #F)
                                            ((equal? (f (car lis)) #F) (RETURN-FIRST-NOT-FALSE f (cdr lis)))
                                            (else (f (car lis)))))

(define (ADJACENT lis1 lis2)(cond ((and (eq? (car lis1) (car lis2)) (eq? (cadr lis1) (cadr lis2)))  #T) ;(x,y) (x,y)
                                  ((and (eq? (car lis1) (car lis2)) (or (eq? 1 (- (cadr lis1) (cadr lis2))) (eq? 1 (- (cadr lis2) (cadr lis1))))) #T) ;(x,y+1) (x,y) || (x,y) (x,y+1)
                                  ((and (eq? (cadr lis1) (cadr lis2)) (or (eq? 1 (- (car lis1) (car lis2))) (eq? 1 (- (car lis2) (car lis1))))) #T)   ;(x+1,y) (x,y) || (x,y) (x+1,y)
                                  ((and (eq? 1 (- (car lis1) (car lis2))) (eq? 1 (- (cadr lis1) (cadr lis2)))) #T) ;(x+1,y+1) (x,y)
                                  ((and (eq? 1 (- (car lis1) (car lis2))) (eq? 1 (- (cadr lis2) (cadr lis1)))) #T) ;(x+1,y) (x,y+1)
                                  ((and (eq? 1 (- (car lis2) (car lis1))) (eq? 1 (- (cadr lis2) (cadr lis1)))) #T) ;(x,y) (x+1,y+1)
                                  ((and (eq? 1 (- (car lis2) (car lis1))) (eq? 1 (- (cadr lis1) (cadr lis2)))) #T) ;(x,y+1) (x+1,y)
                                  (else #F)))

(define (NEIGHBOR-LIST lis)(cond ((and (> (car lis) 1) (> (cadr lis) 1)) (list (list (+ 1 (car lis)) (cadr lis))   ;(x+1,y)
                                                                             (list (- (car lis) 1) (cadr lis))   ;(x-1,y)
                                                                             (list (car lis) (+ 1 (cadr lis)))   ;(x,y+1)
                                                                             (list (car lis) (- (cadr lis) 1)))) ;(x,y-1)
                                 ((and (not (> (car lis) 1)) (> (cadr lis) 1)) (list (list (+ 1 (car lis)) (cadr lis))   ;(x+1,y)
                                                                             (list (car lis) (+ 1 (cadr lis)))   ;(x,y+1)
                                                                             (list (car lis) (- (cadr lis) 1)))) ;(x,y-1)
                                 ((and (> (car lis) 1) (not (> (cadr lis) 1))) (list (list (+ 1 (car lis)) (cadr lis))   ;(x+1,y)
                                                                             (list (- (car lis) 1) (cadr lis))   ;(x-1,y)
                                                                             (list (car lis) (+ 1 (cadr lis)))))   ;(x,y+1)
                                 ((not (or (> (car lis) 1) (> (cadr lis) 1))) (list (list (+ 1 (car lis)) (cadr lis))   ;(x+1,y)
                                                                             (list (car lis) (+ 1 (cadr lis)))))))   ;(x,y+1)

(define (NEIGHBOR-LIST-OF-LIST lis)(cond ((null? lis) '())
                                         (else (append (NEIGHBOR-LIST (car lis)) (NEIGHBOR-LIST-OF-LIST (cdr lis)) ))))

(define (ADJACENT-WITH-LIST lis biglis) (if (null? biglis) #F (or (member (car biglis) (NEIGHBOR-LIST lis)) (ADJACENT-WITH-LIST lis (cdr biglis)))))

(define (REPLACE-NTH lis n num)(if (eq? 1 n) (cons num (cdr lis)) (cons (car lis) (REPLACE-NTH (cdr lis) (- n 1) num))))

;gets the available blocks to put tents,then eliminates the row and coloumns which has no need to put tent, then calls the CHECK-ALWAYS function
(define (TENTS-SOLUTION lis)(CHECK-ALWAYS (caddr lis) (car lis) (cadr lis) (DELETE-ZERO-COLOUMNS 1 (cadr lis) (DELETE-ZERO-ROWS 1 (car lis) (GET-BLOCKS 1 lis))) '()))

;decrements the nth element of the list by 1
(define (DEC lis n)(if (eq? 1 n) (cons (- (car lis) 1) (cdr lis)) (cons (car lis) (DEC (cdr lis) (- n 1) ))))

;for given list  of blocks returns the list of blocks which belongs to row i
(define (TAKE-ELEMENTS-ROW i lis)(cond ((null? lis) '())
                                   ((eq? i (car (car lis))) (cons (car lis) (TAKE-ELEMENTS-ROW i (cdr lis))))
                                   (else (TAKE-ELEMENTS-ROW i (cdr lis)))))

;for given list  of blocks returns the list of blocks which belongs to coloumn i
(define (TAKE-ELEMENTS-COL i lis)(cond ((null? lis) '())
                                   ((eq? i (cadr (car lis))) (cons (car lis) (TAKE-ELEMENTS-COL i (cdr lis))))
                                   (else (TAKE-ELEMENTS-COL i (cdr lis)))))

;if any two blocks are adjacents return false
(define (CHECK-IF-TENTS-ARE-ADJACENTS lis)(cond ((null? lis) #F)
                                                ((ADJACENT-WITH-LIST(car lis) (cdr lis)) #T)
                                                (else (CHECK-IF-TENTS-ARE-ADJACENTS (cdr lis)))))

;returns all the available places to put tent starting from coloumn 1, list of those available places generally referred as lis in rest of the program
(define (GET-BLOCKS j lis)(if (eq? j (+ 1 (length (cadr lis)))) '() (append (GET-COORDINATES 1 j lis) (GET-BLOCKS (+ 1 j) lis))))

;returns the available places to put tent in row i, it eliminates the one that are not neighbor to a tree
(define (GET-COORDINATES i j lis)(cond ((eq? i (+ 1 (length (car lis)))) '())
                                     (( and (or (< i (length (car lis))) (eq? i (length (car lis))))
                                            (not (member (list i j) (caddr lis)))  (member (list i j) (NEIGHBOR-LIST-OF-LIST (caddr lis))))
                                      (cons (list i j) (GET-COORDINATES (+ 1 i) j lis)))
                                     (else (GET-COORDINATES (+ 1 i) j lis))))

;if the number of tents will be put is 0, eliminates the blocks in that row in lis (list of available blocks to put tent)
(define (DELETE-ZERO-ROWS i rowinput lis)(cond ((null? rowinput) lis)
                                         ((eq? 0 (car rowinput)) (DELETE-ZERO-ROWS (+ 1 i) (cdr rowinput) (DELETE-ROW i lis) ))
                                         (else (DELETE-ZERO-ROWS (+ 1 i) (cdr rowinput) lis))))

;if the number of tents will be put is 0, eliminates the blocks in that coloumn in lis
(define (DELETE-ZERO-COLOUMNS j coloumninput lis)(cond ((null? coloumninput) lis)
                                         ((eq? 0 (car coloumninput)) (DELETE-ZERO-COLOUMNS (+ 1 j) (cdr coloumninput) (DELETE-COLOUMN j lis) ))
                                         (else (DELETE-ZERO-COLOUMNS (+ 1 j) (cdr coloumninput) lis))))

;checks all the rows if there is a row that the number of tents will be put is 0, calls the DELETE-ZERO-ROWS function
(define (DELETE-ROW i lis)(cond ((null? lis) '())
                                ((eq? i (car (car lis))) (DELETE-ROW i (cdr lis)))
                                (else (cons (car lis) (DELETE-ROW i (cdr lis))))))

;;checks all the coloumns if there is a coloumn that the number of tents will be put is 0, calls the DELETE-ZERO-COLOUMNS function
(define (DELETE-COLOUMN i lis)(cond ((null? lis) '())
                                ((eq? i (cadr (car lis))) (DELETE-COLOUMN i (cdr lis)))
                                (else (cons (car lis) (DELETE-COLOUMN i (cdr lis))))))

(define (DELETE-ROW-TENTS j lis copy tents)(cond ((null? lis) (list copy tents))
                                ((eq? j (car (car lis))) (list (intersection (DELETE-ADJ (car lis) copy) (car (DELETE-ROW-TENTS j (cdr lis) copy (cons (car lis) tents)))) (cadr (DELETE-ROW-TENTS j (cdr lis) copy (cons (car lis) tents)))))
                                (else (DELETE-ROW-TENTS j (cdr lis) copy tents))))

(define (DELETE-COLOUMN-TENTS j lis copy tents)(cond ((null? lis) (list copy tents))
                                ((eq? j (cadr (car lis))) (list (intersection (DELETE-ADJ (car lis) copy) (car (DELETE-COLOUMN-TENTS j (cdr lis) copy (cons (car lis) tents)))) (cadr (DELETE-COLOUMN-TENTS j (cdr lis) copy (cons (car lis) tents) ))))
                                (else (DELETE-COLOUMN-TENTS j (cdr lis) copy tents))))

;if any block is picked to put tent, it deletes it adjacents from the list also
(define (DELETE-ADJ lis biglis)(cond ((null? biglis) '())
                                    ((ADJACENT lis (car biglis)) (DELETE-ADJ lis (cdr biglis)))
                                    (else (cons (car biglis) (DELETE-ADJ lis (cdr biglis))))))

;returns the number of blocks in lis for the given row number
(define (ROW-HOW-MANY i lis)(cond ((null? lis) 0)
                                  ((eq? i (car (car lis))) (+ 1 (ROW-HOW-MANY i (cdr lis))))
                                  (else (ROW-HOW-MANY i (cdr lis)))))

;returns the number of blocks in lis for the given coloumn number
(define (COL-HOW-MANY i lis)(cond ((null? lis) 0)
                                  ((eq? i (cadr (car lis))) (+ 1 (COL-HOW-MANY i (cdr lis))))
                                  (else (COL-HOW-MANY i (cdr lis)))))

;if there are only available blocks in lis as the same number in row-input which declares how many tents will be put and if the blocks are not adjacents, places the tents in that blocks
(define (CHECK-ROW i inputrow lis tents)(cond ((or (null? inputrow) (null? lis)) (list lis tents))
                                              ((> (car inputrow) (howMany (TAKE-ELEMENTS-ROW i lis))) #F)
                                              ((and (eq? (car inputrow) (ROW-HOW-MANY i lis)) (CHECK-IF-TENTS-ARE-ADJACENTS (TAKE-ELEMENTS-ROW i lis))) #F)
                                              ((and (eq? (car inputrow) (ROW-HOW-MANY i lis)) (not (CHECK-IF-TENTS-ARE-ADJACENTS (TAKE-ELEMENTS-ROW i lis))))  (CHECK-ROW (+ 1 i) (cdr inputrow) (car (DELETE-ROW-TENTS i lis lis tents)) (cadr (DELETE-ROW-TENTS i lis lis tents))))
                                              (else (CHECK-ROW (+ 1 i) (cdr inputrow) lis tents))))

;if there are only available blocks in lis as the same number in coloumn-input which declares how many tents will be put and if the blocks are not adjacents, places the tents in that blocks
(define (CHECK-COLOUMN i coloumninput lis tents)(cond ((or (null? coloumninput) (null? lis)) (list lis tents))
                                                      ((> (car coloumninput) (howMany (TAKE-ELEMENTS-COL i lis))) #F)
                                                      ((and (eq? (car coloumninput) (COL-HOW-MANY i lis)) (CHECK-IF-TENTS-ARE-ADJACENTS (TAKE-ELEMENTS-COL i lis))) #F)
                                                      ((and (eq? (car coloumninput) (COL-HOW-MANY i lis)) (not (CHECK-IF-TENTS-ARE-ADJACENTS (TAKE-ELEMENTS-COL i lis)))) (CHECK-COLOUMN (+ 1 i) (cdr coloumninput) (car (DELETE-COLOUMN-TENTS i lis lis tents)) (cadr (DELETE-COLOUMN-TENTS i lis lis tents)))) ;may be useless
                                                      (else (CHECK-COLOUMN (+ 1 i) (cdr coloumninput) lis tents))))

(define (CHECK-TREES trees lis tents)(cond ((null? trees) (list lis tents))
                                           ((and (eq? 0 (howMany (intersection tents (NEIGHBOR-LIST (car trees))))) (eq? 0 (howMany (intersection lis (NEIGHBOR-LIST (car trees)))))) #F) ;if there is no tent adjacent to tree and there is no available place to put return #F
                                           ((and (eq? 0 (howMany (intersection tents (NEIGHBOR-LIST (car trees))))) (eq? 1 (howMany (intersection lis (NEIGHBOR-LIST (car trees))))))     ;if there is only one available place to put tent, update the lis and tents
                                            (CHECK-TREES (cdr trees) (DELETE-ADJ (car (intersection lis (NEIGHBOR-LIST (car trees)))) lis) (cons (car (intersection lis (NEIGHBOR-LIST (car trees)))) tents)))
                                           (else (CHECK-TREES (cdr trees) lis tents))))

;it calls the check-row, check-coloumn and check-trees until no need to place tent, or false, or no changes occurs with this method, if so it calls the BRUTE
(define (CHECK-ALWAYS trees rowinput colinput lis tents)(cond ((equal? #F (CHECK-TREES trees lis tents)) #F)
                                                              ( (and (and (ISALLZERO rowinput) (ISALLZERO colinput) ) (not (eq? (howMany trees) (howMany tents)))) #F)
                                                          ( (and (ISALLZERO rowinput) (ISALLZERO colinput) (eq? (howMany trees) (howMany tents)) ) tents)
                                                        ((equal? #F (CHECK-ROW 1 rowinput lis tents)) #F)
                                                        ((equal? #F (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents)))) #F)
                                                        ((equal? #F (CHECK-TREES  trees (car (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))
                                                                                             (cadr (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents)))))) #F)
                                                        ((eq? tents (cadr (CHECK-TREES  trees (car (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))
                                                                                             (cadr (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents)))))))
                                                         (BRUTE trees rowinput colinput lis tents (SORT-ITEMS lis rowinput colinput)))
                                                        (else (CHECK-ALWAYS trees (update-row rowinput (cadr (CHECK-TREES  trees (car (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))
                                                                                             (cadr (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents)))))) tents)
                                                                    (update-col colinput (cadr (CHECK-TREES  trees (car (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))
                                                                                             (cadr (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents)))))) tents)
                                                                    (car (CHECK-TREES  trees (car (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))
                                                                                             (cadr (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))))
                                                                    (cadr (CHECK-TREES  trees (car (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))
                                                                                             (cadr (CHECK-COLOUMN 1 (update-col colinput (cadr (CHECK-ROW 1 rowinput lis tents)) tents) (car (CHECK-ROW 1 rowinput lis tents)) (cadr (CHECK-ROW 1 rowinput lis tents))))))))))

;gives priority to blocks which has the least distance, distance is difference between the number of available blocks to put tent in a row/coloumn and number of tents need to be placed in that row/coloumn
(define (SORT-ITEMS lis rowinput colinput)(sort-by-distance 10 (append (distance-row lis rowinput lis) (distance-col lis colinput lis))))

;if there is no accurate way to put tent, picks the first block from sorted list, if it is false tries another block from the list
(define (BRUTE trees rowinput colinput lis tents lis2)(cond ((null? lis2) #F)
                                                                ((equal? #F (CHECK-ALWAYS trees (DEC rowinput (car (car lis2))) (DEC colinput (cadr (car lis2))) (DELETE-ZERO-COLOUMNS 1 (DEC colinput (cadr (car lis2))) (DELETE-ZERO-ROWS 1 (DEC rowinput (car (car lis))) (DELETE-ADJ (car lis2) lis))) (cons (car lis2) tents)))
                                                                  (BRUTE trees rowinput colinput lis tents (cdr lis2)))
                                                                (else (CHECK-ALWAYS trees (DEC rowinput (car (car lis2))) (DEC colinput (cadr (car lis2))) (DELETE-ADJ (car lis2) lis) (cons (car lis2) tents)))))

;if there new tents are placed rowinput is updated, decrementing the tent's row
(define (update-row row biglis lis)(cond ((null? biglis) row)
                                  ((not (member (car biglis) lis)) (update-row (DEC row (car (car biglis))) (cdr biglis) lis))
                                  (else (update-col row (cdr biglis) lis))))

;if there new tents are placed coloumn input is updated, decrementing the tent's coloumn
(define (update-col col biglis lis)(cond ((null? biglis) col)
                                  ((not (member (car biglis) lis)) (update-col (DEC col (cadr (car biglis))) (cdr biglis) lis))
                                  (else (update-col col (cdr biglis) lis))))

;checks if all elements are zero of the list
(define (ISALLZERO lis)(cond ((null? lis) #T)
                             ((not (eq? 0 (car lis))) #F)
                             (else (ISALLZERO (cdr lis)))))

;returns the number of elements of a list
(define (howMany list) (if (null? list) 0 (+ 1 (howMany (cdr list)))))

;returns the intersection of two list
(define (intersection a b) (if (null? a) '() (if (member (car a) b) (cons (car a)(intersection (cdr a) b)) (intersection (cdr a) b))))

;insert the item to the end of the list
(define (insert-to-end lis item)(if (null? lis) (list item) (cons (car lis) (insert-to-end (cdr lis) item))))

;appends first list to the end of the second list
(define (append-to-end lis1 lis2)(cond ((null? lis2) lis1)
                                       ((member (car lis2) lis1) (append-to-end lis1 (cdr lis2)))
                                       (else (append-to-end (insert-to-end lis1 (car lis2)) (cdr lis2)))))

;calculates the distance of the blocks according to row 
(define (distance-row lis rowinput copylis)(if (null? lis) '() (cons (cons (- (ROW-HOW-MANY (car (car lis)) copylis) (nth-item (car (car lis)) rowinput)) (car lis)) (distance-row (cdr lis) rowinput copylis))))

;calculates the distance of the blocks according to coloumn 
(define (distance-col lis colinput copylis)(if (null? lis) '() (cons (cons (- (COL-HOW-MANY (cadr (car lis)) copylis) (nth-item (cadr (car lis)) colinput)) (car lis)) (distance-col (cdr lis) colinput copylis))))

(define (take-lis i lis)(cond ((null? lis) '())
                              ((eq? i (car (car lis))) (cons (cdr (car lis)) (take-lis i (cdr lis))))
                              (else (take-lis i (cdr lis)))))

(define (sort-by-distance i lis)(if (eq? i 0) (take-lis 0 lis) (append-to-end (take-lis i lis) (sort-by-distance (- i 1) lis))))

(define (nth-item n lis)(if (eq? 1 n) (car lis) (nth-item (- n 1) (cdr lis))))
