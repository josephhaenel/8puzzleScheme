;; Joseph Haenel
;; CS330 HW-8
;; 11/3/2023
;; Can do Easy-Hard, but cannot do Worst :(
;; Included print8puzzle function


(define (list-set lst index value)
  (define (helper lst idx acc)
    (cond ((null? lst) (reverse acc)) ; End of list, return reversed accumulator
          ((zero? idx) (append (reverse acc) (cons value (cdr lst)))) ; Index found, replace value
          (else (helper (cdr lst) (- idx 1) (cons (car lst) acc))))) ; Recurse to next element
  (helper lst index '()))

(define (8puzzle init goal)
  (let bfs ((queue (list (list init))) (visited '()))
    (cond ((null? queue) 'failure) ;; No solution if the queue is empty.
          ((equal? (car (car queue)) goal) (reverse (car queue))) ;; Solution found.
          (else
            (let* ((cur-path (car queue))
                   (cur-state (car cur-path))
                   (new-paths (foldl (lambda (op acc)
                                       (let ((new-state (op cur-state)))
                                         (if (and (not (member new-state (map car queue)))
                                                  (not (member new-state visited))
                                                  (not (null? new-state)))
                                             (cons (cons new-state cur-path) acc)
                                             acc)))
                                     '()
                                     (list up down left right))))
              (bfs (append (cdr queue) new-paths) (cons cur-state visited)))))))

(define op-list '(up down left right))

(define (extend lst)
  (foldl (lambda (op acc)
           (let ((new-state (op (caar lst))))
             (if (and (not (null? new-state)) (not (in new-state lst)))
                 (append acc (list (cons new-state (car lst))))
                 acc)))
         '()
         op-list))

(define (index-of-blank state)
  (let loop ((i 0))
    (if (= i (length state))
        -1
        (if (equal? 'B (list-ref state i))
            i
            (loop (+ i 1))))))

(define (swap list i j)
  (let ((temp (list-ref list i)))
    (list-set (list-set list i (list-ref list j)) j temp)))

(define (up state)
  (let ((blank-index (index-of-blank state)))
    (if (< blank-index 3)
        '() ;; Cannot move up, return empty list to signify invalid move.
        (swap state blank-index (- blank-index 3)))))

(define (down state)
  (let ((blank-index (index-of-blank state)))
    (if (>= blank-index 6)
        '() ;; Cannot move down, return empty list to signify invalid move.
        (swap state blank-index (+ blank-index 3)))))

(define (left state)
  (let ((blank-index (index-of-blank state)))
    (if (zero? (modulo blank-index 3))
        '() ;; Cannot move left, return empty list to signify invalid move.
        (swap state blank-index (- blank-index 1)))))

(define (right state)
  (let ((blank-index (index-of-blank state)))
    (if (= (modulo blank-index 3) 2)
        '() ;; Cannot move right, return empty list to signify invalid move.
        (swap state blank-index (+ blank-index 1)))))

(define (print-tile tile)
  (display (if (equal? tile 'B) "  " (format "~a " tile))))

(define (print-row state start-index)
  (print-tile (list-ref state start-index))
  (print-tile (list-ref state (+ start-index 1)))
  (print-tile (list-ref state (+ start-index 2)))
  (newline))

(define (print-state state)
  (print-row state 0)  ; Print first row
  (print-row state 3)  ; Print second row
  (print-row state 6)) ; Print third row

(define (print8puzzle solution)
  (if (null? solution)
      (display "No solution found.") ; Just in case
      (begin
        (let ((count 0))
          (for-each (lambda (state)
                      (print-state state)
                      (newline)
                      (set! count (+ count 1)))
                    solution)
          (display count)))))

