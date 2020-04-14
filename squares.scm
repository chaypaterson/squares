;;;; squares.scm
;;;; find a list of numbers that cannot be written as a sum of 2 squares

;;; overview:
;;; 1. generate list of pairs of integers close to a^2+b^2=n with b<a
;;; 2. scan this list for hits
;;; 3. For a range of numbers n, check if they have no hits.

;;; the algorithm can be viewed geometrically in terms of an abstract walker
;;; with a "position" and a "direction". This walker moves around on an
;;; integer lattice, taking one step forward each turn, and then turning left
;;; if it has walked uphill last turn, and right otherwise.
;;; The walker starts close to a target altitude, facing a direction that
;;; takes it above. It then wanders back and forth along the target altitude,
;;; occasionally looping back on itself, until an end condition is met.

;; A walker's state looks like this:
;; (list (list a b) (list dirx diry))
;;        ^ position ^ direction
;; The dynamics of the walk consist of a function that operates on the current
;; state and returns a new state. This function is applied until some end
;; condition is met.

;; we need the functions left and right from my postscript library

(load "/home/chay/Projects/lisp/postscript.scm")

(define (next-state walker hfn)
  (define next-posn 
    (map + (car walker) (cadr walker)))
  (list ; return a new walker state
    next-posn
    (if (> (hfn next-posn) 0)
        (left (cadr walker))
        (right (cadr walker)))))

;; functions to generate walks (lists of states)
;; the sequence of positions can be extracted with (map car walk)

(define (walk-for n w0 hfn)
  (define (walk-for-aux m w hfn so-far)
    (if (eq? m 0) so-far
      (walk-for-aux 
        (- m 1) 
        (next-state w hfn)
        hfn
        (cons (next-state w hfn) so-far))))
  (walk-for-aux n w0 hfn (list w0)))

(define (walk-until test w0 hfn) ; test must be a closure
  (define (walk-for-aux test w hfn so-far)
    (if (test w) ; test acts on w each turn
      so-far
      (walk-for-aux
        test
        (next-state w hfn)
        hfn
        (cons (next-state w hfn) so-far))))
  (walk-for-aux test w0 hfn (list w0)))
  ; ran first time B)

;; initial conditions for the problem in question:
;; and then make a list of all the numbers that cannot be expressed as the sum
;; of two squares

(define (initial-walker-state height)
  (list 
    (list (inexact->exact (floor (sqrt height))) 0) 
    (list 1 0))) ; todo make 5 argument

(define (height-function height)
  (lambda (posn) ; return a closure
    (define x (car posn))
    (define y (car (cdr posn)))
    (- (+ (* x x) (* y y)) height)))

;; a list of all candidates can now be generated with:
(define (candidates height)
  (map car (reverse
    (walk-until
      (lambda (w) (> (cadr (car w)) (car (car w))))
      (initial-walker-state height)
      (height-function height)))))

; the scaling of the number of candidates n with sqrt n can be verified with:
(define asymptot-coeff
  (lambda (x) (exact->inexact (/ (length (candidates x)) (sqrt x)))))
; appears to be approx 2 sqrt n. consistent with hard lower bound of sqrt(2
; n). interesting that the actual coefficient should be exactly 2?

(define (hit-list n)
  (define (aux-hit-list hfn popped lst)
    (if (eq? lst '()) popped
      (aux-hit-list hfn
        (if (eq? (hfn (car lst)) 0)
          (cons (car lst) popped)
          popped)
        (cdr lst))))
  (aux-hit-list (height-function n) '() (candidates n)))

(define (no-hits lst) ; expects list of integers
  (define (aux-no-hits dec acc)
    (if (eq? dec '()) 
      acc
      (aux-no-hits 
        (cdr dec)
        (if (eq? (length (hit-list (car dec))) 0)
          (cons (car dec) acc)
          acc))))
  (reverse (aux-no-hits lst '())))

;; an example range function:
(define (range n) 
  (define (auxrange m acc) 
    (if (eq? m 0) 
      acc 
      (auxrange (- m 1) (cons m acc)))) 
  (auxrange n (list)))
