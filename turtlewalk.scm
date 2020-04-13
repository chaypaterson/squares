;;;; turtle-walk
;;; visualises the behaviour of the candidate selection algorithm in
;;; squares.scm

(load "squares.scm")

;; this pulls in postscript.scm. Also,

;; use "line" to draw geometrical shapes
;; given a list of (x y) positions, each turn "start" should be car walk
;; and "end" should be cadr walk. next turn we should do the same with cdr
;; walk.
;; if walk is empty, do nothing.
;; if walk only has one entry, this is the end. do nothing.

(define (trace-walk walk out)
  (if (not (or (eq? walk '()) (eq? (cdr walk) '())))
    (begin
      (line (car walk) (cadr walk) out)
      (trace-walk (cdr walk) out))))

;; what to do: generate a list of candidates, translate this to a sequence of
;; positions, and trace this sequence with lines in a postscript file

(define test-walk 
  (map ; magnify this walk
    (lambda (sblst) (map (lambda (x) (* x 10)) sblst)) 
    (candidates 100)))

;; writing the file:

(define page (open-output-file "turtle.ps"))

(define width 100)
(define height 100)
(size-page width height page)

(trace-walk test-walk page)

(show-page page)
(close-output-port page)

(exit)
