;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 6|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #t)))
;ex 1
;a

; distance-to-origin : Posn -> Number
; Produces the distance from this position to the origin
(check-within (distance-to-origin (make-posn 2 2)) (sqrt 8) 1e-06)
(check-expect (distance-to-origin (make-posn 3 4)) 5)
(define (distance-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

; map1 : [List-of Posn] Number [Number Number -> Number] [Posn -> Number] -> Number
; either sums all the x-coordinates in a List of Positions
; or multiplies all the distances from each position to the origin
(define LOP-1 (cons (make-posn 3 4) (cons (make-posn 5 12) empty)))
(check-expect (map1 empty 0 + posn-x) 0)
(check-expect (map1 LOP-1 0 + posn-x) 8)
(check-expect (map1 empty 1 * distance-to-origin) 1)
(check-expect (map1 LOP-1 1 * distance-to-origin) 65)

(define (map1 lop k f g)
  (cond
    [(empty? lop) k]
    [(cons? lop)
     (f (g (first lop))
        (map1 (rest lop) k f g))]))

; sum-x-coords : [List-of Posn] -> Number
; returns the sum of all the x-coordinates in the list of positions
(check-expect (sum-x-coords empty) 0)
(check-expect (sum-x-coords
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 8)
(define (sum-x-coords lop)
  (map1 lop 0 + posn-x))

; mult-distances : [List-of Posn] -> Number
; multiplies all the distances from each position to the origin
(check-expect (mult-distances empty) 1)
(check-expect (mult-distances
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 65)
(define (mult-distances lop)
  (map1 lop 1 * distance-to-origin))

;b
; difference-of-posn : Posn -> Number
; returns the difference between the x and y value in a Posn
(check-expect (difference-of-posn (make-posn 4 3)) 1)
(check-expect (difference-of-posn (make-posn 5 12)) 7)
(check-expect (difference-of-posn (make-posn 6 6)) 0)
(define (difference-of-posn p)
  (abs (- (posn-x p) (posn-y p))))

; biggest-difference : [List-of Posn] -> Number
; produces the biggest difference between the x and y value in a List of Positions
(check-expect (biggest-difference LOP-1) 7)
(check-expect (biggest-difference empty) 0)
(define (biggest-difference lop)
  (map1 lop 0 max difference-of-posn))

;ex 2

; my-animate : [Number -> Image] ->
(define (my-animate )
  (big-bang x

;ex 3

;ex 4