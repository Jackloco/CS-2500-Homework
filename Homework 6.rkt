;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

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

(define SKY-WIDTH 300)
(define SKY-HEIGHT 200)
(define RADIUS 25)
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "grey"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
; eclipse : Number -> Image
; draws a moon at a given x position in the sky
(define (eclipse x-moon)
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) SKY)))

; my-animate : [Number -> Image] -> Number
; animates the provided function by changing the image every frame
(define (my-animate f)
  (big-bang 0
    [to-draw f]
    [on-tick add1]))

;ex 3

; a NonEmptyListOfStrings is a
; (cons String ListOfStrings)
; representing any ListOfStrings that is not empty
(define NELOS-1 (cons "goodbye" '()))
(define NELOS-2 (cons "hello" NELOS-1))
(define NELOS-3 (cons "apple" NELOS-2))
(define NELOS-4 (cons "banana" NELOS-3))
#; (define (nelos-temp nelos)
     (... (first nelos)...
          (nelos-temp (rest nelos))...))

; earliest : NonEmptyListOfStrings [String String -> Boolean] -> String
; takes a non-empty ListOfStrings and a function that returns if the first string comes "before" the
; second in the non-empty ListOfStrings
(check-expect (earliest NELOS-4 string<?) "apple")
(check-expect (earliest NELOS-4 string>?) "hello")
(check-expect (earliest NELOS-4 string=?) "goodbye")

(define (earliest los f)
 (cond
   [(empty? (rest los)) (first los)]
   [(f (first los) (earliest (rest los) f)) (first los)]
   [else (earliest (rest los) f)]))

; earliest-lex : NonEmptyListOfStrings -> String
; determines the string that comes first lexographically
(check-expect (earliest-lex NELOS-4) "apple")
(check-expect (earliest-lex (cons "red" (cons "green" (cons "blue" '())))) "blue")
(define (earliest-lex los)
  (earliest los string<?))

; last-lex : NonEmptyListOfStrings -> String
; determines the string that comes last lexographically
(check-expect (last-lex NELOS-4) "hello")
(check-expect (last-lex (cons "z" (cons "b" (cons "y" '())))) "z")
(define (last-lex los)
  (earliest los string>?))

; last-string : NonEmptyListOfStrings -> String
; returns the final string in the list
(check-expect (last-string (cons "apple" (cons "apple" (cons "banana" '())))) "banana")
(check-expect (last-string NELOS-3) "goodbye")
(define (last-string los)
  (earliest los string=?))

;ex 4
