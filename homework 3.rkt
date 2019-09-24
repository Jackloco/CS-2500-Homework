;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |homework 3|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
; loki : PosInt PosInt PosInt PosInt -> Boolean
; Determines if the base, plus a random number in a bound, is even and within a threshold
(define (loki base random-upper too-small too-big)
  (and (even? (+ base (random random-upper)))
       (> (+ base (random random-upper)) too-small)
       (< (+ base (random random-upper)) too-big)))
;;the random is being called three times. the randoms will not have the same seed giving wildly
;;different answers

; A Fish is a (make-fish String String NonNegNumber)
(define-struct fish [species color weight])
; and represents a fish's species, color, and weight in grams

(define fish_light(make-fish "salmon" "white" 4))
(define fish_medium(make-fish "herring" "black" 10))
(define fish_heavy(make-fish "dolphin" "blue" 100))


;;print-fish: Fish-> String
;;Fish needs to be predefined

;;make-fish: String String Number -> (make-fish String String Number)


;;PiSamples: Number Number -> String
;;takes in x and y and concants them
(define (PiSamples X Y)
  ((string-append(number->string X)(number->string Y))))

