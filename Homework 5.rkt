;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 5|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #t)))
;;ex 1

;A LoS is one of
; - empty
; - (cons String LoS)
; represents a list of strings
;Examples:
(define LOS1 empty)
(define LOS2 (cons "blue" LOS1))
(define LOS3 (cons "red" LOS2))
(define LOS4 (cons "blue" LOS3))
;Template
;los-temp: LoS -> ???
(define (los-temp lt)
  (cond
    [(empty? lt)...]
    [(cons? lt)(...(first lt)...
                   (los-temp (rest lt))...)]))

(check-expect (str-in-list-even?/v1 "blue" LOS4) true)
(check-expect (str-in-list-even?/v1 "red" LOS4) false)
(check-expect (str-in-list-even?/v1 "yellow" LOS4) true)
(check-expect (str-in-list-even?/v1 "blue" LOS1) true)
(check-expect (str-in-list-even?/v1 "red" LOS4) false)

;;zero is an even number
;;str-in-list-even?/v1: String LoS -> Boolean
;;takes in a target string and a list of strings and finds if the target appears even amount of times
;;in the LoS
(define (str-in-list-even?/v1 target LoS)
  (cond
    [(empty? LoS) true]
    [(cons? LoS) (= 0 (string-ticker target LoS))]))

;string-ticker: String LoS -> NatNumber
;takes in the target and list from the function above and helps it by getting the amount of times
;the string shows up
(define (string-ticker target LoS)
  (cond
    [(empty? LoS) 0]
    [(cons? LoS)
     (if(string=? target (first LoS)) (modulo (add1(string-ticker target (rest LoS))) 2)
        (modulo (string-ticker target (rest LoS)) 2))]))

;;ex 2
(check-expect (str-in-list-even?/v2 "blue" LOS4) true)
(check-expect (str-in-list-even?/v2 "red" LOS4) false)
(check-expect (str-in-list-even?/v2 "yellow" LOS4) true)
(check-expect (str-in-list-even?/v2 "blue" LOS1) true)
(check-expect (str-in-list-even?/v2 "red" LOS4) false)
;;str-in-list-even?/v2: String LoS -> Boolean
;;takes in a target string and a list of strings and finds if the target appears even amount of times
;;in the LoS

(define (str-in-list-even?/v2 target LoS)
  (cond
    [(empty? LoS) true]
    [(cons? LoS) (if (string=? target (first LoS)) (not (str-in-list-even?/v2 target (rest LoS)))
                                                    (str-in-list-even?/v2 target (rest LoS)))]))

;flipper: Boolean->Boolean
;takes in boolean and changes it state to the opposite
(define (flipper flop)
  (not flop))

;;ex 3

;;ex 4

;;ex 5

;;ex 6