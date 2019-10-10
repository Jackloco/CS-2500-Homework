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

(check-expect (flipper false) true)
(check-expect (flipper true) false)

;flipper: Boolean->Boolean
;takes in boolean and changes it state to the opposite
(define (flipper flop)
  (not flop))

;; List of list of numbers is one of:
; - empty
; - (cons LoN LoLoN)

; (list (1 2) (3 6 8))
; (list 1 2 3 6 8)
; (

;(cons 1 2)
;(cons 1 (cons 2 empty))

;(list 1 2 3) ==
;(cons 1 (cons 2 (cons 3 empty)))

;;ex 3
(define LoN-1 (cons (cons 1 (cons 2 empty)) (cons (cons 3 (cons 6 (cons 8 empty))) empty)))
(define LoN-2 (cons (cons 2 (cons 4 empty)) (cons (cons 4 (cons 10 (cons 12 empty))) empty)))

;list-of-lists: LoL-> ???
#;(define (LoL-temp lol)
  (cond
    [(empty? lol)...]
    [(cons? lol)
     (list-temp(first lol))...
     (LoL-temp(rest lol))...]))

(check-expect (all-the-nums LoN-1) 20)
(check-expect (all-the-nums LoN-2) 32)
;all-the-nums: List of Lists -> Number
;aggregates all the integers in each list to one number
(define (all-the-nums lolon)
  (cond [(empty? lolon) 0]
        [(cons? lolon)
         (+ (list-add (first lolon))
               (all-the-nums (rest lolon)))]))

(check-expect (list-add (cons 6 (cons 8 empty))) 14)
(check-expect (list-add (cons 5 (cons 3 (cons 7 empty)))) 15)
;list-add: List->Number
;gets a list and adds it's insides
(define(list-add l)
  (cond [(empty? l) 0]
        [(cons? l)
         (+ (first l)
            (list-add (rest l)))]))

;;ex 4 
(check-expect (flattens LoN-1) (cons 1 (cons 2 (cons 3 (cons 6 (cons 8 empty))))))
(check-expect (flattens LoN-2) (cons 2 (cons 4 (cons 4 (cons 10 (cons 12 empty))))))
;flattens: List of Lists -> List
;appends an entire list of lists into one list
(define (flattens lolon)
  (cond [(empty? lolon) empty]
        [(cons? lolon)
         (string-append (number->string(first lolon))
               (flattens (rest lolon)))]))

;;ex 5

;;ex 6