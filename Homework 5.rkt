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

;ListofNumbers(LoN) is one of:
;- empty
;- (cons Number LoN )
;list of numbers

(define LON-1 (cons 1 (cons 2 (cons 3 empty))))
(define LON-2 (cons 2 (cons 4 (cons 6 empty))))

(define (lon-temp l)
  (cond
    [(empty? l)...]
    [(cons? l)
     (first l)
     (lon-temp(rest l))]))
   

;; Listoflistofnumbers(lolon) is one of:
; - empty
; - (cons listofnumbers lolon)
;one list which is several lists of numbers inside of it

; (list (1 2) (3 6 8))
; (list 1 2 3 6 8)

;(cons 1 2)
;(cons 1 (cons 2 empty))

;(list 1 2 3) ==
;(cons 1 (cons 2 (cons 3 empty)))

;;ex 3
(define LoloN-1 (cons (cons 1 (cons 2 empty)) (cons (cons 3 (cons 6 (cons 8 empty))) empty)))
(define LoloN-2 (cons (cons 2 (cons 4 empty)) (cons (cons 4 (cons 10 (cons 12 empty))) empty)))

;list-of-lists: LoL-> ???
#;(define (LoL-temp lol)
    (cond
      [(empty? lol)...]
      [(cons? lol)
       (lon-temp(first lol))...
       (LoL-temp(rest lol))...]))

(check-expect (all-the-nums LoloN-1) 20)
(check-expect (all-the-nums LoloN-2) 32)
;all-the-nums: Lolon -> Number
;aggregates all the integers in each list to one number
(define (all-the-nums lolon)
  (cond [(empty? lolon) 0]
        [(cons? lolon)
         (+ (list-add (first lolon))
            (all-the-nums (rest lolon)))]))

(check-expect (list-add (cons 6 (cons 8 empty))) 14)
(check-expect (list-add (cons 5 (cons 3 (cons 7 empty)))) 15)
;list-add: ListofNumbers->Number
;gets a list and adds it's insides
(define(list-add l)
  (cond [(empty? l) 0]
        [(cons? l)
         (+ (first l)
            (list-add (rest l)))]))

;;ex 4 
(check-expect (flattens LoloN-1) (cons 1 (cons 2 (cons 3 (cons 6 (cons 8 empty))))))
(check-expect (flattens LoloN-2) (cons 2 (cons 4 (cons 4 (cons 10 (cons 12 empty))))))
;flattens: ListofLists -> List
;appends an entire list of lists into one list
(define (flattens lolon)
  (cond [(empty? lolon) empty]
        [(cons? lolon)
         (append (first lolon)
                 (flattens (rest lolon)))]))

;;ex 5

(define-struct slide [title shown hidden])
; A Slide is a (make-slide String LoS LoS)
; and represents a slide's title,
; what bullets have been shown (top to bottom)
; and which are hidden (top to bottom)

(define SLIDE-1 (make-slide ("cheese" (cons "blue" (cons "american" empty))
                                      (cons "goat" (cons "dairy-free" empty)))))
(define SLIDE-2 (make-slide ("cars" (cons "honda" (cons "ford" empty))
                                    (cons "nissan" (cons "lexus" empty)))))
(define SLIDE-3 (make-slide ("websites" (cons empty)
                                        (cons "youtube" (cons "facebook" (cons "twitter"))))))

(define (slide-temp st)
  (slide-title st) ...
  (los-temp (slide-shown st))...
  (los-temp (slide-hidden st)...))
 
; A Slideshow is one of:
; - empty
; - (cons Slide Slideshow)
; and represents an ordered slideshow

(define SLIDESHOW-1 (empty))
(define SLIDESHOW-2 (cons SLIDE-1 empty))
(define SLIDESHOW-3 (cons SLIDE-1 (cons SLIDE-2 empty)))
(define SLIDESHOW-3 (cons SLIDE-1 (cons SLIDE-2 (cons SLIDE-3 empty))))


(define (slideshow-temp st)
  (cond
    [(empty? st)...]
    [(cons? st)
     (first st)...
     (slideshow-temp(rest st))...]))

(define BLACK (rectangle 
;strong-singularity: World -> World
;this the big-bang for making a working slideshow
(define (strong-singularity slideshow)
  (big-bang slidshow
    [to-draw view-slide]
    [on-key right-key]
    [stop-when end BLACK]))

;;ex 6
;;a
;a feature is a number from [0, 255]
;0 is white/off, 1-254 is grey, 255 is black/on
(define FEATURE-1 0)
(define FEATURE-2 1)
(define FEATURE-3 254)
(define FEATURE-4 255)

;;b
;a bitmap

;;c

;;d

;;e