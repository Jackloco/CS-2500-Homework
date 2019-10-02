;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
;;ex 1
(define-struct slope-intercept [m b])
;; A slope-intercept is a (make-slope-intercept Number Number)
;; - where m is the slope and a number
;; - and b is they value and a number

;; A Line is one of:
;; - (make-slope-intercept Number Number)
;; - Number
;; one is a slope intercepet is a sloped line while the other is a vertical line


;;ex 2
(define-struct monkey [name c others])
; A MonkeyChain is a (make-monkey String String MonkeyChain)
; and represents a collection of monkeys where:
; - name is the name of this monkey
; - c is the color of this monkey
; - others is the other monkeys it is attached to

;(define monkey1 (make-monkey "j" "blue" never-ending)
;the data definition will never end because MonkeyChain needs
;another MonkeyChain to keep it from crashing out

;;ex3
(define-struct delay [reason minutes])
; A TrainStatus is one of:
; - Integer
; - String
; - (make-delay String PosInt)
; - false
; and represents number of minutes away, a status message (e.g., "arriving"),
; minutes of delay and reason, or that the train has been cancelled

;delay-template: delay ->???
#;(define (delay-template d)
    (cond [(integer? d)...]
          [(string? d)...]
          [(delay? d)
           (... (delay-reason d)
                (delay-minutes d) ...)]
          [(false? d)...]))

;;announce: Number String TrainStatus -> String
;;takes in trainNumber destination and any status of the train to post a message of the train
(check-expect (announce 9 "Portland" 1) "Train 9 to Portland is 1 minutes away.")
(check-expect (announce 123 "Newark" "arriving") "Train 123 to Newark is arriving.")
(check-expect (announce 82 "Philadelphia"
                        "boarding") "Train 82 to Philadelphia is boarding.")
(check-expect (announce 76 "Chicago"
                        (make-delay "signaling error" "10"))
              "Train 76 to Chicago is delayed 10 minutes due to signaling error.")
(check-expect (announce 9348 "Boston" false) "Train 9348 to Boston is cancelled.")

(define (announce trnNum dest status)
  (string-append "Train " (number->string trnNum) " to " dest " is "
                 (cond [(integer? status) (string-append (number->string status) " minutes away")]
                       [(string? status) status]
                       [(delay? status)
                        (string-append "delayed " (delay-minutes status) " minutes due to "
                                       (delay-reason status))]
                       [(false? status) "cancelled"])"."))
;ex4
(define-struct red [ticks-left])
(define-struct blue [ticks-left])
 
; A ReflexGameState (RGS) is one of:
; - (make-red Nat)
; - (make-blue Nat)
; and represents the current state of the game,
; either red or blue, and how many ticks are left
; to show in that state before switching to the other

#;(define (ReflexGameState-temp state)
    (cond [(red? state)
           (red-ticks-left state)...]
          [(blue? state)
           (blue-ticks-left state)...]))

(define RF1 (make-red 140))
(define RF2 (make-blue 14))
(define red-screen (rectangle 500 500 "solid" "red"))
(define blue-screen (rectangle 500 500 "solid" "blue"))

;relfexes: ReflexGameState -> reflexes
;makes an image for each RGS until it's own tick time runs out, then switches to next
(define (reflexes state)
    (big-bang state
      [on-tick compute-next]
      [on-key any-key]
      [stop-when pressed-key]
      [to-draw red-blue-red]))

;compute-next: state->state
;counts down tick and flips to other tick countdown when the current one hits zero
(check-expect(compute-next (make-red 0)) (make-blue 14))
(check-expect(compute-next (make-blue 0)) (make-red 140))
(check-expect(compute-next (make-red 134)) (make-red 133))
(check-expect(compute-next (make-blue 12)) (make-blue 11))
(define (compute-next state)
  (cond [(red? state)
         (if (> (red-ticks-left state) 0)
             (make-red (- (red-ticks-left state) 1))
             (make-blue 14))]
        [(blue? state)
         (if (> (blue-ticks-left state) 0)
             (make-blue (- (blue-ticks-left state) 1))
             (make-red 140))]))
;any-key: World -> World
;will switch state from RGS to boolean if key is pressed
(check-expect (any-key true "w") true)
(check-expect (any-key false "e") false)
(check-expect (any-key (make-red 67) "r") false)
(check-expect (any-key (make-blue 4) "t") true)
(define (any-key state a-key)
  (cond
    [(boolean? state) state]
    [else (blue? state)]))
;pressed-key: World->World
;this will only work if the world state has been changed to a boolean, false or true will kill
(check-expect (pressed-key true) true)
(check-expect (pressed-key false) true)
(define (pressed-key state)
  (boolean? state))
  ;;(cond
    ;;[(boolean? state) true]
    ;;[else state]))

;red-blue-red: World -> World
;will draw red or blue depending on which color is present in state
(check-expect (red-blue-red (make-red 44)) red-screen)
(check-expect (red-blue-red (make-blue 10)) blue-screen)
(define (red-blue-red state)
  (cond
    [(red? state) red-screen]
    [(blue? state) blue-screen]))

;ex5
; A Posn is a (make-posn Number Number)
; and represents a 2D position
 
(define-struct circ [radius center])
(define-struct sq [side center])


; A Shape is one of...
; - (make-circ PosNumber Posn)
; - (make-sq PosNumber Posn)
; and represents a cirle with a radius and center
; or a square with side length and center

#;(define (shape-temp radl center)
  (cons
   [(circ? radl center)...]
   [(sq? radl center)...]))

#;(define (blink state)
    (big-bang state
      [on-mouse down-press]
      [to-draw on-screen]))

(define (down-press state x y event)
  (string=? event "button-up"))

(define (on-screen ))
  

;ex 6
(define-struct building [width height color right])
; A Skyline is one of:
; - false
; - (make-building PosInteger PosInteger String Skyline) 
; and represents either an empty skyle or a building with
; a width and height in pixels, the color of the building, and the
; rest of the skyline to the right of the building
