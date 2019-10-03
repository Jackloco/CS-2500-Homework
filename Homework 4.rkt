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

;A State is one of:
;- RGS
;- Boolean
;state can either be an RGS struct or a boolean

#;(define (state-temp state)
    (cond [((or (red? state) (blue? state)))...]
          [(boolean? state)...]))

(define state1 (make-red 140))
(define state2 (make-blue 14))
(define state3 false)
(define state4 true)

;relfexes: State -> State
;makes an image for each RGS until it's own tick time runs out, then switches to next

(define (reflexes state)
  (big-bang state
    [on-tick compute-next]
    [on-key any-key]
    [stop-when pressed-key]
    [to-draw red-blue-red]))

;compute-next: State->State
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
;any-key: State KeyEvent -> State
;will switch state from RGS to boolean if key is pressed
(check-expect (any-key true "w") true)
(check-expect (any-key false "e") false)
(check-expect (any-key (make-red 67) "r") false)
(check-expect (any-key (make-blue 4) "t") true)
(define (any-key state a-key)
  (cond
    [(boolean? state) state]
    [else (blue? state)]))
;pressed-key: State -> State
;this will only work if the world state has been changed to a boolean, false or true will kill
(check-expect (pressed-key true) true)
(check-expect (pressed-key false) true)
(define (pressed-key state)
  (boolean? state))
;;(cond
;;[(boolean? state) true]
;;[else state]))

;red-blue-red: State -> State
;will draw red or blue depending on which color is present in state
(check-expect (red-blue-red (make-red 44)) red-screen)
(check-expect (red-blue-red (make-blue 10)) blue-screen)
(define (red-blue-red state)
  (cond
    [(red? state) red-screen]
    [(blue? state) blue-screen]))

;ex5
; (define-struct posn [x-pos y-pos])
; A Posn is a (make-posn Number Number)
; and represents a 2D position

(define POSN-0 (make-posn 0 0))
(define POSN-1 (make-posn 1 3))
(define POSN-2 (make-posn 4 5))

; posn-temp : Posn -> ???
#;(define (posn-temp posn)
    (... (posn-xpos posn)...
         (posn-ypos posn)...))


(define-struct circ [radius center])
; A Circle is a (make-circ Number Posn)
; and represents the size and location of a circle
(define CIRC-1 (make-circ 10 POSN-0))
(define CIRC-2 (make-circ 5 POSN-2))

; circ-temp : Circle -> ???
#;(define (circ-temp c)
    (... (circ-radius c)...
         (posn-xpos (circ-center c))...
         (posn-ypos (circ-center c))...))

(define-struct sq [side center])
; A Square is a (make-sq Number Posn)
; and represents the size and location of a square
(define SQ-1 (make-sq 4 POSN-1))
(define SQ-2 (make-sq 20 POSN-0))

; square-temp : Square -> ???
#;(define (square-temp s)
    (... (sq-side s)...
         (posn-xpos (sq-center s))...
         (posn-ypos (sq-center s))...))


; A Shape is one of...
; - (make-circ PosNumber Posn)
; - (make-sq PosNumber Posn)
; and represents a cirle with a radius and center
; or a square with side length and center

(define SHAPE-1 CIRC-1)
(define SHAPE-2 SQ-1)

; shape-temp : Shape -> ???
#;(define (shape-temp radl center)
    (cons
     [(circ? radius center)...]
     [(sq? side center)...]))

;a State is a (make-st Shape Boolean)
(define-struct st [shape fill])

#;(define (shape-state-temp state)
    (shape-temp (shape-state-shape state))...
    (shape-state-fill state)...)

(define SHAPE1 (make-circ 30 (make-posn 30 30)))
(define SHAPE2 (make-sq 15 (make-posn 50 50)))

(define SHAPE-STATE1 (make-st SHAPE1 true))
(define SHAPE-STATE2 (make-st SHAPE1 false))
(define SHAPE-STATE3 (make-st SHAPE2 true))
(define SHAPE-STATE4 (make-st SHAPE2 false))

;blink: State -> State
(define (blink st)
  (big-bang st
    [on-mouse up-press]
    [to-draw draw-shapes]))
;up-press: State Num Num MouseEvent -> State
;checks to see if the release of the mouse is on or off the shape
(define (up-press st x y event)
  (cond
    [(and(string=? event "button-up")
         (in-shape? x y st)) (make-st (st-shape st)
                                      (not (st-fill st)))]
    [(and(string=? event "button-up")
         (not(in-shape? x y st))) (make-st (change-center x y (st-shape st))
                                           (st-fill st))]
    [else st]))

;change-center Num Num Shape -> Shape
;takes in shape from down-press but only the object where the click was outside the object

(define (change-center x y s)
  (cond
    [(circ? s) (make-circ (circ-radius s) (make-posn x y))]
    [(sq? s) (make-sq (sq-side s) (make-posn x y))]))


;draw-shapes: State -> Image
;will either draw a circle or a square
(define (draw-shapes st)
  (cond
    [(circ? (st-shape st)) (draw-circle st)]
    [(sq? (st-shape st)) (draw-square st)]))


(define BG (square 600 "solid" "white"))
;draw-circle : State -> Image
;when called from draw-shapes, it will draw a circle
(define (draw-circle s)
  (place-image (circle (circ-radius (st-shape s)) (if (st-fill s) "outline" "solid")
                       "purple")
               (posn-x (circ-center (st-shape s)))
               (posn-y (circ-center (st-shape s)))
               BG))
;draw-square : State -> Image
;when called from draw-shapes, it will draw a square
(define (draw-square s)
  (place-image (square (sq-side (st-shape s)) (if (st-fill s) "outline" "solid")
                       "purple")
               (posn-x (sq-center (st-shape s)))
               (posn-y (sq-center (st-shape s)))
               BG))


;in-shape?: Num Num State -> Boolean
;checks if the click is in the shape
(check-expect (in-shape? 50 50 SHAPE-STATE1) true)
(check-expect (in-shape? 100 100 SHAPE-STATE1) false)
(check-expect (in-shape? 50 50 SHAPE-STATE2) true)
(check-expect (in-shape? 100 100 SHAPE-STATE2) false)
(define (in-shape? x y st)
  (cond
    [(circ? (st-shape st)) (> (circ-radius (st-shape st))
                              (sqrt (+ (sqr (- x (posn-x(circ-center (st-shape st)))))
                                       (sqr (- y (posn-y(circ-center (st-shape st))))))))]
    [(sq? (st-shape st)) (edges x y (st-shape st))]))


;edges: Num Num square -> Boolean
;finds edges of the square depending on center and length
(check-expect (edges 40 40 (make-sq 20 (make-posn 50 50))) true)
(check-expect (edges 100 100 (make-sq 20 (make-posn 50 50))) false)
(define (edges x y square)
  (and (and(< x (+ (/ 2 (sq-side square)) (posn-x(sq-center square))))
           (> x (- (/ 2 (sq-side square)) (posn-x(sq-center square)))))
       (and (< y (+ (/ 2 (sq-side square)) (posn-x(sq-center square))))
            (> y (- (/ 2 (sq-side square)) (posn-x(sq-center square)))))))


;ex 6
(define-struct building [width height color right])
; A Skyline is one of:
; - false
; - (make-building PosInteger PosInteger String Skyline) 
; and represents either an empty skyle or a building with
; a width and height in pixels, the color of the building, and the
; rest of the skyline to the right of the building

;;(define SKY1 (make-building 100 500 "blue" Skyline))
;;(define SKY2 (make-building 250 400 "red" (make-building 100 500 "blue" Skyline)))
;;(define SKY3 (make-building 500 1000 "pink"
                            ;;(make-building 250 400 "red" (make-building 100 500 "blue" false))))

#;(define (Skyline-temp skah)
  (cond
    [(false? skah)...]
    [()]))

;ex 7
(define-struct moon [distance mass])
; A Moon is a (make-moon Number Number Planet)
; and represents a moon orbiting around a planet with
; a defined distance from the planet and a mass
(define MOON-1 (make-moon 500 30))
(define MOON-2 (make-moon 700 120))
(define MOON-3 (make-moon 900 150))
(define MOON-4 (make-moon 3000 500))
(define MOON-5 (make-moon 1000 75))
(define MOON-6 (make-moon 7000 250))

; moon-temp : Moon -> ???
#; (define (moon-temp m)
     (... (moon-distance m)...
          (moon-mass m)...))

(define-struct planet [name distance firstmoon secondmoon])
; A Planet is a (make-planet String Number Moon Moon)
; and represents a named planet in a planetary system
; a defined distance from the star with two moons
(define PLANET-1 (make-planet "Joe" 3000000 MOON-1 MOON-2))
(define PLANET-2 (make-planet "Bill" 1000 MOON-3 MOON-4))
(define PLANET-3 (make-planet "Earth-2" 800000 MOON-5 MOON-6))

; planet-temp : Planet -> ???
#; (define (planet-temp p)
     (... (planet-name p)...
          (planet-distance p)...
          (moon-mass (planet-firstmoon p))...
          (moon-mass (planet-secondmoon p))...))

(define-struct system [planet1 planet2 planet3])
; A System is a (make-system Planet Planet Planet)
; and represents a planetary system with three planets
(define SYSTEM-1 (make-system PLANET-1 PLANET-2 PLANET-3))

; system-temp : System -> ???
#; (define (system-temp s)
     (... (system-planet1 s)...
          (system-planet2 s)...
          (system-planet3 s)...))

; planet-bigger-moon? : Planet Number -> Boolean
; returns whether or not a Planet has a Moon with
; a greater mass than the supplied value
(check-expect (planet-bigger-moon? PLANET-1 130) false)
(check-expect (planet-bigger-moon? PLANET-2 300) true)
(check-expect (planet-bigger-moon? PLANET-3 50) true)

(define (planet-bigger-moon? p n)
  (or
   (> (moon-mass (planet-firstmoon p)) n)
   (> (moon-mass (planet-secondmoon p)) n)))

; bigger-moon? : System Number -> Boolean
; returns whether or not a System has a Planet with a Moon with a greater
; mass than the supplied value
(check-expect (bigger-moon? SYSTEM-1 15) true)
(check-expect (bigger-moon? SYSTEM-1 400) true)
(check-expect (bigger-moon? SYSTEM-1 1000) false)

(define (bigger-moon? s n)
  (or
   (planet-bigger-moon? (system-planet1 s) n)
   (planet-bigger-moon? (system-planet2 s) n)
   (planet-bigger-moon? (system-planet3 s) n)))