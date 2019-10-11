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

#;(define (lon-temp l)
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

(define SLIDE-1 (make-slide "cheese" (cons "blue" (cons "american" empty))
                            (cons "goat" (cons "dairy-free" empty))))
(define SLIDE-2 (make-slide "cars" (cons "honda" (cons "ford" empty))
                            (cons "nissan" (cons "lexus" empty))))
(define SLIDE-3 (make-slide "websites" empty
                            (cons "youtube" (cons "facebook" (cons "twitter" empty)))))
(define SLIDE-4 (make-slide "sports" (cons "rugby" (cons "football" (cons "soccer" empty))) empty))

#;(define (slide-temp st)
    (slide-title st) ...
    (los-temp (slide-shown st))...
    (los-temp (slide-hidden st)...))
 
; A Slideshow is one of:
; - empty
; - (cons Slide Slideshow)
; and represents an ordered slideshow

(define SLIDESHOW-1 empty)
(define SLIDESHOW-2 (cons SLIDE-1 empty))
(define SLIDESHOW-3 (cons SLIDE-3 (cons SLIDE-1 empty)))
(define SLIDESHOW-4 (cons SLIDE-1 (cons SLIDE-3 (cons SLIDE-2 empty))))
(define SLIDESHOW-5 (cons SLIDE-4 (cons SLIDE-3 (cons SLIDE-2 empty))))

(define (slideshow-temp st)
  (cond
    [(empty? st)...]
    [(cons? st)
     (...(slide-temp(first st))
         (slideshow-temp(rest st)))]))

(define BLACK (square 500 "solid" "black"))
(define BACKGROUND (square 500 "solid" "white"))
;strong-singularity: Slideshow -> Slideshow
;this the big-bang for making a working slideshow
(define (strong-singularity slideshow)
  (empty?(big-bang slideshow
           [to-draw draw-slideshow]
           [on-key right-key]
           [stop-when end draw-end-background]
           )))

;;draw-slideshow:Slideshow->Image
;;takes in slideshow, outputs slide
(check-expect (draw-slideshow SLIDESHOW-1) BLACK)
(check-expect (draw-slideshow SLIDESHOW-2)
              (overlay (above (text "cheese" 24 "olive")
                              (text "american" 12 "olive") (text "blue" 12 "olive")) BACKGROUND))
(define (draw-slideshow slideshow)
  (cond
    [(empty? slideshow) BLACK]
    [(cons? slideshow)
     (overlay (draw-slide (first slideshow)) BACKGROUND)]))

;;draw-slide: Slide-> Image
;;draws the current slide singular, uses above needs images to passed up through
(check-expect (draw-slide SLIDE-1) (above (text "cheese" 24 "olive")
                                          (text "american" 12 "olive") (text "blue" 12 "olive")))
(define (draw-slide st)
  (above(title-print st)
        (bullet-print(slide-shown st))))


;title-print: Slide->Image
;only prints the title for the current slide
(check-expect (title-print SLIDE-1) (text "cheese" 24 "olive"))
(check-expect (title-print SLIDE-2) (text "cars" 24 "olive"))

(define (title-print st)
  (text (slide-title st) 24 "olive"))

;bullet-print: LoS->Image
;only prints the shown bullets for the current slide
(check-expect (bullet-print (cons "blue" (cons "american" empty)))
              (above(text "american" 12 "olive") (text "blue" 12 "olive")))
(define (bullet-print lt)
  (cond
    [(empty? lt) empty-image]
    [(cons? lt)
     (above (bullet-print (rest lt))
            (text (first lt) 12 "olive"))]))
;;was told by TA to do this so they can stack correctly pls no bully

;;right-key: Slideshow KeyEvent -> Slideshow
;;press right key to unveal hidden and next slide in the Slideshow
(check-expect (right-key SLIDESHOW-1 "right") empty)
(check-expect (right-key SLIDESHOW-2 "right")
              (cons (make-slide "cheese" (cons "goat"(cons "blue" (cons "american" empty)))
                                (cons "dairy-free" empty)) empty))
(check-expect (right-key SLIDESHOW-2 "left") SLIDESHOW-2)
(check-expect (right-key SLIDESHOW-3 "right")
              (cons (make-slide "websites" (cons "youtube" empty)
                                (cons "facebook" (cons "twitter" empty)))
                    (cons SLIDE-1 empty)))

(define (right-key slideshow ke)
  (cond
    [(key=? ke "right")(showey slideshow)]
    [else slideshow]))

;showey: Slideshow->Slideshow
;only if right is clicked will the slideshow be checked in this function if it is anything
(check-expect(showey SLIDESHOW-1) empty)
(check-expect(showey SLIDESHOW-2)
             (cons(make-slide "cheese"(cons "goat"(cons "blue"(cons "american" empty)))
                              (cons "dairy-free" empty)) empty))
(check-expect
 (showey SLIDESHOW-5)
 (cons (make-slide  "websites" empty(cons "youtube" (cons "facebook"
                                                          (cons "twitter" empty))))
       (cons (make-slide "cars" (cons "honda" (cons "ford" empty)) (cons "nissan"
                                                                         (cons "lexus" empty)))
             empty)))
(define (showey sl)
  (cond
    [(empty? sl) empty]
    [(cons? sl) (if(slide-empty? (first sl))
                   (rest sl) (cons (change-bullets (first sl)) (rest sl)))]))

;slide-empty? Slide-> Boolean
;checks if the slide doesn't have any hidden bullets
(check-expect (slide-empty? SLIDE-2) false)
(check-expect (slide-empty? SLIDE-1) false)
(check-expect (slide-empty? SLIDE-4) true)
(define (slide-empty? rs)
  (empty?(slide-hidden rs)))

;change-bullets: Slide->Slide
;only changes the bullets of the current slide
(check-expect(change-bullets SLIDE-2)
             (make-slide "cars" (cons "nissan" (cons "honda" (cons "ford" empty)))
                         (cons "lexus" empty)))
(check-expect(change-bullets SLIDE-3)
             (make-slide "websites" (cons "youtube" empty)
                         (cons "facebook" (cons "twitter" empty))))
(define (change-bullets cb)
  (make-slide (slide-title cb)
              (cons (first(slide-hidden cb)) (slide-shown cb)) (rest(slide-hidden cb))))

;end: Slideshow->BLACK
;when the slideshow is empty, it will end the program and draw for the last scene the black background
(check-expect  (end SLIDESHOW-1) true)
(check-expect  (end SLIDESHOW-2) false)
(check-expect  (end SLIDESHOW-3) false)
(define (end slideshow)
  (empty? slideshow))

;draw-end-background: Slideshow-> Image
;the stop when function wasn't working correctly so i had to make this to make a black end
;;willow told me to do this
(check-expect(draw-end-background SLIDESHOW-1) BLACK)
(check-expect(draw-end-background SLIDESHOW-2) BLACK)
(define (draw-end-background _)
  BLACK)

;;ex 6
;;a

;a Feature is a number from [0, 255]
;representing a single pixel in an image
;0 is white/off, 1-254 is grey, 255 is black/on
(define FEATURE-1 0)
(define FEATURE-2 1)
(define FEATURE-3 127)
(define FEATURE-4 254)
(define FEATURE-5 255)

#;(define (feature-temp f)
    (... (cond
           [(= f ...) ...]
           [(= f ...) ...])))

;;b

;a Row is one of:
; - empty
; - (cons Feature Row)
;representing all the features in a single row of a bitmap
(define ROW-0 '())
(define ROW-1 (cons 254 (cons 255 (cons 255 ROW-0))))
(define ROW-2 (cons 255 (cons 0 (cons 253 ROW-0))))
(define ROW-3 (cons 252 (cons 255 (cons 255 ROW-0))))

#;(define (row-temp r)
    (... (cond
           [(empty? r) ...]
           [(cons? r) ...])))

;a Bitmap is one of:
; - empty
; - (cons Row Bitmap)
;representing all the rows that make up a bitmap image
;by stacking the rows on top of each other
(define BITMAP-1 (cons ROW-1 '()))
(define BITMAP-2 (cons ROW-2 BITMAP-1))
(define BITMAP-3 (cons ROW-3 BITMAP-2))

#;(define (bitmap-temp b)
    (... (cond
           [(empty? b) ...]
           [(cons? b) ...])))

;;c

;an Instance is one of:
; - empty
; - (cons Row Instance)
;representing all the features that make up a bitmap image in a single row
(define INSTANCE-1 (cons ROW-1 (cons ROW-2 (cons ROW-3 '()))))

#;(define (instance-temp i)
    (... (cond
           [(empty? i) ...]
           [(cons? i) ...])))

;;d
(define COLOR-0 (make-color 0 0 0 0))
(define COLOR-255 (make-color 0 0 0 255))
(define COLOR-254 (make-color 0 0 0 254))
(define COLOR-253 (make-color 0 0 0 253))
(define COLOR-252 (make-color 0 0 0 252))
(define SQUARE-0 (square 10 "solid" COLOR-0))
(define SQUARE-255 (square 10 "solid" COLOR-255))
(define SQUARE-254 (square 10 "solid" COLOR-254))
(define SQUARE-253 (square 10 "solid" COLOR-253))
(define SQUARE-252 (square 10 "solid" COLOR-252))

(define-struct training [filename instance digit image])
;a Training image is a (make-training String Instance Digit Image)
;representing the file name, instance, corresponding digit from 0-9, and image of a training image
(define TRAINING-1 (make-training "zero" INSTANCE-1 0 .))

#;(define (training-temp t)
    (... (training-instance t)...
         (training-digit t)...
         (training-image t)...))

(define-struct testing [filename instance image])
;a Testing image is a (make-testing String Instance Image)
;representing the file name, instance, and image of a testing image
(define TESTING-1 (make-testing "zero" INSTANCE-1 .))

