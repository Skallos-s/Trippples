#lang racket

(require 2htdp/universe
         2htdp/image)

(provide launch-client)



; __          ________    ________    ______    __      __
;/_/\        /_______/\  /_______/\  /_____/\  /_/\    /_/\
;\ \ \       \  ____ \ \ \  ____ \ \ \__  _\/  \ \ \__ \ \ \
; \ \ \       \ \ \ \ \ \ \ \ \ \_\/    \ \ \   \ \/_/\_\ \ \
;  \ \ \       \ \ \ \ \ \ \ \ \  ____   \ \ \   \  _\/_/\ \ \
;   \ \ \       \ \ \ \ \ \ \ \ \/___/\   \ \ \   \ \ \_\/\ \ \
;    \ \ \______ \ \ \_\ \ \ \ \ \__ \ \   \ \ \__ \ \ \ \__ \ \
;     \ \/_____/\ \ \/__\ \ \ \ \/__\ \ \  /\ \/_/\ \ \ \   \ \ \
;      \_______\/  \_______\/  \_______\/  \_____\/  \_\/    \_\/

; Login Constants
(define LOGIN-HEIGHT-COMP 80)
(define LOGIN-WIDTH 250)

; Login Struct
; String
; String
; Integer
; name and IP are inputs to be used to connect.
; menu determines what button is selected.
; Sign of menu determines if help screen is shown or not.
(define-struct login-data (name IP menu))

; Draws a box with toggleable shading
; String Boolean Integer Integer => Image
(define
  (box txt sh sx sy)
  (overlay
   (place-image
    (text txt 20 'black)
    (/ (- sx 2) 2) (/ (- sy 2) 2)
    (rectangle (- sx 2) (- sy 2) 'solid (if sh 'gainsboro 'gray)))
   (rectangle sx sy 'solid 'black)))

; Draws main login screen.
; login-data => Image
(define
  (login-front logdat)
  (let ([select (abs (login-data-menu logdat))]) ; Menu select
    (overlay
     (above

      ; Game Title
      (overlay
       (text "TRIPPPLES" 40 'red)
       (rectangle LOGIN-WIDTH LOGIN-HEIGHT-COMP 0 'red))

      ; Input Fields
      (overlay
       (above/align 'left
                    (beside
                     (box (login-data-name logdat) (= 1 select) 120 30)
                     (square 2 0 'red)
                     (text "Username" 10 'black))
                    (square 2 0 'red)
                    (beside
                     (box (login-data-IP logdat) (= 2 select) 120 30)
                     (square 2 0 'red)
                     (text "Server IP" 10 'black)))
       (rectangle LOGIN-WIDTH LOGIN-HEIGHT-COMP 0 'red))

      ; Remaining Buttons
      (overlay
       (above
        (box "Join Server" (= 3 select) 120 30)
        (square 2 0 'red)
        (beside
         (box "How To Play" (= 4 select) 120 30)
         (square 10 0 'red)
         (box "Quit" (= 5 select) 50 30)))
       (rectangle LOGIN-WIDTH LOGIN-HEIGHT-COMP 0 'red)))
     (rectangle    LOGIN-WIDTH         (* 3 LOGIN-HEIGHT-COMP)  'solid 'white)
     (rectangle (+ LOGIN-WIDTH 2) (+ 2 (* 3 LOGIN-HEIGHT-COMP)) 'solid 'black))))

; Static Help Screen
; => Image
(define
  (login-help)
  (overlay
   (text
    (string-append
     "GOAL: Move your token from the START tile\n( ● or ■ "
     ") to the FINISH tile ( ○ or □ ) by moving\nyour toke"
     "n one space at a time orthogonally.\n\nRULE: You can"
     " only move your token in the\ndirection under your o"
     "pponent's token.\nFurther, you may only move onto op"
     "en\narrow tiles and your own goal.\n\nPLAY: Begin by"
     " taking a free move from your\nSTART tile.\n\nWINS: "
     "Reach your own FINISH tile to win.\nDraws may occur.")
    12 'black)
   (rectangle LOGIN-WIDTH (* 3 LOGIN-HEIGHT-COMP) 'solid 'white)
   (rectangle (+ LOGIN-WIDTH 2) (+ 2 (* 3 LOGIN-HEIGHT-COMP)) 'solid 'black)))

; Login Draw Function
; login-data => Image
(define
  (login-draw logdat)
  (if (positive? (login-data-menu logdat))
      (login-front logdat)
      (login-help)))

; Increment/Decrement Menu Counter
; login-data Integer => login-data
(define
  (change-menu logdat i)
  (struct-copy
   login-data
   logdat
   [menu
    (+ 1 (modulo (+ i -1 (login-data-menu logdat)) 5))]))

; Type Helping Function
; Deletes if last character is backspace.
; Adds character otherwise.
; String String => String
(define
  (type txt key)
  (cond
    [(string=? key "f1") LOCALHOST]
    [(> (string-length key) 1) txt]
    [(or (string=? key "\n") (string=? key "\r")) txt]
    [(and (string=? "\b" key) (> (string-length txt) 0))
     (substring txt 0 (- (string-length txt) 1))]
    [(string-append txt key)]))

; Keyboard Input
; login-data String => login-data
(define
  (login-key logdat key)
  (let
      ([select (abs (login-data-menu logdat))]
       [inmenu (negative? (login-data-menu logdat))])
    (cond
      [(string=? key "escape") (stop-with "q")]
      [inmenu (struct-copy login-data logdat [menu (abs (login-data-menu logdat))])]
      [(string=? key "up"   ) (change-menu logdat -1)]
      [(string=? key "left" ) (change-menu logdat -1)]
      [(string=? key "down" ) (change-menu logdat  1)]
      [(string=? key "right") (change-menu logdat  1)]
      [(string=? key "	") (change-menu logdat 1)]
      [(= select 1) (struct-copy login-data logdat [name (type (login-data-name logdat) key)])]
      [(= select 2) (struct-copy login-data logdat [IP (type (login-data-IP logdat) key)])]
      [(string=? key "\r")
       (cond
         [(= select 3) (stop-with (struct-copy login-data logdat [menu 0]))]
         [(= select 4) (struct-copy login-data logdat [menu (- (login-data-menu logdat))])]
         [(= select 5) (stop-with "Quit")]
         [else logdat])]
      [(string=? key "q") (stop-with "Quit")]
      [else logdat])))




; Main Client Launch
; This client launcher connects the sub big-bangs.
(define
  (launch-client)
  (let ([output (launch-client-login)])
    (if
     (= 0 (login-data-menu output))
     (launch-client-hidden output)
     "Quit")))

; Login Client Launch
; Big-bang for login world.
(define
  (launch-client-login)
  (big-bang (make-login-data "" "" 1)
    (to-draw login-draw)
    (on-key login-key)
    (name "Trippples Client Launch")
    (close-on-stop true)))

; Hidden Client Launch
; Big-bang for connected world.
(define
  (launch-client-hidden logdat)
  (big-bang (make-game-data (list (login-data-name logdat)) 0 '() '() '() 0+0i) 
    (to-draw game-draw)
    (on-receive game-parse)
    (on-key game-key)
    (name (login-data-name logdat))
    (register (login-data-IP logdat))))




; ________    ________    __      __  ________
;/_______/\  /_______/\  /_/\    /_/\/_______/\
;\  ____ \ \ \  ____ \ \ \ \ \__ \ \ \  _____\/
; \ \ \ \_\/  \ \ \_\ \ \ \ \/_/\/\ \ \ \ \______
;  \ \ \  ____ \ \/__\ \ \ \  _\_\__ \ \ \/_____/\
;   \ \ \/___/\ \  ____ \ \ \ \ \_\/\ \ \  _____\/
;    \ \ \__ \ \ \ \ \ \ \ \ \ \ \   \ \ \ \ \______
;     \ \/__\ \ \ \ \ \ \ \ \ \ \ \   \ \ \ \/_____/\
;      \_______\/  \_\/  \_\/  \_\/    \_\/\_______\/


; Game Struct
; List of Strings
; Integer
; List of Integers
; List of Complex Numbers
; List of Integers
; Complex Number
; Check server.rkt for more details.
; name is list of player names.
; select is selected tile on the board and selected rem-tile.
; Real part is selected tile.
; Imag part is rem-tile.
(define-struct game-data
  (name active board play-posn rem-tile select))


; Data Receive Parse
; Read in messages and visualize the world.
; game-data CToSMessage => game-data
(define
  (game-parse game msg)
  (make-game-data
   (first msg)
   (second msg)
   (third msg)
   (fourth msg)
   (fifth msg)
   (let*
       ([sel (game-data-select game)]
        [fif (fifth msg)]
        [cap (if fif (length fif) 0)])
     (make-rectangular
      (real-part sel)
      (max 0 (min (imag-part sel) (- cap 1)))))))

; Game Key Function
; Reads in game key presses and updates the world accordingly.
; May also send out messages.
; game-data String => game-data
(define
  (game-key game k)
  (let*
      ([x (real-part (game-data-select game))]
       [y (imag-part (game-data-select game))]
       [s (length (game-data-rem-tile game))]
       [ns (if (= s 0) 1 s)])
    (cond
      [(string=? k "\r") (make-package game (list x y))]
      [(string=? k "r") (make-package game (list x (random s)))]
      [(and (string=? k "q") (> s 0))
       (make-package
        game
        (list
         (first (shuffle (indexes-of (game-data-board game) 256)))
         (random s)))]
      [else
       (struct-copy
        game-data game
        [select
         (cond
           [(and (string=? k "left" ) (> (remainder x 8) 0))
            (make-rectangular (- x 1) y)]
           [(and (string=? k "right") (< (remainder x 8) 7))
            (make-rectangular (+ x 1) y)]
           [(and (string=? k "up"   ) (> (quotient x 8) 0))
            (make-rectangular (- x 8) y)]
           [(and (string=? k "down" ) (< (quotient x 8) 7))
            (make-rectangular (+ x 8) y)]
           [(string=? k "wheel-up" )
            (make-rectangular x (modulo (+ y 1) ns))]
           [(string=? k "wheel-down" )
            (make-rectangular x (modulo (- y 1) ns))]
           [else (make-rectangular x y)])])])))

; Game Screen Proper
; Game screen for the full game.
; game-data => Image
(define
  (game-draw-proper game)
  (above
   (overlay
    (cond
      [(= 0+7i (first  (game-data-play-posn game)))
       (text
        (string-append
         (first  (game-data-name game))
         " Won!")
        40 'green)]
      [(= 0+0i (second (game-data-play-posn game)))
       (text
        (string-append
         (second (game-data-name game))
         " Won!")
        40 'green)]
      [else empty-image])
    (foldl
     (lambda (x y z)
       (place-image
        x ; Token or Highlight
        (+ 32 (* 64 (imag-part y))) ; x coordinate
        (+ 32 (* 64 (real-part y))) ; y coordinate
        z)) ; Scene
     (draw-board (game-data-board game)) ; Starting scene
     (list HL CT ST) ; Tokens and Highlight
     (list
      (let ([pos (real-part (game-data-select game))])
        (make-rectangular
         (quotient pos 8)
         (remainder pos 8))) ; Highlight
      (first (game-data-play-posn game)) ; Circle Token
      (second (game-data-play-posn game)))) ; Square Token
    (square 516 'solid 'black))

   (overlay
    (beside
     (let ([name-list (game-data-name game)])
       (above
        (box (first name-list) (= 0 (third name-list)) 256 40)
        (square 16 0 'red)
        (box (second name-list) (= 1 (third name-list)) 256 40)))
     (square 10 0 'red)
     (above
      (circle 10 'solid (if (= 0 (game-data-active game)) 'green 'red))
      (square 36 0 'red)
      (square 20 'solid (if (= 1 (game-data-active game)) 'green 'red)))
     (square 10 0 'red)
     (if
      (> (length (game-data-rem-tile game)) 0)
      (scale 1.5 (AT
                  (list-ref
                   (game-data-rem-tile game)
                   (imag-part (game-data-select game)))))
      (square 96 0 'red)))
    (rectangle 516 124 'solid 'white))))

; Game Screen
; Shows either game screen, or waiting message.
; Note game-draw-proper crashes when fed fauly inputs.
(define
  (game-draw game)
  (if
   (>= 2 (length (game-data-name game)))
   (overlay
    (text "Waiting For Player 2" 50 'black)
    (rectangle 516 640 'solid 'white))
   (game-draw-proper game)))




; Blank Tile
; Empty tile shown in the center.
; => Image
(define BT
  (overlay
   (square 60 'solid 'sandybrown)
   (square 64 'solid 'black)))

; Unknown Tile
; Unplaced tile.
; => Image
(define UT
  (overlay
   (square 60 'solid 'peachpuff)
   (square 64 'solid 'black)))

; Arrow Segment
; Arrow segment. Duplicate, flipped, and transparent on bottom to align overlays.
; => Image
(define AS
  (above
   (triangle 10 'solid 'black)
   (rectangle 3 16 'solid 'black)
   (rectangle 3 16 0 'red)
   (rotate 180 (triangle 10 0 'red))))

; Arrow Filler
; Life Arrow Segment, but completely transparent.
; For proper spacings when using rotations.
; => Image
(define AF
  (above
   (triangle 10 0 'red)
   (rectangle 3 32 0 'red)
   (rotate 180 (triangle 10 0 'red))))

; Arrow Tile
; Return arrow tile by number.
; Integer => Image
(define (AT n)
  (overlay
   (above
    (apply overlay
           (circle 1.5 'solid 'black)
           (build-list 8
                       (lambda (x)
                         (rotate (* -45 x)
                                 (if (bitwise-bit-set? n x) AS AF)))))
    (rectangle 0 2 0 'red)
    (rectangle 20 3 'solid 'black))
   BT))

; Tile
; Return appropriate tile.
; Integer => Image
(define (tile n)
  (cond
    [(= -5 n) (overlay (circle 15 'outline 'black) BT)]
    [(= -4 n) (overlay (square 30 'outline 'black) BT)]
    [(= -3 n) (overlay (circle 15 'solid 'black) BT)]
    [(= -2 n) (overlay (square 30 'solid 'black) BT)]
    [(= -1 n) BT]
    [(= 256 n) UT]
    [else (AT n)]))

; Draw Board
; Create tiles and tile a board.
; ListOf[Integer] => Image
(define
  (draw-board board)
  (apply above
         (foldl
          (lambda (x y)
            (append (drop y 8)
                    (list (apply beside (take y 8)))))
          (map tile board) (range 8))))

; Highlight
; Cursor square.
(define HL (square 64 127 'green))

; Square Token
(define ST (square 50 127 'white))

; Circle Token
(define CT (circle 25 127 'white))
