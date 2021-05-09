#lang racket

(require 2htdp/universe)

(provide launch-server launch-server-quick-start)


; Universe Data Structure
; List of IWorlds
; Integer
; List of Integers
; List of Complexs
; List of Integers
; active is active player id. Either 0 or 1.
; board represents each tile as an integer between 0 or 255, representing arrows.
; [256, -1, -2, -3, -4, -5]
;    =>
;      [Unplaced Tile, Blank Tile, Square Start, Circle Start, Square Finish, Circle Finish]
; Row major ordering.
; play-posn is at most 2 Complexs, representing position of player chips.
; rem-tile is like board, but for remaining pieces.

;     0      
;7__  ^  __1
; |\  |  /| 
;   \ | /   
;    \|/    
;6<---0--->2
;    /|\    
;   / | \   
; |/  |  \| 
; ""  V  "" 
;5    4    3
(define-struct uni-data (iwos active board play-posn rem-tile))

; Server Launchers
; Launch server from new state.
(define
  (launch-server)
  (universe (make-uni-data '() 0 #f #f #f)
            ;(state #t)
            (on-new (lambda (x y) (connect x y #f)))
            (on-disconnect disconnect)
            (on-msg handle-msg)))
; Launch server in quick-start mode.
; Board is randomly preset beforehand.
(define
  (launch-server-quick-start)
  (universe (make-uni-data '() 0 #f #f #f)
            (on-new (lambda (x y) (connect x y #t)))
            (on-disconnect disconnect)
            (on-msg handle-msg)))

; Handles new client connections.
; Only accepts 2 cleints.
; Boots off 3rd above.
; Refuse nameless cleints. They are no fun.
; uni-data IWorld Boolean => [Bundle uni-data [Listof [Mail IWorld Data]] [Listof IWorld]]
(define (connect uni client quick-start)
  (let ([num-play (length (uni-data-iwos uni))])
    (cond
      [(zero? (string-length(iworld-name client))) (make-bundle uni empty (list client))]
      [(= num-play 0) ; Take player in. Don't begin game.
       (let ([blank-world (make-uni-data (list client) 0 #f #f #f)])
         (make-bundle
          blank-world
          (list (make-mail client (datum blank-world 0))) '()))]
      
      [(= num-play 1) ; Take player in. Begin game.
       (let ([start (init-game uni client quick-start)])
         (make-bundle
          start
          (list
           (make-mail (first  (uni-data-iwos uni)) (datum start 0))
           (make-mail client (datum start 1)))
          '()))]
      
      [else (make-bundle uni empty (list client))]))) ; Reject player.

; Handles client disconnects.
; Reset world if 1 player left.
; Don't send a message if no players left.
; uni-data IWorld => [Bundle uni-data [Listof [Mail IWorld Data]] [Listof IWorld]]
(define (disconnect uni client)
  (let*
      ([players (uni-data-iwos uni)]
       [left-player (list-ref players (- 1 (index-of players client)))]
       [blank-world (make-uni-data (list left-player) 0 #f #f #f)])
    (make-bundle
     blank-world
     (list (make-mail left-player (datum blank-world 0)))
     (list client))))

; Handles client messages.
; uni-data IWorld CtoSMessage => [Bundle uni-data [Listof [Mail IWorld Data]] [Listof IWorld]]
(define
  (handle-msg uni client msg)
  (let ([next (update uni client msg)])
    (make-bundle
     next
     (map
      (lambda(x y) (make-mail x (datum next y)))
      (uni-data-iwos uni)
      (range (length (uni-data-iwos uni))))
     '())))

; Initialize game
; Create starting pieces, shuffled.
; Initialize empty board.
; Return uni-data
; uni-data IWorld => uni-data
(define
  (init-game uni client quick-start)
  (let
      ([init-board
        (append ; Initial Board Data
         (list -4)
         (make-list 6 256)
         (list -5)
         (make-list 19 256)
         (list -1 -1)
         (make-list 6 256)
         (list -1 -1)
         (make-list 19 256)
         (list -3)
         (make-list 6 256)
         (list -2))]
       
       [init-rem-tile
        (map ; Unpalced Tiles.
         (lambda (x) ; Generate every permutation of (2^0, 2^1, ..., 2^7) of size 3.
           (apply + x)) ; Sum each permutation afterwards.
         (combinations ; Result is all 56 possible piece tiles.
          (map
           (lambda (y) (expt 2 y))
           (range 8))
          3))])
    (make-uni-data
   
     (append (uni-data-iwos uni) (list client)) ; 2 Clients
     0 ; Player 1 starts
     (if quick-start (quick-place init-board init-rem-tile) init-board) ; Random or Empty
     (list 7+0i 7+7i) ; Player Starting Positions
     (if quick-start empty init-rem-tile))))

; Randomly place rem-tile pieces onto board.
; Listof[Integer] Listof[Integer] => Listof[Integer]
(define
  (quick-place board rem-tile)
  (foldl
   (lambda (x y) (list-set y (index-of y 256) x))
   board
   (shuffle rem-tile)))

; Transforms uni-data into sendable list that each world receives.
; Replace client with client name and repackage as list.
; uni-data => [List String [Listof Integer] [Listof Complex] [Listof Integer]]
(define
  (datum uni n)
  (list
   (append (map iworld-name (uni-data-iwos uni)) (list n))
   (uni-data-active uni)
   (uni-data-board uni)
   (uni-data-play-posn uni)
   (uni-data-rem-tile uni)))

; Process client message and update universe data accordingly.
; Only accept messages from active player.
; Accept piece placement when rem-tile has pieces.
; Accept valid movements when rem-tile is empty.
; Reject all messages upon winner.
; uni-data IWorld CtoSMessage => uni-data
(define
  (update uni client msg)
  (if
   (and
    (iworld=?
     client
     (list-ref (uni-data-iwos uni) (uni-data-active uni)))
    (not (= 0+7i (first  (uni-data-play-posn uni))))
    (not (= 0+0i (second (uni-data-play-posn uni)))))
   (if (> (length (uni-data-rem-tile uni)) 0)
       (place-piece uni msg)
       (move-token uni (uni-data-active uni) msg))
   uni))

; Places piece if possible.
; Player validation is already done.
; Check if location is within bounds and on a blank tile.
; Check if selected piece is within bounds.
; uni-data CtoSMessage => uni-data
(define
  (place-piece uni msg)
  (let ([coord (first msg)])
    (if
     (and
      (<= 0 coord 63)
      (= 256 (list-ref (uni-data-board uni) coord))
      (<= 0 (second msg) (length (uni-data-rem-tile uni))))
     (let ([tile (list-ref (uni-data-rem-tile uni) (second msg))])
       (make-uni-data
        (uni-data-iwos uni) ; No change
     
        (- 1 (uni-data-active uni)) ; Swap active player
     
        (list-set ; Set tile
         (uni-data-board uni)
         coord
         tile)
     
        (uni-data-play-posn uni) ; No change
     
        (remove tile (uni-data-rem-tile uni)))) ; Remove placed tile
     uni)))
    
; Moves the active player token if possible.
; Player validation already done.
; Check if location is reachable and valid.
; uni-data Integer CtoSMessage => uni-data
(define
  (move-token uni active msg)
  (let*
      ([new-indx (first msg)]
       [new-posn (board-comp new-indx)]
       [old-posn (list-ref (uni-data-play-posn uni) active)]
       [old-indx (board-index old-posn)]
       [opp-posn (list-ref (uni-data-play-posn uni) (- 1 active))]
       [opp-indx (board-index opp-posn)]
       [opp-tile (list-ref (uni-data-board uni) opp-indx)])
    (cond
      [(and
        (empty?
         (filter
          (lambda (x)
            (not
             (or
              (= x 7+0i)
              (= x 7+7i)
              (= x 3+3i)
              (= x 4+3i)
              (= x 3+4i)
              (= x 4+4i)
              (= x (+ (* 7 active) 0+7i))
              (not (<= 0 (real-part x) 7))
              (not (<= 0 (imag-part x) 7)))))
          (arrow-move old-posn opp-tile)))
        (not (= old-posn 7+0i))
        (not (= old-posn 7+7i)))
       (struct-copy uni-data uni [active (- 1 (uni-data-active uni))])]
      [(or
        (= new-posn 7+0i) ; Starting position
        (= new-posn 7+7i)
        (= new-posn 3+3i) ; Center block
        (= new-posn 4+3i)
        (= new-posn 3+4i)
        (= new-posn 4+4i)
        (= new-posn (first (uni-data-play-posn uni))) ; Player token
        (= new-posn (second (uni-data-play-posn uni)))
        (< new-indx 0) ; Out of bounds
        (> new-indx 63)
        (and (= new-posn 0+0i) (= active 0)) ; Incorrect goal
        (and (= new-posn 0+7i) (= active 1))
        (and (= old-posn 7+0i) (not (arrow-check new-posn 7+0i 255))) ; Player 1 starting move
        (and (= old-posn 7+7i) (not (arrow-check new-posn 7+7i 255))) ; Player 2 starting move
        (not
         (or
          (= old-posn 7+0i) ; First Move
          (= old-posn 7+7i)
          (arrow-check new-posn old-posn opp-tile)))) ; 
       uni]
      [else
       (make-uni-data
        (uni-data-iwos uni) ; No change
    
        (- 1 (uni-data-active uni)) ; Swap active player
    
        (uni-data-board uni) ; no change
    
        (list-set (uni-data-play-posn uni) active new-posn) ; Update player position
    
        (uni-data-rem-tile uni))]))) ; No change
    

; Convert complex to board index.
; Complex => Integer
(define
  (board-index x)
  (+
   (* (real-part x) 8)
   (imag-part x)))

; Convert board index to complex.
; Integer => Complex
(define
  (board-comp x)
  (make-rectangular
   (quotient x 8)
   (modulo x 8)))

; Convert arrow direction value to complex delta.
; Integer =? Complex
(define
  (arrow-delta x)
  (list-ref
   (list -1+0i -1+1i 0+1i 1+1i 1+0i 1-1i 0-1i -1-1i)
   x))

; Check if move follows arrow.
; Complex Complex Integer => (or Integer False)
(define
  (arrow-check new-posn old-posn arrow-tile)
  (or
   (< arrow-tile 0)
   (index-of
    (arrow-move old-posn arrow-tile)
    new-posn)))

; Output Valid Moves
; Complex Integer => Listof[Complex]
(define
  (arrow-move old-posn arrow-tile)
  (map
   (lambda (y) (+ old-posn (arrow-delta y)))
   (indexes-of
    (build-list
     8
     (lambda (x) (bitwise-bit-set? arrow-tile x)))
    #t)))