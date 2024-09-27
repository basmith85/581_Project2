#|
------------------Prologue--------------------
Program Name: EECS581_Project2
Description: Racket based Battleship game using the R-Cade game engine. Players can play against a random guessing AI or PvP Pass and Play. 
Reqiruments: Dr.Racket IDE (https://racket-lang.org)
Libraries: R-Cade (https://r-cade.io)
R-Cade Documentation: https://docs.racket-lang.org/r-cade/index.html

Current bugs: the code is working but there are some issues that need to be fixed. 
- When playing with AI, it displays "you win" even if the AI wins. 

Suggestions: 
- Possibly add a "back button to navigate the menus"

Input: Mouse and Key input in order to manage/play the game
Output:
	- Displays a 800x900 window with the running battleship game. It will always start at the home state
Author: Darshil Patel
Date Created: 9/26/24
--------------------------------------------
|#

#lang racket
(require r-cade)
(require racket/match)

;;-------------Initialization---------------;;

;; Define States
(define home 0)
(define game-mode-selection 1)
(define select-ai-difficulty 2)
(define ship-selection 3)
(define ship-placement 4)
(define in-play 5)
(define game-over 6)
(define help-screen 7)
(define pause-menu 8)

;; Declare Variables
(define currentState home)
(define stateStack '()) ; Stack for state management
(define num-ships 0)
(define opponent-y-offset 50)
(define player-y-offset 465)
(define playerTurn 0) ; 1 for Player 1, 2 for Player 2, 0 for AI
(define game-mode '2-player)
(define player1-ships-placed 0)
(define player2-ships-placed 0)
(define current-player 1)
(define ai-difficulty 'easy)

;; Track ship sizes and placements
(define ship-sizes '())
(define ships-placed-locations '())
(define ship-orientation 'horizontal) ; Use for ship placement

;; Variables for Special Shooting Feature
(define player1-special-shots 3)
(define player2-special-shots 3)
(define special-shot-active #f)
(define special-shot-orientation 'horizontal) ; Use for special shots

;; AI State Variables
(define ai-hit-cells '()) ; For medium AI tracking
(define ai-target-cells '()) ; For medium AI targeting

;; Variables to Track Total Ship Parts
(define player1-total-ship-parts 0)
(define player2-total-ship-parts 0)
(define opponent-total-ship-parts 0)

;; Initialize a board with vectors using a given size
;; Use symbols for cell states: 'empty, 'ship, 'hit, 'miss
(define (createBoard size)
  (let ([board (make-vector size)])
    (for ([i (in-range size)])
      (vector-set! board i (make-vector size 'empty)))
    board))

;; Create boards/grids using the createBoard function
(define boardSize 10)
(define cellSize 40)
(define x-offset 200)
(define y-offset 200)
(define button-width 160)
(define button-height 60)

(define opponentBoard (createBoard boardSize))
(define player1-board (createBoard boardSize))
(define player2-board (createBoard boardSize))

;; Function to draw the grid for in-play state
(define (draw-grid x-offset y-offset board showShips)
  ;; Draw grid
  (for ([i (in-range boardSize)])
    (for ([j (in-range boardSize)])
      ;; Draw grid lines
      (color 7)
      (rect (+ x-offset (* j cellSize))
            (+ y-offset (* i cellSize))
            cellSize cellSize
            #:fill #f)
      ;; Handle cell state
      (let ([cell (vector-ref (vector-ref board i) j)])
        (cond
          [(eq? cell 'hit)
           (color 8)
           (rect (+ x-offset (* j cellSize))
                 (+ y-offset (* i cellSize))
                 cellSize cellSize
                 #:fill #t)]
          [(eq? cell 'miss)
           (color 12)
           (rect (+ x-offset (* j cellSize))
                 (+ y-offset (* i cellSize))
                 cellSize cellSize
                 #:fill #t)]
          [(and (eq? cell 'ship) showShips)
           (color 7)
           (rect (+ x-offset (* j cellSize))
                 (+ y-offset (* i cellSize))
                 cellSize cellSize
                 #:fill #t)])))))

;; Function to draw the grid with ship placement highlighting
(define (draw-grid-with-highlight x-offset y-offset board showShips ship-size orientation)
  ;; Get mouse position
  (let* ((mouseX (mouse-x))
         (mouseY (mouse-y))
         (board-pos (mouse-to-board mouseX mouseY x-offset y-offset)))
    ;; Draw grid
    (for ([i (in-range boardSize)])
      (for ([j (in-range boardSize)])
        ;; Determine if this cell should be highlighted
        (define highlight-cell #f)
        (when board-pos
          (define (within-ship-range row col)
            (if (eq? orientation 'horizontal)
                (and (= row (car board-pos))
                     (<= col (+ (cdr board-pos) (- ship-size 1)))
                     (>= col (cdr board-pos)))
                (and (= col (cdr board-pos))
                     (<= row (+ (car board-pos) (- ship-size 1)))
                     (>= row (car board-pos)))))
          (when (within-ship-range i j)
            (set! highlight-cell #t)))
        ;; Draw grid lines
        (color 7)
        (rect (+ x-offset (* j cellSize))
              (+ y-offset (* i cellSize))
              cellSize cellSize
              #:fill #f)
        ;; Handle cell state
        (let ([cell (vector-ref (vector-ref board i) j)])
          (cond
            [(eq? cell 'hit)
             (color 8)
             (rect (+ x-offset (* j cellSize))
                   (+ y-offset (* i cellSize))
                   cellSize cellSize
                   #:fill #t)]
            [(eq? cell 'miss)
             (color 12)
             (rect (+ x-offset (* j cellSize))
                   (+ y-offset (* i cellSize))
                   cellSize cellSize
                   #:fill #t)]
            [(and (eq? cell 'ship) showShips)
             (color 7)
             (rect (+ x-offset (* j cellSize))
                   (+ y-offset (* i cellSize))
                   cellSize cellSize
                   #:fill #t)]
            [highlight-cell
             ;; Check if ship can be placed here
             (if (can-place-ship? board (car board-pos) (cdr board-pos) ship-size orientation)
                 (color 14) ; Green highlight if valid
                 (color 12)) ; Red highlight if invalid
             (rect (+ x-offset (* j cellSize))
                   (+ y-offset (* i cellSize))
                   cellSize cellSize
                   #:fill #t)]))))))

;; Function to draw the grid with special shot highlighting
(define (draw-grid-with-special-highlight x-offset y-offset board showShips)
  ;; Get mouse position
  (let* ((mouseX (mouse-x))
         (mouseY (mouse-y))
         (board-pos (mouse-to-board mouseX mouseY x-offset y-offset)))
    ;; Draw grid
    (for ([i (in-range boardSize)])
      (for ([j (in-range boardSize)])
        (begin
          ;; Draw grid lines
          (color 7)
          (rect (+ x-offset (* j cellSize))
                (+ y-offset (* i cellSize))
                cellSize cellSize
                #:fill #f)
          ;; Handle cell state
          (let ([cell (vector-ref (vector-ref board i) j)])
            (cond
              [(eq? cell 'hit)
               (color 8)
               (rect (+ x-offset (* j cellSize))
                     (+ y-offset (* i cellSize))
                     cellSize cellSize
                     #:fill #t)]
              [(eq? cell 'miss)
               (color 12)
               (rect (+ x-offset (* j cellSize))
                     (+ y-offset (* i cellSize))
                     cellSize cellSize
                     #:fill #t)]
              [(and (eq? cell 'ship) showShips)
               (color 7)
               (rect (+ x-offset (* j cellSize))
                     (+ y-offset (* i cellSize))
                     cellSize cellSize
                     #:fill #t)]))
          ;; Highlight the row or column for special shot
          (when (and special-shot-active board-pos)
            (if (eq? special-shot-orientation 'horizontal)
                (when (= i (car board-pos))
                  (color 14)
                  (rect (+ x-offset (* j cellSize))
                        (+ y-offset (* i cellSize))
                        cellSize cellSize
                        #:fill #t))
                (when (= j (cdr board-pos))
                  (color 14)
                  (rect (+ x-offset (* j cellSize))
                        (+ y-offset (* i cellSize))
                        cellSize cellSize
                        #:fill #t)))))))))

;; Checks if the mouse click is within a given area
(define (mouse-in? mx my x y width height)
  (and (<= x mx (+ x width))
       (<= y my (+ y height))))

;; Converts mouse position to board coordinates, given the correct offset
(define (mouse-to-board mx my x-offset y-offset)
  (let* ((col (quotient (- mx x-offset) cellSize))
         (row (quotient (- my y-offset) cellSize)))
    (if (and (>= col 0) (< col boardSize) (>= row 0) (< row boardSize))
        (cons row col)
        #f)))

;; Checks if a ship can be placed without overlapping or out of bounds
(define (can-place-ship? board row col size orientation)
  (cond
    [(eq? orientation 'horizontal)
     (and (<= (+ col size) boardSize)
          (for/and ([i (in-range size)])
            (eq? (vector-ref (vector-ref board row) (+ col i)) 'empty)))]
    [(eq? orientation 'vertical)
     (and (<= (+ row size) boardSize)
          (for/and ([i (in-range size)])
            (eq? (vector-ref (vector-ref board (+ row i)) col) 'empty)))]))

;; Places a ship on the board
(define (place-ship board row col size orientation)
  (for ([i (in-range size)])
    (if (eq? orientation 'horizontal)
        (vector-set! (vector-ref board row) (+ col i) 'ship)
        (vector-set! (vector-ref board (+ row i)) col 'ship)))
  (set! ships-placed-locations (cons (list row col size orientation) ships-placed-locations)))

;; Places ships for the opponent (AI)
(define (place-opponent-ships ship-sizes)
  ;; Loop over the sizes of ships
  (for ([size ship-sizes])
    (let loop ()
      (let* ([row (random boardSize)] ; Random row
             [col (random boardSize)] ; Random column
             [orientation (if (zero? (random 2)) 'horizontal 'vertical)]) ; Random orientation
        (if (can-place-ship? opponentBoard row col size orientation) ; Check if placement is possible
            (place-ship opponentBoard row col size orientation) ; Place the ship
            (loop)))))) ; Retry if placement is invalid

;; Removes the most recently added ship from the board
(define (remove-ship board ship)
  (let* ((row (first ship))
         (col (second ship))
         (size (third ship))
         (orientation (fourth ship)))
    (for ([i (in-range size)])
      (if (eq? orientation 'horizontal)
          (vector-set! (vector-ref board row) (+ col i) 'empty)
          (vector-set! (vector-ref board (+ row i)) col 'empty))))
  (set! ships-placed-locations (rest ships-placed-locations)))

;; Randomly decide who starts the game first
(define (coinToss)
  (cond
    [(eq? game-mode '2-player)
     (set! playerTurn (if (zero? (random 2)) 1 2))]
    [(eq? game-mode '1-player-vs-ai)
     (set! playerTurn (if (zero? (random 2)) 1 0))]))

;; Function to handle player's guess on the opponent's board
(define (player-guess board mouseX mouseY)
  (let* ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset)))
    (when board-pos
      (let ((row (car board-pos))
            (col (cdr board-pos)))
        (player-guess-cell board row col)))))

;; Function to handle guessing a single cell
(define (player-guess-cell board row col)
  (let ([cell (vector-ref (vector-ref board row) col)])
    (cond
      [(or (eq? cell 'hit) (eq? cell 'miss))
       (printf "Cell at ~a, ~a was already guessed.~n" row col)]
      [(eq? cell 'ship)
       (vector-set! (vector-ref board row) col 'hit)
       (printf "Hit at ~a, ~a!~n" row col)]
      [else
       (vector-set! (vector-ref board row) col 'miss)
       (printf "Miss at ~a, ~a.~n" row col)])))

;; Function to perform a special shot
(define (player-special-shot board row col)
  (cond
    [(eq? special-shot-orientation 'horizontal)
     ;; Attack entire row
     (for ([i (in-range boardSize)])
       (player-guess-cell board row i))]
    [(eq? special-shot-orientation 'vertical)
     ;; Attack entire column
     (for ([i (in-range boardSize)])
       (player-guess-cell board i col))]))

;; Function to check if the game is over (all ships are hit)
(define (check-game-over board total-ship-parts)
  (let ([hit-part-count 0])
    (for ([row (in-vector board)])
      (for ([cell (in-vector row)])
        (when (eq? cell 'hit)
          (set! hit-part-count (+ hit-part-count 1)))))
    (= hit-part-count total-ship-parts)))

;; Function to get adjacent cells (for medium AI)
(define (adjacent-cells row col)
  (define positions
    (list (list (- row 1) col)
          (list (+ row 1) col)
          (list row (- col 1))
          (list row (+ col 1))))
  (filter (lambda (pos)
            (and (>= (car pos) 0) (< (car pos) boardSize)
                 (>= (cadr pos) 0) (< (cadr pos) boardSize)))
          positions))

;; Opponent's (AI) guess function
(define (opponent-guess board)
  (cond
    [(eq? ai-difficulty 'easy)
     ;; Easy AI: random guessing
     (let loop ()
       (let* ((row (random boardSize))
              (col (random boardSize)))
         (if (or (eq? (vector-ref (vector-ref board row) col) 'hit)
                 (eq? (vector-ref (vector-ref board row) col) 'miss))
             (loop)
             (if (eq? (vector-ref (vector-ref board row) col) 'ship)
                 (begin
                   (vector-set! (vector-ref board row) col 'hit)
                   (printf "Opponent hit at ~a, ~a!~n" row col))
                 (begin
                   (vector-set! (vector-ref board row) col 'miss)
                   (printf "Opponent missed at ~a, ~a.~n" row col))))))]
    [(eq? ai-difficulty 'medium)
     ;; Medium AI: smarter guessing
     (opponent-guess-medium board)]
    [(eq? ai-difficulty 'hard)
     ;; Hard AI: knows player's ship locations
     (opponent-guess-hard board)]))

;; Medium AI guess function
(define (opponent-guess-medium board)
  (if (not (null? ai-target-cells))
      ;; If there are target cells, try them
      (let* ((pos (car ai-target-cells))
             (row (car pos))
             (col (cadr pos)))
        (set! ai-target-cells (cdr ai-target-cells))
        (if (or (eq? (vector-ref (vector-ref board row) col) 'hit)
                (eq? (vector-ref (vector-ref board row) col) 'miss))
            (opponent-guess-medium board) ; Cell already guessed, try next
            (if (eq? (vector-ref (vector-ref board row) col) 'ship)
                (begin
                  (vector-set! (vector-ref board row) col 'hit)
                  (printf "Opponent hit at ~a, ~a!~n" row col)
                  (set! ai-hit-cells (cons (list row col) ai-hit-cells))
                  ;; Add adjacent cells to target list
                  (set! ai-target-cells (append (adjacent-cells row col) ai-target-cells)))
                (begin
                  (vector-set! (vector-ref board row) col 'miss)
                  (printf "Opponent missed at ~a, ~a.~n" row col)))))
      ;; Else: No target cells, make a random guess
      (let loop ()
        (let* ((row (random boardSize))
               (col (random boardSize)))
          (if (or (eq? (vector-ref (vector-ref board row) col) 'hit)
                  (eq? (vector-ref (vector-ref board row) col) 'miss))
              (loop) ; Cell already guessed, try next
              (if (eq? (vector-ref (vector-ref board row) col) 'ship)
                  (begin
                    (vector-set! (vector-ref board row) col 'hit)
                    (printf "Opponent hit at ~a, ~a!~n" row col)
                    (set! ai-hit-cells (cons (list row col) ai-hit-cells))
                    ;; Add adjacent cells to target list
                    (set! ai-target-cells (append (adjacent-cells row col) ai-target-cells)))
                  (begin
                    (vector-set! (vector-ref board row) col 'miss)
                    (printf "Opponent missed at ~a, ~a.~n" row col))))))))

;; Hard AI guess function
(define (opponent-guess-hard board)
  ;; Hard AI knows player's ship locations and only targets them
  (let loop ((row 0) (col 0))
    (cond
      [(>= row boardSize) (void)] ; All rows checked
      [else
       (cond
         [(>= col boardSize) (loop (+ row 1) 0)] ; Move to next row
         [else
          (if (and (eq? (vector-ref (vector-ref board row) col) 'ship)
                   (not (eq? (vector-ref (vector-ref board row) col) 'hit)))
              (begin
                (vector-set! (vector-ref board row) col 'hit)
                (printf "Opponent hit at ~a, ~a!~n" row col))
              (loop row (+ col 1)))])])))

;; Function to change state using a state stack
(define (pushState newState)
  (set! stateStack (cons currentState stateStack))
  (set! currentState newState))

(define (popState)
  (when (not (null? stateStack))
    (set! currentState (car stateStack))
    (set! stateStack (cdr stateStack))))

;; Function to approximate text centering horizontally
(define (center-text x y width text-str)
  (let* ((font-width (font-advance))
         (text-length (* (string-length text-str) font-width))
         (text-x (+ x (/ (- width text-length) 2))))
    (text text-x y text-str)))

;; Game update function
(define (update state)
  (let* ((mouseX (mouse-x))
         (mouseY (mouse-y))
         (mouseClicked (btn-mouse)))
    ;; Check for help key
    (when (btn-right)
      (pushState help-screen))
    (cond
      ;; Help Screen State
      [(eq? currentState help-screen)
       ;; Return to previous state on mouse click
       (when mouseClicked
         (popState))]
      ;; Home state - transition to game mode selection or exit
      [(eq? currentState home)
       ;; Start Game Button
       (when (and (mouse-in? mouseX mouseY 300 200 button-width button-height)
                  mouseClicked)
         ;; Change to help screen
         (pushState game-mode-selection)
         (pushState help-screen))
       ;; Exit Button
       (when (and (mouse-in? mouseX mouseY 300 300 button-width button-height)
                  mouseClicked)
         (exit))]
      ;; Game Mode Selection State
      [(eq? currentState game-mode-selection)
       ;; Player vs AI Button
       (when (and (mouse-in? mouseX mouseY 300 200 button-width button-height)
                  mouseClicked)
         (set! game-mode '1-player-vs-ai)
         (pushState select-ai-difficulty))
       ;; 2-Player Button
       (when (and (mouse-in? mouseX mouseY 300 300 button-width button-height)
                  mouseClicked)
         (set! game-mode '2-player)
         (set! currentState ship-selection))]
      ;; AI Difficulty Selection State
      [(eq? currentState select-ai-difficulty)
       ;; Easy AI Button
       (when (and (mouse-in? mouseX mouseY 300 400 button-width button-height)
                  mouseClicked)
         (set! ai-difficulty 'easy)
         (set! player1-special-shots 1)
         (set! currentState ship-selection))
       ;; Medium AI Button
       (when (and (mouse-in? mouseX mouseY 300 500 button-width button-height)
                  mouseClicked)
         (set! ai-difficulty 'medium)
         (set! player1-special-shots 3)
         (set! currentState ship-selection))
       ;; Hard AI Button
       (when (and (mouse-in? mouseX mouseY 300 600 button-width button-height)
                  mouseClicked)
         (set! ai-difficulty 'hard)
         (set! player1-special-shots 5)
         (set! currentState ship-selection))]
      ;; Ship Selection State
      [(eq? currentState ship-selection)
       (for ([i (in-range 5)])
         (let ((option-y (+ 60 (* i 50))))
           (when (and (mouse-in? mouseX mouseY 350 option-y 100 40)
                      mouseClicked)
             (set! num-ships (+ i 1))
             (set! ship-sizes (reverse (build-list num-ships (lambda (i) (+ i 1)))))
             (set! currentState ship-placement))))
       ;; Error handling: Ensure at least one ship is selected
       (when (and mouseClicked (< num-ships 1))
         (printf "Please select at least one ship.~n"))]
      ;; Ship Placement State
      [(eq? currentState ship-placement)
       ;; Toggle orientation on LEFT arrow key press
       (when (btn-left)
         (set! ship-orientation (if (eq? ship-orientation 'horizontal) 'vertical 'horizontal)))
       ;; Determine current player's board and placement status
       (define current-board (if (= current-player 1) player1-board (if (eq? game-mode '2-player) player2-board opponentBoard)))
       ;; Get the number of ships placed for the current player
       (define get-ships-placed (if (= current-player 1) player1-ships-placed player2-ships-placed))
       ;; Place ships
       (when (and mouseClicked (< get-ships-placed num-ships))
         (let* ((board-pos (mouse-to-board mouseX mouseY x-offset y-offset))
                (current-ship-size (list-ref ship-sizes get-ships-placed)))
           (when (and board-pos
                      (can-place-ship? current-board (car board-pos) (cdr board-pos) current-ship-size ship-orientation))
             (place-ship current-board (car board-pos) (cdr board-pos) current-ship-size ship-orientation)
             (if (= current-player 1)
                 (set! player1-ships-placed (+ player1-ships-placed 1))
                 (set! player2-ships-placed (+ player2-ships-placed 1))))))
       ;; Check if all ships are placed
       (when (= (if (= current-player 1) player1-ships-placed player2-ships-placed) num-ships)
         (if (= current-player 1)
             (begin
               (set! player1-total-ship-parts (apply + ship-sizes))
               (if (eq? game-mode '2-player)
                   ;; Switch to Player 2 for ship placement
                   (begin
                     (set! current-player 2)
                     (set! ships-placed-locations '())
                     ;; Reset ship placement variables for Player 2
                     (set! ship-orientation 'horizontal)
                     (printf "Player 2, it's your turn to place ships.\n"))
                   ;; For 1-player vs AI, proceed to start the game
                   (begin
                     (place-opponent-ships ship-sizes)
                     (set! opponent-total-ship-parts (apply + ship-sizes))
                     (set! currentState in-play)
                     (coinToss))))
             ;; After Player 2 places ships, start the game
             (begin
               (set! player2-total-ship-parts (apply + ship-sizes))
               (set! currentState in-play)
               (coinToss))))
       ;; Revert Button
       (when (and (mouse-in? mouseX mouseY 300 650 button-width button-height) mouseClicked
                  (> (if (= current-player 1) player1-ships-placed player2-ships-placed) 0))
         (remove-ship current-board (first ships-placed-locations))
         (if (= current-player 1)
             (set! player1-ships-placed (- player1-ships-placed 1))
             (set! player2-ships-placed (- player2-ships-placed 1))))]
      ;; In-Play State
      [(eq? currentState in-play)
       ;; Check for special shot activation
       (when (and (btn-up)
                  (not special-shot-active)
                  (> (if (= playerTurn 1)
                         player1-special-shots
                         player2-special-shots)
                     0))
         (set! special-shot-active #t)
         (printf "Special shot activated! Click on the opponent's grid to use it.~n"))
       ;; Toggle special shot orientation on LEFT arrow key press during special shot
       (when (and (btn-left) special-shot-active)
         (set! special-shot-orientation (if (eq? special-shot-orientation 'horizontal) 'vertical 'horizontal)))
       ;; Check for down arrow key during the game to open pause menu
       (when (btn-down)
         (pushState pause-menu))
       ;; Player turn handling
       (cond
         ;; Player 1's turn
         [(= playerTurn 1)
          ;; Handle clicks for Player 1's turn
          (when mouseClicked
            (let ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset)))
              (when board-pos
                (let ((opponent-board (if (eq? game-mode '2-player) player2-board opponentBoard))
                      (opponent-total-parts (if (eq? game-mode '2-player) player2-total-ship-parts opponent-total-ship-parts)))
                  (if special-shot-active
                      (let* ((row (car board-pos))
                             (col (cdr board-pos)))
                        ;; Apply special shot
                        (player-special-shot opponent-board row col)
                        ;; Decrement special shots
                        (set! player1-special-shots (- player1-special-shots 1))
                        (set! special-shot-active #f)
                        ;; Check if Player 1 has won
                        (when (check-game-over opponent-board opponent-total-parts)
                          (printf "Player 1 wins! All opponent's ships are hit!~n")
                          (set! currentState game-over))
                        ;; Switch to Player 2's turn or AI's turn
                        (set! playerTurn (if (eq? game-mode '2-player) 2 0)))
                      ;; Else, normal shot
                      (if (or (eq? (vector-ref (vector-ref opponent-board (car board-pos))
                                               (cdr board-pos))
                                    'hit)
                              (eq? (vector-ref (vector-ref opponent-board (car board-pos))
                                               (cdr board-pos))
                                    'miss))
                          (printf "Cell was already guessed. Try a different one!~n")
                          (begin
                            (player-guess opponent-board mouseX mouseY)
                            ;; Check if Player 1 has won
                            (when (check-game-over opponent-board opponent-total-parts)
                              (printf "Player 1 wins! All opponent's ships are hit!~n")
                              (set! currentState game-over))
                            ;; Switch to Player 2's turn or AI's turn
                            (set! playerTurn (if (eq? game-mode '2-player) 2 0)))))))))]
         ;; Player 2's turn
         [(and (= playerTurn 2) (eq? game-mode '2-player))
          ;; Handle clicks for Player 2's turn
          (when mouseClicked
            (let ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset)))
              (when board-pos
                ;; Ensure Player 2 interacts with Player 1's board (top grid)
                (let ((opponent-total-parts player1-total-ship-parts))
                  (if special-shot-active
                      (let* ((row (car board-pos))
                             (col (cdr board-pos)))
                        ;; Apply special shot
                        (player-special-shot player1-board row col)
                        ;; Decrement special shots
                        (set! player2-special-shots (- player2-special-shots 1))
                        (set! special-shot-active #f)
                        ;; Check if Player 2 has won
                        (when (check-game-over player1-board opponent-total-parts)
                          (printf "Player 2 wins! All Player 1's ships are hit!~n")
                          (set! currentState game-over))
                        ;; Switch back to Player 1's turn
                        (set! playerTurn 1))
                      ;; Else, normal shot
                      (if (or (eq? (vector-ref (vector-ref player1-board (car board-pos))
                                               (cdr board-pos))
                                    'hit)
                              (eq? (vector-ref (vector-ref player1-board (car board-pos))
                                               (cdr board-pos))
                                    'miss))
                          (printf "Cell was already guessed. Try a different one!~n")
                          (begin
                            (player-guess player1-board mouseX mouseY)
                            ;; Check if Player 2 has won
                            (when (check-game-over player1-board opponent-total-parts)
                              (printf "Player 2 wins! All Player 1's ships are hit!~n")
                              (set! currentState game-over))
                            ;; Switch back to Player 1's turn
                            (set! playerTurn 1))))))))]
         ;; AI's turn
         [(and (= playerTurn 0) (eq? game-mode '1-player-vs-ai))
          ;; AI's turn
          (begin
            (printf "Opponent is taking its turn...\n")
            (opponent-guess player1-board)
            ;; Check if AI has won
            (when (check-game-over player1-board player1-total-ship-parts)
              (printf "Opponent wins! All your ships are hit!~n")
              (set! currentState game-over))
            ;; Switch back to Player's turn if game not over
            (unless (eq? currentState game-over)
              (set! playerTurn 1)))]
         [else
          (printf "Invalid playerTurn or game-mode in in-play state.\n")])]
      ;; Pause Menu State
      [(eq? currentState pause-menu)
       ;; Handle button clicks in pause menu
       (when mouseClicked
         (cond
           ;; Resume Game Button
           [(mouse-in? mouseX mouseY 300 400 button-width button-height)
            ;; Return to the game
            (popState)]
           ;; Main Menu Button
           [(mouse-in? mouseX mouseY 300 500 button-width button-height)
            ;; Reset game variables and go to home
            (set! player1-board (createBoard boardSize))
            (set! player2-board (createBoard boardSize))
            (set! opponentBoard (createBoard boardSize))
            (set! player1-ships-placed 0)
            (set! player2-ships-placed 0)
            (set! current-player 1)
            (set! ships-placed-locations '())
            (set! ship-orientation 'horizontal)
            (set! player1-special-shots 3)
            (set! player2-special-shots 3)
            (set! special-shot-active #f)
            (set! special-shot-orientation 'horizontal)
            ;; Reset AI state variables
            (set! ai-hit-cells '())
            (set! ai-target-cells '())
            ;; Reset total ship parts
            (set! player1-total-ship-parts 0)
            (set! player2-total-ship-parts 0)
            (set! opponent-total-ship-parts 0)
            ;; Clear the state stack and set current state to home
            (set! stateStack '())
            (set! currentState home)]
           ;; Exit Game Button
           [(mouse-in? mouseX mouseY 300 600 button-width button-height)
            (exit)]))]
      ;; Game Over State
      [(eq? currentState game-over)
       ;; Handle button clicks in game over state
       (when mouseClicked
         (cond
           ;; Main Menu Button
           [(mouse-in? mouseX mouseY 300 400 button-width button-height)
            ;; Reset game variables
            (set! player1-board (createBoard boardSize))
            (set! player2-board (createBoard boardSize))
            (set! opponentBoard (createBoard boardSize))
            (set! player1-ships-placed 0)
            (set! player2-ships-placed 0)
            (set! current-player 1)
            (set! ships-placed-locations '())
            (set! ship-orientation 'horizontal)
            (set! player1-special-shots 3)
            (set! player2-special-shots 3)
            (set! special-shot-active #f)
            (set! special-shot-orientation 'horizontal)
            ;; Reset AI state variables
            (set! ai-hit-cells '())
            (set! ai-target-cells '())
            ;; Reset total ship parts
            (set! player1-total-ship-parts 0)
            (set! player2-total-ship-parts 0)
            (set! opponent-total-ship-parts 0)
            ;; Clear the state stack and set current state to home
            (set! stateStack '())
            (set! currentState home)]
           ;; Restart Game Button
           [(mouse-in? mouseX mouseY 300 500 button-width button-height)
            ;; Reset game variables and start ship selection
            (set! player1-board (createBoard boardSize))
            (set! player2-board (createBoard boardSize))
            (set! opponentBoard (createBoard boardSize))
            (set! player1-ships-placed 0)
            (set! player2-ships-placed 0)
            (set! current-player 1)
            (set! ships-placed-locations '())
            (set! ship-orientation 'horizontal)
            (set! player1-special-shots 3)
            (set! player2-special-shots 3)
            (set! special-shot-active #f)
            (set! special-shot-orientation 'horizontal)
            ;; Reset AI state variables
            (set! ai-hit-cells '())
            (set! ai-target-cells '())
            ;; Reset total ship parts
            (set! player1-total-ship-parts 0)
            (set! player2-total-ship-parts 0)
            (set! opponent-total-ship-parts 0)
            ;; Clear the state stack and set current state to ship selection
            (set! stateStack '())
            (set! currentState ship-selection)]
           ;; Exit Game Button
           [(mouse-in? mouseX mouseY 300 600 button-width button-height)
            (exit)]))])))

;; Function to Draw the State
(define (draw state)
  (begin
    (cls)
    (cond
      ;; Help Screen
      [(eq? currentState help-screen)
       (font wide-font)
       (color 7)
       (text 50 50 "Help and Instructions:")
       (text 50 100 "1. Use the mouse to interact with the game.")
       (text 50 130 "2. During ship placement, click on the grid to place ships.")
       (text 50 160 "     - Press LEFT arrow to toggle ship orientation.")
       (text 50 190 "3. During your turn, click on the opponent's grid to attack.")
       (text 50 220 "4. Press UP arrow key to activate a special shot if available.")
       (text 50 250 "     - Special shot will target an entire row or column.")
       (text 50 280 "5. Press LEFT arrow to toggle between row and column.")
       (text 50 310 "6. Press RIGHT arrow key at any time to view this help screen.")
       (text 50 340 "7. Press DOWN arrow key at any time to open the pause menu.")
       (text 50 370 "Click anywhere to continue.")]
      ;; Pause Menu
      [(eq? currentState pause-menu)
       (font wide-font)
       (color 7)
       (text 300 100 "Game Paused")
       ;; Draw buttons
       (color 7)
       ;; Resume Game Button
       (rect 300 400 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 425 button-width "Resume Game")
       ;; Main Menu Button
       (color 7)
       (rect 300 500 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 525 button-width "Main Menu")
       ;; Exit Game Button
       (color 7)
       (rect 300 600 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 625 button-width "Exit Game")]
      ;; Draw Home Menu
      [(eq? currentState home)
       (color 7)
       (rect 300 200 button-width button-height #:fill #t)
       (color 0)
       (text 340 225 "Start Game")
       (rect 300 300 button-width button-height #:fill #t)
       (color 0)
       (text 340 325 "Exit")
       (font wide-font)
       (color 7)
       (text 300 100 "Welcome to Battleship!")]
      ;; Draw Game Mode Selection
      [(eq? currentState game-mode-selection)
       (color 7)
       (text 315 100 "Select Game Mode")
       ;; Draw Player vs A.I. button
       (rect 300 200 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 225 button-width "1-Player vs A.I.")
       ;; Draw 2-Player button
       (color 7)
       (rect 300 300 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 325 button-width "2-Player")]
      ;; Draw AI Difficulty Selection
      [(eq? currentState select-ai-difficulty)
       (color 7)
       (font wide-font)
       (text 300 100 "Select AI Difficulty")
       ;; Draw Easy AI button
       (rect 300 400 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 425 button-width "Easy AI")
       ;; Draw Medium AI button
       (color 7)
       (rect 300 500 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 525 button-width "Medium AI")
       ;; Draw Hard AI button
       (color 7)
       (rect 300 600 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 625 button-width "Hard AI")]
      ;; Draw Ship Selection Screen
      [(eq? currentState ship-selection)
       (color 0)
       (font wide-font)
       (text 20 20 "Select the number of ships:")
       (for ([i (in-range 5)])
         (let ((option-y (+ 60 (* i 50))))
           (color 7)
           (rect 350 option-y 100 40 #:fill #t)
           (color 0)
           (text 375 (+ option-y 20) (format "~a Ships" (+ i 1)))))
       (text 20 300 "Click on a number to select the number of ships.")]
      ;; Draw Ship Placement State
      [(eq? currentState ship-placement)
       (color 7)
       (font wide-font)
       (text 175 50 (format "~a, Place Your Ships! Press LEFT arrow to toggle orientation."
                            (if (= current-player 1) "Player 1" (if (eq? game-mode '2-player) "Player 2" "Player"))))
       ;; Determine which board to draw for the current player
       (define current-board (if (= current-player 1) player1-board (if (eq? game-mode '2-player) player2-board opponentBoard)))
       ;; Get the current ship size
       (define get-ships-placed (if (= current-player 1) player1-ships-placed player2-ships-placed))
       (define current-ship-size (if (< get-ships-placed num-ships)
                                     (list-ref ship-sizes get-ships-placed)
                                     0))
       ;; Draw the grid with highlighting
       (draw-grid-with-highlight x-offset y-offset current-board #t current-ship-size ship-orientation)
       ;; Draw Revert button
       (color 7)
       (rect 300 650 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 675 button-width "Revert Ship")]
      ;; Draw In-Play State
      [(eq? currentState in-play)
       (font wide-font)
       (text 20 20 (if (= playerTurn 1) "Player 1's Turn" (if (eq? game-mode '2-player) "Player 2's Turn" "Opponent's Turn")))
       ;; Show special shots remaining
       (font wide-font)
       (text 20 60 (format "Special Shots Remaining: ~a"
                           (if (= playerTurn 1) player1-special-shots player2-special-shots)))
       ;; Draw the boards based on the current turn
       (if (= playerTurn 1)
           (begin
             ;; Player's turn: show opponent's board on top, player's board on bottom
             (text 330 10 (if (eq? game-mode '2-player) "Player 2's Board" "Opponent's Board"))
             (if special-shot-active
                 (draw-grid-with-special-highlight x-offset opponent-y-offset (if (eq? game-mode '2-player) player2-board opponentBoard) #f)
                 (draw-grid x-offset opponent-y-offset (if (eq? game-mode '2-player) player2-board opponentBoard) #f))
             (text 355 450 "Your Board")
             (draw-grid x-offset player-y-offset player1-board #t))
           (if (eq? game-mode '2-player)
               (begin
                 ;; Player 2's turn: show Player 1's board on top, Player 2's board on bottom
                 (text 330 10 "Player 1's Board")
                 (if special-shot-active
                     (draw-grid-with-special-highlight x-offset opponent-y-offset player1-board #f)
                     (draw-grid x-offset opponent-y-offset player1-board #f))
                 (text 355 450 "Your Board")
                 (draw-grid x-offset player-y-offset player2-board #t))
               ;; Opponent's turn (AI): show message
               (begin
                 (text 330 10 "Opponent is taking its turn...")
                 (draw-grid x-offset opponent-y-offset player1-board #f)
                 (draw-grid x-offset player-y-offset player1-board #t))))]
      ;; Draw Game Over State
      [(eq? currentState game-over)
       (font wide-font)
       (color 7)
       (text 300 100 "Game Over!")
       (text 300 200
             (cond
               [(and (eq? game-mode '1-player-vs-ai) (= playerTurn 1)) "You Lose!"]
               [(and (eq? game-mode '1-player-vs-ai) (= playerTurn 0)) "You Win!"]
               [(and (eq? game-mode '2-player) (= playerTurn 1)) "Player 2 Wins!"]
               [(and (eq? game-mode '2-player) (= playerTurn 2)) "Player 1 Wins!"]))
       ;; Draw buttons
       (color 7)
       ;; Main Menu Button
       (rect 300 400 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 425 button-width "Main Menu")
       ;; Restart Game Button
       (color 7)
       (rect 300 500 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 525 button-width "Restart Game")
       ;; Exit Game Button
       (color 7)
       (rect 300 600 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 625 button-width "Exit Game")])))

;;-------------Run Game---------------;;
(define (game-loop)
  (begin
    (update currentState)
    (draw currentState)))

(run game-loop
     800 ; Width of the window
     900 ; Height of the window
     #:fps 60) ; Set the frame rate to 60 FPS

