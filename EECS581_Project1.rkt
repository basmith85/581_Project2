#|
------------------Prologue--------------------
Program Name: EECS581_Project1
Description: Racket based Battleship game using the R-Cade game engine. Players can play against a random guessing AI or PvP Pass and Play. 

Reqiruments: Dr.Racket IDE (https://racket-lang.org)
Libraries: R-Cade (https://r-cade.io)
R-Cade Documentation: https://docs.racket-lang.org/r-cade/index.html

Current Bug: After implmenting 2-Player, the Player vs. AI gameplay is not working properly. It swaps
the boards like it's a pass and play game. In the push history, the "Game Over" push has a
function Player vs. AI implementation.

Input: Mouse and Key input in order to manage/play the game
Output:
	- Displays a 800x900 window with the running battleship game. It will always start at the home state

Author: Darshil Patel
Date Created: 9/26/24
--------------------------------------------
|#



#lang racket
(require r-cade) ; Import R-Cade library for game development
(require racket/match) ; Import match library for pattern matching

;;-------------Initialization---------------;;

;; Define States
(define home 0) ; Main Menu State
(define game-mode-selection 1) ; State to select Player vs Player or Player vs AI
(define select-ai-difficulty 2) ; State to select AI difficulty
(define ship-selection 3) ; State to select the number of ships
(define ship-placement 4) ; State to place ships on the board
(define in-play 5) ; Game is running
(define game-over 6) ; Game over state, if all ships are hit

;; Declare Variables
(define boardSize 10) ; Dimension of the board (10x10 grid)
(define cellSize 40) ; Pixel size of individual cells on the board
(define x-offset 200) ; X offset for drawing the grid
(define y-offset 200) ; Y offset for drawing the grid
(define button-width 160) ; Width of buttons in pixels
(define button-height 60) ; Height of buttons in pixels
(define currentState home) ; Current state of the game, starting at the main menu
(define num-ships 0) ; Number of ships chosen by the player
(define opponent-y-offset 50) ; Y offset for the opponent's grid
(define player-y-offset 465) ; Y offset for the player's grid
(define playerTurn 0) ; 1 for Player 1's turn, 2 for Player 2's turn, 0 for AI's turn
(define game-mode '2-player) ; Default game mode is 2-player
(define player1-ships-placed 0) ; Number of ships placed by Player 1
(define player2-ships-placed 0) ; Number of ships placed by Player 2
(define current-player 1) ; Tracks the current player (1 or 2)
(define ai-difficulty 'easy) ; Default AI difficulty is easy

;; Track ship sizes and placements
(define ship-sizes '()) ; List of ship sizes
(define ships-placed-locations '()) ; List of ship locations on the board
(define ship-orientation 'horizontal) ; Default ship orientation is horizontal

;; Variables for Special Shooting Feature
(define player1-special-shots 3)
(define player2-special-shots 3)
(define special-shot-active #f)
(define special-shot-type #f) ; 'row' or 'column'

;; Initialize a board with vectors using a given size
(define (createBoard size)
  (let ([board (make-vector size)])
    (for ([i (in-range size)])
      (vector-set! board i (make-vector size #f)))
    board))

;; Create boards/grids using the createBoard function
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
          [(and (eq? cell #t) showShips)
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
        (when (and board-pos)
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
            [(and (eq? cell #t) showShips)
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
            (not (vector-ref (vector-ref board row) (+ col i)))))]
    [(eq? orientation 'vertical)
     (and (<= (+ row size) boardSize)
          (for/and ([i (in-range size)])
            (not (vector-ref (vector-ref board (+ row i)) col))))]))

;; Places a ship on the board
(define (place-ship board row col size orientation)
  (for ([i (in-range size)])
    (if (eq? orientation 'horizontal)
        (vector-set! (vector-ref board row) (+ col i) #t)
        (vector-set! (vector-ref board (+ row i)) col #t)))
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
            (begin
              (place-ship opponentBoard row col size orientation) ; Place the ship
              (printf "Placed opponent ship of size ~a at row ~a, col ~a, orientation ~a~n" size row col orientation)) ; Debug output
            (loop)))))) ; Retry if placement is invalid

;; Removes the most recently added ship from the board
(define (remove-ship board ship)
  (let* ((row (first ship))
         (col (second ship))
         (size (third ship))
         (orientation (fourth ship)))
    (for ([i (in-range size)])
      (if (eq? orientation 'horizontal)
          (vector-set! (vector-ref board row) (+ col i) #f)
          (vector-set! (vector-ref board (+ row i)) col #f))))
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
  (cond
    [(or (eq? (vector-ref (vector-ref board row) col) 'hit)
         (eq? (vector-ref (vector-ref board row) col) 'miss))
     (printf "Cell at ~a, ~a was already guessed.~n" row col)]
    [(eq? (vector-ref (vector-ref board row) col) #t)
     (vector-set! (vector-ref board row) col 'hit)
     (printf "Hit at ~a, ~a!~n" row col)]
    [else
     (vector-set! (vector-ref board row) col 'miss)
     (printf "Miss at ~a, ~a.~n" row col)]))

;; Function to check if the game is over (all ships are hit)
(define (check-game-over board)
  (let ([ship-part-count 0]
        [hit-part-count 0])
    (for ([row (in-vector board)])
      (for ([cell (in-vector row)])
        (when (eq? cell #t)
          (set! ship-part-count (+ ship-part-count 1)))
        (when (eq? cell 'hit)
          (set! hit-part-count (+ hit-part-count 1)))))
    (and (> ship-part-count 0) (= ship-part-count hit-part-count))))

;; Opponent's (AI) guess function
(define (opponent-guess board)
  (let loop ()
    (let* ((row (random boardSize))
           (col (random boardSize)))
      (if (or (eq? (vector-ref (vector-ref board row) col) 'hit)
              (eq? (vector-ref (vector-ref board row) col) 'miss))
          (loop)
          (if (eq? (vector-ref (vector-ref board row) col) #t)
              (begin
                (vector-set! (vector-ref board row) col 'hit)
                (printf "Opponent hit at ~a, ~a!~n" row col))
              (begin
                (vector-set! (vector-ref board row) col 'miss)
                (printf "Opponent missed at ~a, ~a.~n" row col)))))))

;; Game update function
(define (update state)
  (let* ((mouseX (mouse-x))
         (mouseY (mouse-y))
         (mouseClicked (btn-mouse)))
    (cond
      ;; Home state - transition to game mode selection or exit
      [(eq? currentState home)
       ;; Start Game Button
       (when (and (mouse-in? mouseX mouseY 300 200 button-width button-height)
                  mouseClicked)
         (set! currentState game-mode-selection))
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
         (set! currentState select-ai-difficulty))
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
         (set! currentState ship-selection))
       ;; Medium AI Button
       (when (and (mouse-in? mouseX mouseY 300 500 button-width button-height)
                  mouseClicked)
         (set! ai-difficulty 'medium)
         (set! currentState ship-selection))
       ;; Hard AI Button
       (when (and (mouse-in? mouseX mouseY 300 600 button-width button-height)
                  mouseClicked)
         (set! ai-difficulty 'hard)
         (set! currentState ship-selection))]
      ;; Ship Selection State
      [(eq? currentState ship-selection)
       (for ([i (in-range 5)])
         (let ((option-y (+ 60 (* i 50))))
           (when (and (mouse-in? mouseX mouseY 350 option-y 100 40)
                      mouseClicked)
             (set! currentState ship-placement)
             (set! num-ships (+ i 1))
             (set! ship-sizes (reverse (build-list num-ships add1))))))
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
         ;; If player 1 finished placing ships, switch to player 2 or start game
         (if (and (= current-player 1) (eq? game-mode '2-player))
             (begin
               (set! current-player 2)
               (set! ships-placed-locations '())
               (set! currentState ship-placement))
             (begin
               ;; Start Game Button
               (when (and (mouse-in? mouseX mouseY 300 750 button-width button-height) mouseClicked)
                 (if (eq? game-mode '2-player)
                     (begin
                       (set! currentState in-play)
                       (coinToss))
                     (begin
                       ;; For 1-player vs AI
                       (place-opponent-ships ship-sizes)
                       (set! currentState in-play)
                       (coinToss)))))))
       ;; Revert Button
       (when (and (mouse-in? mouseX mouseY 300 650 button-width button-height) mouseClicked
                  (> (if (= current-player 1) player1-ships-placed player2-ships-placed) 0))
         (remove-ship current-board (first ships-placed-locations))
         (if (= current-player 1)
             (set! player1-ships-placed (- player1-ships-placed 1))
             (set! player2-ships-placed (- player2-ships-placed 1))))]
      ;; In-Play State
      [(eq? currentState in-play)
       (cond
         ;; If it's Player 1's turn
         [(= playerTurn 1)
          ;; Handle clicks for Player 1's turn
          (when mouseClicked
            (let ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset)))
              (when board-pos
                (define opponent-board (if (eq? game-mode '2-player) player2-board opponentBoard))
                (if (or (eq? (vector-ref (vector-ref opponent-board (car board-pos)) (cdr board-pos)) 'hit)
                        (eq? (vector-ref (vector-ref opponent-board (car board-pos)) (cdr board-pos)) 'miss))
                    (printf "Cell was already guessed. Try a different one!~n")
                    (begin
                      (player-guess opponent-board mouseX mouseY)
                      ;; Check if Player 1 has won
                      (when (check-game-over opponent-board)
                        (printf "Player 1 wins! All opponent's ships are hit!~n")
                        (set! currentState game-over))
                      ;; Switch to Player 2's turn or AI's turn
                      (set! playerTurn (if (eq? game-mode '2-player) 2 0)))))))]
         ;; If it's Player 2's turn or AI's turn
         [else
          (if (eq? game-mode '2-player)
              (begin
                ;; Handle clicks for Player 2's turn
                (when mouseClicked
                  (let ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset)))
                    (when board-pos
                      ;; Ensure Player 2 interacts with Player 1's board (top grid)
                      (if (or (eq? (vector-ref (vector-ref player1-board (car board-pos)) (cdr board-pos)) 'hit)
                              (eq? (vector-ref (vector-ref player1-board (car board-pos)) (cdr board-pos)) 'miss))
                          (printf "Cell was already guessed. Try a different one!~n")
                          (begin
                            (player-guess player1-board mouseX mouseY)
                            ;; Check if Player 2 has won
                            (when (check-game-over player1-board)
                              (printf "Player 2 wins! All Player 1's ships are hit!~n")
                              (set! currentState game-over))
                            ;; Switch back to Player 1's turn
                            (set! playerTurn 1)))))))
              ;; AI's turn
              (begin
                ;; Opponent (AI) guesses on Player 1's board
                (opponent-guess player1-board)
                ;; Check if AI has won
                (when (check-game-over player1-board)
                  (printf "Opponent wins! All your ships are hit!~n")
                  (set! currentState game-over))
                ;; Switch back to Player's turn
                (set! playerTurn 1)))])]
      ;; Game Over State
      [(eq? currentState game-over)
       (when mouseClicked
         ;; Reset the game to the home state
         (set! currentState home)
         ;; Reset variables
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
         (set! special-shot-type #f))])))

;; Function to approximate text centering horizontally
(define (center-text x y width text-str)
  (let* ((font-width (font-advance))
         (text-length (* (string-length text-str) font-width))
         (text-x (+ x (/ (- width text-length) 2))))
    (text text-x y text-str)))

;; Function to Draw the State
(define (draw state)
  (begin
    (cls)
    (cond
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
       (font wide-font)
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
       (font tall-font)
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
       ;; Draw Start Game button after all ships placed for the current player
       (when (= (if (= current-player 1) player1-ships-placed player2-ships-placed) num-ships)
         (color 7)
         (rect 300 750 button-width button-height #:fill #t)
         (color 0)
         (center-text 300 775 button-width "Start Game"))
       ;; Draw Revert button
       (color 7)
       (rect 300 650 button-width button-height #:fill #t)
       (color 0)
       (center-text 300 675 button-width "Revert Ship")]
      ;; Draw In-Play State
      [(eq? currentState in-play)
       (font wide-font)
       (text 20 20 (if (= playerTurn 1) "Player 1's Turn" (if (eq? game-mode '2-player) "Player 2's Turn" "Opponent's Turn")))
       ;; Draw the boards based on the current turn
       (if (= playerTurn 1)
           (begin
             ;; Player's turn: show opponent's board on top, player's board on bottom
             (text 330 10 (if (eq? game-mode '2-player) "Player 2's Board" "Opponent's Board"))
             (draw-grid x-offset opponent-y-offset (if (eq? game-mode '2-player) player2-board opponentBoard) #f)
             (text 355 450 "Your Board")
             (draw-grid x-offset player-y-offset player1-board #t))
           (if (eq? game-mode '2-player)
               (begin
                 ;; Player 2's turn: show Player 1's board on top, Player 2's board on bottom
                 (text 330 10 "Player 1's Board")
                 (draw-grid x-offset opponent-y-offset player1-board #f)
                 (text 355 450 "Your Board")
                 (draw-grid x-offset player-y-offset player2-board #t))
               ;; Opponent's turn (AI): show message
               (begin
                 (text 330 10 "Opponent is thinking...")
                 (draw-grid x-offset opponent-y-offset player1-board #f)
                 (draw-grid x-offset player-y-offset player1-board #t))))]
      ;; Draw Game Over State
      [(eq? currentState game-over)
       (font wide-font)
       (color 7)
       (text 300 100 "Game Over!")
       (text 300 200 (if (= playerTurn 1) (if (eq? game-mode '1-player-vs-ai) "You Lose!" "Player 2 Wins!") (if (eq? game-mode '1-player-vs-ai) "You Win!" "Player 1 Wins!")))
       (text 300 300 "Click anywhere to return to the main menu.")])))

;;-------------Run Game---------------;;
;; Game loop function calls both update and draw state each frame
(define (game-loop)
  (begin
    (update currentState)
    (draw currentState)))

;; Start the game loop
(run game-loop
     800 ; Width of the window
     900 ; Height of the window
     #:fps 60) ; Set the frame rate to 60 FPS

     800 ; Width of the window
     900 ; Height of the window
     #:fps 60) ; Set the frame rate to 60 FPS
