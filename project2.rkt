#lang racket

(provide test-checkpoint-code)
(provide solve-sudoku)

(require racket/lazy-require)
(lazy-require ["main.rkt" (print-board get-value set-value run-sudoku)])

; Name: Noah Ong
; VUNet ID: ongnz
; Email: noah.z.ong@vanderbilt.edu
; Date: 2/14/2023
; Course: CS3270
; Honor statement: I have neither given nor received unauthorized help on this assignment.


; Description: Sudoku solver


; Define some global constants.
(define BOARD-SIZE 9)       ; The size of the board.
(define ROWS 9)             ; The number of rows.
(define COLS 9)             ; The number of columns.
(define GRID-SIZE 3)        ; The size of a subgrid.


; Returns the result of checking if a value can be placed at
; a specific row and column on a board.
;
; It is your job to call your implemented "checker" functions
; from this function so it can be tested for the checkpoint
; requirements.
;
; DO NOT change the name/signature of this function, as our
; testing script depends upon it.
;
; @param  board The board to check.
; @param  row   The row where the value will be placed.
; @param  col   The column where the value will be placed.
; @param  num   The number to be placed.
; @return Whether value can be placed at row and col in board.
(define (test-checkpoint-code board row col num)
  (cond [(not (check-row board row col num)) #f]
        [(not (check-col board row col num)) #f]
        [(not (check-box board row col num)) #f]
        [#t])
)

; Checks if the given num is placed in a valid given row
;
; @param board The board to check
; @param row The row where the value will be placed
; @param col The col where the value will be placed
; @param num The number to be placed
; @return True if placing the value into the row is valid
(define (check-row board row col num)
  (cond [(equal? num (get-value board row 0)) #f]
        [(equal? num (get-value board row 1)) #f]
        [(equal? num (get-value board row 2)) #f]
        [(equal? num (get-value board row 3)) #f]
        [(equal? num (get-value board row 4)) #f]
        [(equal? num (get-value board row 5)) #f]
        [(equal? num (get-value board row 6)) #f]
        [(equal? num (get-value board row 7)) #f]
        [(equal? num (get-value board row 8)) #f]
        [#t]))

; Checks if the given num is placed in a valid given col
;
; @param board The board to check
; @param row The row where the value will be placed
; @param col The col where the value will be placed
; @param num The number to be placed
; @return True if placing the value into the col is valid
(define (check-col board row col num)
  (cond [(equal? num (get-value board 0 col)) #f]
        [(equal? num (get-value board 1 col)) #f]
        [(equal? num (get-value board 2 col)) #f]
        [(equal? num (get-value board 3 col)) #f]
        [(equal? num (get-value board 4 col)) #f]
        [(equal? num (get-value board 5 col)) #f]
        [(equal? num (get-value board 6 col)) #f]
        [(equal? num (get-value board 7 col)) #f]
        [(equal? num (get-value board 8 col)) #f]
        [#t]))

; Checks if the given num is placed in a valid given box
;
; @param board The board to check
; @param row The row where the value will be placed
; @param col The col where the value will be placed
; @param num The number to be placed
; @return True if placing the value into the box is valid
(define (check-box board row col num)
  (let ([row-start (* 3 (quotient row 3))]
        [col-start (* 3 (quotient col 3))])
    (cond [(equal? num (get-value board (+ 0 row-start) (+ 0 col-start))) #f]
          [(equal? num (get-value board (+ 1 row-start) (+ 0 col-start))) #f]
          [(equal? num (get-value board (+ 2 row-start) (+ 0 col-start))) #f]
          [(equal? num (get-value board (+ 0 row-start) (+ 1 col-start))) #f]
          [(equal? num (get-value board (+ 1 row-start) (+ 1 col-start))) #f]
          [(equal? num (get-value board (+ 2 row-start) (+ 1 col-start))) #f]
          [(equal? num (get-value board (+ 0 row-start) (+ 2 col-start))) #f]
          [(equal? num (get-value board (+ 1 row-start) (+ 2 col-start))) #f]
          [(equal? num (get-value board (+ 2 row-start) (+ 2 col-start))) #f]
          [#t])))


; This function should call your recursive backtracking solver
; and, depending upon the result, either return the solved puzzle
; or return null if the puzzle has no solution.
;
; It is your job to write this and necessary helper functions.
;
; DO NOT change the name/signature of this function, as our
; testing script depends upon it.
;
; Solves a sudoku puzzle if possible
;
; @param board The board to solve
; @return The solved puzzle, if not possible, then null
(define (solve-sudoku board)
  (add-number board 0 0 1))


; Add your other functions here.
; Helper method to solve the puzzle
;
; @param board The board to solve
; @param row The row being examined
; @param col the column being examined
; @param num the number to be placed in the given row and column space
; @return The solved board, if not possible then null
(define (add-number board row col num)
    (cond
      [(equal? num (+ 1 BOARD-SIZE)) null]
      [(equal? col 9) (add-number board (+ row 1) 0 1)]
      [(equal? row 9) board]
      [(not (zero? (get-value board row col)))
       (add-number board row (+ col 1) num)]
      [#t (cond [(test-checkpoint-code board row col num)
                  (let* ([next-board (set-value board row col num)]
                         [test (add-number next-board row (+ col 1) 1)])
                        (cond [(null? test)(add-number board row col (+ num 1))]
                              [#t  test]))]
             [#t(add-number board row col (+ num 1))])]))