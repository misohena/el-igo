;;; igo-model.el --- Igo Game Model                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains data structures for representing and processing Go games.
;;
;; - Position & Move
;; - Color & Empty (Intersection State)
;; - Board (W, H, Intersections, Turn, Prisoners, Ko-Position)
;; - Board Changes (Used in undo move and setup node)
;; - Node (Game Tree)
;; - Game (Board, Game Tree, Undo Stack)
;;
;; https://en.wikipedia.org/wiki/Rules_of_Go

;;; Code:

(defgroup el-igo nil
  "Go Game Editor"
  :prefix "igo-"
  :group 'games)

;;
;; Position & Move
;;

;; Position
(defconst igo-npos -1) ;;same as nmove
(defun igo-npos-p (pos) (= pos igo-npos))
(defun igo-pos-on-board-p (move-or-pos &optional w h)
  (and
   (>= move-or-pos 0)
   (or (null w) (null h) (< move-or-pos (* w h)))))

(defun igo-xy-to-pos (x y w) (+ x (* y w)))
(defun igo-pos-to-x (pos w) (% pos w))
(defun igo-pos-to-y (pos w) (/ pos w))

;; Move (Placement(Position), Pass, Resign)
(defconst igo-nmove -1) ;;same as npos
(defconst igo-pass -2)
(defconst igo-resign -3)
(defun igo-nmove-p (move) (= move igo-nmove))
(defun igo-pass-p (move) (= move igo-pass))
(defun igo-resign-p (move) (= move igo-resign))
(defun igo-placement-p (move) (igo-pos-on-board-p move))
(defun igo-move-string (move &optional board)
  (cond
   ((igo-pass-p move) "Pass")
   ((igo-resign-p move) "Resign")
   ((igo-placement-p move)
    (if board
        (format "%s %s"
                (1+ (igo-board-pos-to-x board move))
                (1+ (igo-board-pos-to-y board move)))
      (format "%s" move)))
   (t "Not Move")))

;;
;; Color & Empty (Intersection State)
;;

(defun igo-empty-p (obj) (eq obj 'empty))
(defun igo-black-p (obj) (eq obj 'black))
(defun igo-white-p (obj) (eq obj 'white))

(defun igo-intersection-state-p (obj)
  (or (eq obj 'empty) (eq obj 'black) (eq obj 'white)))
(defun igo-not-intersection-state-p (obj)
  (not (igo-intersection-state-p obj)))
(defun igo-same-intersection-state-p (obj1 obj2)
  (eq obj1 obj2))
(defun igo-diff-intersection-state-p (obj1 obj2)
  (not (eq obj1 obj2)))

(defun igo-color-p (obj) (or (eq obj 'black) (eq obj 'white)))
(defun igo-same-color-p (obj1 obj2) (eq obj1 obj2))
(defun igo-diff-color-p (obj1 obj2) (not (eq obj1 obj2)))

(defun igo-opposite-color (color)
  (cond ((eq color 'black) 'white)
        ((eq color 'white) 'black)
        (t color)))


;;
;; Board
;;

(defvar igo-board-default-w 9)
(defvar igo-board-default-h 9)

(defun igo-board (w h &optional intersections turn prisoners ko-pos)
  (if (null w) (setq w igo-board-default-w)
    (igo-board-validate-w w))
  (if (null h) (setq h igo-board-default-h)
    (igo-board-validate-h h))
  (if (null intersections) (setq intersections (make-vector (* w h) 'empty))
    (igo-board-validate-intersections intersections w h))
  (if (null turn) (setq turn 'black)
    (igo-board-validate-turn turn))
  (if (null prisoners) (setq prisoners (cons 0 0))
    (igo-board-validate-prisoners prisoners))
  (if (null ko-pos) (setq ko-pos igo-npos)
    (igo-board-validate-ko-pos ko-pos))

  (vector w h intersections turn prisoners ko-pos))

(defconst igo-board--idx-w 0)
(defconst igo-board--idx-h 1)
(defconst igo-board--idx-intersections 2)
(defconst igo-board--idx-turn 3)
(defconst igo-board--idx-prisoners 4)
(defconst igo-board--idx-ko-pos 5)

(defun igo-board-clone (board)
  (let ((w (igo-board-w board))
        (h (igo-board-h board)))
    (igo-board
     w h
     (copy-sequence (igo-board-intersections board))
     (igo-board-turn board)
     (cons (igo-board-get-black-prisoners board)
           (igo-board-get-white-prisoners board))
     (igo-board-ko-pos board))))

(defun igo-board-validate-w (w)
  (if (not (and (integerp w) (> w 0) (<= w 52)))
      (error "Invalid argument igo-board w=%s" w)))
(defun igo-board-validate-h (h)
  (if (not (and (integerp h) (> h 0) (<= h 52)))
      (error "Invalid argument igo-board h=%s" h)))
(defun igo-board-validate-intersections (intersections w h)
  (if (not (and (vectorp intersections)
                (= (length intersections) (* w h))
                ;; all elements are valid
                (null (seq-some #'igo-not-intersection-state-p intersections))))
      (error "Invalid argument igo-board intersections=%s" intersections)))
(defun igo-board-validate-turn (turn)
  (if (not (igo-color-p turn))
      (error "Invalid argument igo-board turn=%s" turn)))
(defun igo-board-validate-prisoners (prisoners)
  (if (not (and (consp prisoners)
                (integerp (car prisoners))
                (>= (car prisoners) 0)
                (integerp (cdr prisoners))
                (>= (cdr prisoners) 0)))
      (error "Invalid argument igo-board prisoners=%s" prisoners)))
(defun igo-board-validate-ko-pos (ko-pos)
  (if (not (and (integerp ko-pos)
                (or (igo-pos-on-board-p ko-pos) (igo-npos-p ko-pos))))
      (error "Invalid argument igo-board ko-pos=%s" ko-pos)))

;; Board - Basic Accessors

(defun igo-board-w (board) (aref board igo-board--idx-w))
(defun igo-board-h (board) (aref board igo-board--idx-h))
(defun igo-board-intersections (board) (aref board igo-board--idx-intersections))
(defun igo-board-turn (board) (aref board igo-board--idx-turn))
(defun igo-board-prisoners (board) (aref board igo-board--idx-prisoners))
(defun igo-board-ko-pos (board) (aref board igo-board--idx-ko-pos))

;; Board - Intersections

(defun igo-board-intersection-count (board) (* (igo-board-w board)
                                               (igo-board-h board)))
(defun igo-board-get-at (board pos)
  (aref (igo-board-intersections board) pos))
(defun igo-board-set-at (board pos istate)
  (aset (igo-board-intersections board) pos istate))
(defun igo-board-empty-p-at (board pos)
  (and (not (igo-npos-p pos)) (igo-empty-p (igo-board-get-at board pos))))
(defun igo-board-remove-stone (board pos)
  (igo-board-set-at board pos 'empty))

;; Board - Position(Index of Intersection)

(defun igo-board-xy-to-pos (board x y)
  (igo-xy-to-pos x y (igo-board-w board)))
(defun igo-board-pos-to-x (board pos)
  (igo-pos-to-x pos (igo-board-w board)))
(defun igo-board-pos-to-y (board pos)
  (igo-pos-to-y pos (igo-board-w board)))
(defun igo-board-pos-on-board-p (board pos)
  (and (integerp pos)
       (igo-pos-on-board-p pos (igo-board-w board) (igo-board-h board))))
(defun igo-board-left-of (board pos)
  (if (or (igo-npos-p pos) (= (% pos (igo-board-w board)) 0))
      igo-npos
    (1- pos)))
(defun igo-board-right-of (board pos)
  (if (or (igo-npos-p pos) (= (% (1+ pos) (igo-board-w board)) 0))
      igo-npos
    (1+ pos)))
(defun igo-board-above (board pos)
  (if (or (igo-npos-p pos) (< pos (igo-board-w board)))
      igo-npos
    (- pos (igo-board-w board))))
(defun igo-board-below (board pos)
  (let ((w (igo-board-w board))
        (h (igo-board-h board)))
    (if (or (igo-npos-p pos) (>= pos (* w (1- h))))
        igo-npos
      (+ pos w))))

;; Board - Turn

(defun igo-board-set-turn (board color)
  (aset board igo-board--idx-turn color))

;; Board - Prisoners

(defun igo-board-add-prisoners (board color num-prisoners)
  (let ((cell (igo-board-prisoners board)))
    (cond
     ((igo-black-p color) (setcar cell (+ (car cell) num-prisoners)))
     ((igo-white-p color) (setcdr cell (+ (cdr cell) num-prisoners))))))
(defun igo-board-remove-prisoners (board color num-prisoners)
  (igo-board-add-prisoners board color (- num-prisoners)))
(defun igo-board-get-prisoners (board color)
  (cond
   ((igo-black-p color) (car (igo-board-prisoners board)))
   ((igo-white-p color) (cdr (igo-board-prisoners board)))))
(defun igo-board-get-black-prisoners (board)
  (car (igo-board-prisoners board)))
(defun igo-board-get-white-prisoners (board)
  (cdr (igo-board-prisoners board)))

;; Board - Move

(defun igo-board-pass (board &optional color allow-illegal-move)
  (if (null color) (setq color (igo-board-turn board)))

  ;; Check COLOR's pass is legal. If the pass is illegal, return nil
  (when (or allow-illegal-move
            (igo-same-color-p color (igo-board-turn board)))
    ;; legal
    (let ((ko-pos-old (igo-board-ko-pos board))
          (turn-old (igo-board-turn board)))
      (igo-board-set-ko-pos board igo-npos)
      (igo-board-set-turn board (igo-opposite-color color)) ;;not opposite turn, if allow illegal moves
      ;; return undo data
      (igo-board-changes nil nil nil ko-pos-old turn-old nil nil))))

(defun igo-board-put-stone (board pos &optional color allow-illegal-move)
  (if (null color) (setq color (igo-board-turn board)))

  ;; Check the move(pos, color) is legal. If the move is illegal, return nil
  (when (or allow-illegal-move
            (igo-board-legal-move-p board pos color))

    ;; Illegal move behavior is based on the SGF specification:
    ;;   SGF FF[5] - Move vs. Adding Stones
    ;;   https://www.red-bean.com/sgf/ff5/m_vs_ax.htm

    (let* (;; Put COLOR's stone at POS
           (istate-old (igo-board-get-at board pos))
           (_istate-new (igo-board-set-at board pos color))

           ;; Remove captured stones
           (removed-stones
            (append
             (igo-board-remove-string-if-surrounded board (igo-board-left-of board pos) color)
             (igo-board-remove-string-if-surrounded board (igo-board-right-of board pos) color)
             (igo-board-remove-string-if-surrounded board (igo-board-above board pos) color)
             (igo-board-remove-string-if-surrounded board (igo-board-below board pos) color)))
           ;; Remove suicide stones
           (suicide-stones
            (if allow-illegal-move
                (igo-board-remove-string-if-surrounded board pos (igo-opposite-color color))))
           ;; Change ko-pos
           (ko-pos-old (igo-board-ko-pos board))
           (ko-pos-new (igo-board-get-new-ko-pos board pos color removed-stones))
           ;; Rotate turn
           (turn-old (igo-board-turn board))
           (turn-new (igo-opposite-color color)) ;;not opposite turn, if allow illegal moves
           )

      (igo-board-add-prisoners board color (length suicide-stones))
      (igo-board-add-prisoners board (igo-opposite-color color) (length removed-stones))
      (igo-board-set-ko-pos board ko-pos-new)
      (igo-board-set-turn board turn-new)
      ;; return undo data
      (igo-board-changes-make-move-undo color pos istate-old removed-stones suicide-stones ko-pos-old turn-old))))

(defun igo-board-legal-move-p (board pos color)
  (and
   board
   (igo-color-p color) ;; valid color
   (igo-board-pos-on-board-p board pos) ;; valid position
   (igo-same-color-p color (igo-board-turn board)) ;; color's turn
   (igo-board-empty-p-at board pos) ;; there is not already stone
   (not (igo-board-suicide-move-p board pos color)) ;; not suicide move
   (not (igo-board-ko-move-p board pos)) ;; pos is not ko
   ))

(defun igo-board-suicide-move-p (board pos color)
  ;; pre-conditions
  ;; - valid pos
  ;; - valid color
  ;; - pos is empty
  (if (or (igo-board-empty-p-at board (igo-board-left-of board pos))
          (igo-board-empty-p-at board (igo-board-right-of board pos))
          (igo-board-empty-p-at board (igo-board-above board pos))
          (igo-board-empty-p-at board (igo-board-below board pos)))
      nil
    ;; there are no empty point
    (let ((old-istate (igo-board-get-at board pos)))

      ;; put stone temporary
      (igo-board-set-at board pos color)

      (let ((suicide (and (igo-board-string-surrounded-p board pos)
                          (not (or (igo-board-string-surrounded-and-diff-color-p board (igo-board-left-of board pos) color)
                                   (igo-board-string-surrounded-and-diff-color-p board (igo-board-right-of board pos) color)
                                   (igo-board-string-surrounded-and-diff-color-p board (igo-board-above board pos) color)
                                   (igo-board-string-surrounded-and-diff-color-p board (igo-board-below board pos) color))))))

        ;; remove stone temporary puted
        (igo-board-set-at board pos old-istate)

        suicide))))

(defun igo-board-ko-move-p (board pos)
  (= pos (igo-board-ko-pos board)))

(defun igo-board-string-surrounded-p (board pos)
  "Return t if stones specified by pos are surrounded by any colors."
  (not (igo-board-find-liberty board pos)))

(defun igo-board-string-surrounded-and-diff-color-p (board pos color)
  (if (igo-board-pos-on-board-p board pos)
      (let ((pos-istate (igo-board-get-at board pos)))
        (and
         (not (igo-empty-p pos-istate)) ;; not empty
         (igo-diff-color-p pos-istate color) ;; different color
         (igo-board-string-surrounded-p board pos)))))

(defun igo-board-find-liberty (board pos)
  (if (igo-board-pos-on-board-p board pos)
      (let ((pos-istate (igo-board-get-at board pos)))
        (or (igo-empty-p pos-istate)
            (igo-board-find-liberty-recursive board pos pos-istate (make-vector (igo-board-intersection-count board) nil))))))

(defun igo-board-find-liberty-recursive (board pos color visited-arr)
  (if (or (igo-npos-p pos) ;; out of board
          (aref visited-arr pos)) ;; already visited
      nil
    (aset visited-arr pos t)
    (let ((istate (igo-board-get-at board pos)))
      (if (igo-empty-p istate)
          t
        (if (not (igo-same-color-p istate color))
            nil
          (or (igo-board-find-liberty-recursive board (igo-board-left-of board pos) color visited-arr)
              (igo-board-find-liberty-recursive board (igo-board-right-of board pos) color visited-arr)
              (igo-board-find-liberty-recursive board (igo-board-above board pos) color visited-arr)
              (igo-board-find-liberty-recursive board (igo-board-below board pos) color visited-arr)))))))

(defun igo-board-remove-string-if-surrounded (board pos turn)
  (if (igo-npos-p pos)
      nil
    (let ((istate (igo-board-get-at board pos)))
      (if (or (igo-empty-p istate) ;; empty
              (igo-same-color-p istate turn) ;; this turn's color
              (not (igo-board-string-surrounded-p board pos))) ;;not surrounded
          nil
        (igo-board-remove-string board pos istate)))))

(defun igo-board-remove-string (board pos color)
  (if (or (igo-npos-p pos) ;; out of board
          (not (igo-same-color-p (igo-board-get-at board pos) color))) ;; color changed
      nil
    (igo-board-remove-stone board pos)
    (append
     (list pos)
     (igo-board-remove-string board (igo-board-left-of board pos) color)
     (igo-board-remove-string board (igo-board-right-of board pos) color)
     (igo-board-remove-string board (igo-board-above board pos) color)
     (igo-board-remove-string board (igo-board-below board pos) color))))

;; Board - ko-pos

(defun igo-board-set-ko-pos (board ko-pos)
  (aset board igo-board--idx-ko-pos ko-pos))

(defun igo-board-get-new-ko-pos (board pos color removed-stones)
  (if (or (igo-npos-p pos)
          (igo-empty-p color)
          (not (= (length removed-stones) 1))
          (not (igo-same-color-p (igo-board-get-at board pos) color)))
      igo-npos

    (let ((num-empty-or-same-color 0)
          (pos-empty-or-same-color igo-npos))
      (dolist (neighbor-pos (list (igo-board-left-of board pos)
                                  (igo-board-right-of board pos)
                                  (igo-board-above board pos)
                                  (igo-board-below board pos)))
        (if (not (igo-npos-p neighbor-pos))
            (let ((neighbor-istate (igo-board-get-at board neighbor-pos)))
              (when (or (igo-empty-p neighbor-istate) (igo-same-color-p neighbor-istate color))
                (setq num-empty-or-same-color (1+ num-empty-or-same-color))
                (setq pos-empty-or-same-color neighbor-pos)))))

      (if (= num-empty-or-same-color 1)
          pos-empty-or-same-color
        igo-npos))))


;;
;; Board Changes
;;

(defun igo-board-changes (black white empty ko-pos turn black-prisoners white-prisoners)
  (vector (list black white empty) ko-pos turn black-prisoners white-prisoners))

;; Board Changes - Basic Accessors

(defun igo-board-changes-pos-list-for-each-istate (changes) (aref changes 0))
(defun igo-board-changes-ko-pos (changes) (if changes (aref changes 1)))
(defun igo-board-changes-turn (changes) (if changes (aref changes 2)))
(defun igo-board-changes-black-prisoners (changes) (if changes (aref changes 3)))
(defun igo-board-changes-white-prisoners (changes) (if changes (aref changes 4)))

(defun igo-board-changes-turn-set (changes turn)
  (if changes
      (aset changes 2 turn)))

;; Board Changes - Predicate

(defun igo-board-changes-empty-p (changes)
  (and
   (null (igo-board-changes-black changes))
   (null (igo-board-changes-white changes))
   (null (igo-board-changes-empty changes))
   (null (igo-board-changes-ko-pos changes))
   (null (igo-board-changes-turn changes))
   (null (igo-board-changes-black-prisoners changes))
   (null (igo-board-changes-white-prisoners changes))))

;; Board Changes - Apply Changes

(defun igo-board-changes-apply (changes board)
  (when (and changes board)
    ;; Intersection States(Black Stones, White Stones, Empty Intersections)
    (igo-board-changes-apply-intersections
     (igo-board-changes-black changes) 'black board)
    (igo-board-changes-apply-intersections
     (igo-board-changes-white changes) 'white board)
    (igo-board-changes-apply-intersections
     (igo-board-changes-empty changes) 'empty board)

    (let ((ko-pos (igo-board-changes-ko-pos changes))
          (turn (igo-board-changes-turn changes))
          (black-prisoners (igo-board-changes-black-prisoners changes))
          (white-prisoners (igo-board-changes-white-prisoners changes)))

      (if (integerp ko-pos) (igo-board-set-ko-pos board ko-pos))
      (if (igo-color-p turn) (igo-board-set-turn board turn))
      (if (integerp black-prisoners) (igo-board-add-prisoners board 'black black-prisoners))
      (if (integerp white-prisoners) (igo-board-add-prisoners board 'white white-prisoners))
      )))

(defun igo-board-changes-apply-intersections (positions new-istate board)
  ;; ensure list
  (if (integerp positions)
      (setq positions (list positions)))

  (if (and board
           (igo-intersection-state-p new-istate)
           (listp positions))
      (dolist (pos positions)
        (if (and (integerp pos) (igo-board-pos-on-board-p board pos))
            (igo-board-set-at board pos new-istate)))))

;; Board Changes - List of Intersection Positions

(defun igo-board-changes-black (changes)
  (nth 0 (igo-board-changes-pos-list-for-each-istate changes)))
(defun igo-board-changes-white (changes)
  (nth 1 (igo-board-changes-pos-list-for-each-istate changes)))
(defun igo-board-changes-empty (changes)
  (nth 2 (igo-board-changes-pos-list-for-each-istate changes)))

(defun igo-board-changes-istate-index (istate)
  (cond
   ((igo-black-p istate) 0)
   ((igo-white-p istate) 1)
   ((igo-empty-p istate) 2)))

(defun igo-board-changes-istate-list ()
  '(black white empty))

(defun igo-board-changes-pos-list-cell (changes istate)
  (nthcdr (igo-board-changes-istate-index istate)
          (igo-board-changes-pos-list-for-each-istate changes)))

(defun igo-board-changes-pos-list (changes istate)
  (car (igo-board-changes-pos-list-cell changes istate)))

;; Board Changes - Intersection State

(defun igo--find-previous-of (pred elements)
  "Return the cell before the first element in ELEMENTS where (PRED element) is non-nil."
  (let (prev-cell)
    (while (and elements (not (funcall pred (car elements))))
      (setq prev-cell elements)
      (setq elements (cdr elements)))
    prev-cell))

(defun igo-board-changes-get-at (changes pos)
  "Return the changed state('black, 'white, 'empty) of the intersection specified by POS."
  (when changes
    (cond
     ((member pos (igo-board-changes-black changes)) 'black)
     ((member pos (igo-board-changes-white changes)) 'white)
     ((member pos (igo-board-changes-empty changes)) 'empty)
     (t nil))))

(defun igo-board-changes-delete-at (changes pos)
  (if changes
      ;; Remove POS in position list for each istate
      (let ((pos-list (igo-board-changes-pos-list-for-each-istate changes)))
        (while pos-list
          (setcar pos-list (delete pos (car pos-list)))
          (setq pos-list (cdr pos-list))))))

(defun igo-board-changes-set-at (changes pos istate)
  (when changes
    (igo-board-changes-delete-at changes pos)

    ;; Insert in ascending order by POS
    (let* ((cell (igo-board-changes-pos-list-cell changes istate))
           (insertion-point (igo--find-previous-of
                             (lambda (elem) (> elem pos))
                             (car cell))))
      (if insertion-point
          (setcdr insertion-point (cons pos (cdr insertion-point)))
        (setcar cell (cons pos (car cell)))))))

;; TEST: igo-board-changes-set-at
;; (let ((c (igo-board-changes '(1 3 5) '(2 4 6) '(8 10 12) nil nil nil nil)))
;;   (igo-board-changes-set-at c 0 'white)
;;   c)
;; => [((1 3 5) (0 2 4 6) (8 10 12)) nil nil nil nil]



;; BoardChange - Creation

(defun igo-board-changes-diff-board (to-board from-board)
  "Return a igo-board-changes that change from FROM-BOARD to TO-BOARD."
  (let ((pos-lists (igo-board-changes--diff-intersections to-board from-board)))
    (igo-board-changes
     (cdr (assq 'black pos-lists))
     (cdr (assq 'white pos-lists))
     (cdr (assq 'empty pos-lists))
     (igo-board-changes--diff-value
      to-board from-board 'igo-board-ko-pos '=)
     (igo-board-changes--diff-value
      to-board from-board 'igo-board-turn 'igo-same-color-p)
     (igo-board-changes--diff-integer-delta
      to-board from-board 'igo-board-get-black-prisoners)
     (igo-board-changes--diff-integer-delta
      to-board from-board 'igo-board-get-white-prisoners))))

(defun igo-board-changes--diff-intersections (to-board from-board)
  (if (or (/= (igo-board-w to-board) (igo-board-w from-board))
          (/= (igo-board-h to-board) (igo-board-h from-board)))
      (error "Board sizes not match"))
  (let ((pos-lists (list (cons 'black nil)
                         (cons 'white nil)
                         (cons 'empty nil)))
        (num-intersections (igo-board-intersection-count to-board)))
    (dotimes (pos num-intersections)
      (let ((to-istate (igo-board-get-at to-board pos))
            (from-istate (igo-board-get-at from-board pos)))
        (if (not (igo-same-intersection-state-p to-istate from-istate))
            (let ((pos-list (assq to-istate pos-lists)))
              (push pos (cdr pos-list))))))
    (dolist (cell pos-lists)
      (setcdr cell (nreverse (cdr cell))))
    pos-lists))

(defun igo-board-changes--diff-value (to-board from-board getter fun-equal)
  (let ((to-value (funcall getter to-board))
        (from-value (funcall getter from-board)))
    (if (not (funcall fun-equal to-value from-value)) to-value)))

(defun igo-board-changes--diff-integer-delta (to-board from-board getter)
  (let ((to-value (funcall getter to-board))
        (from-value (funcall getter from-board)))
    (if (/= to-value from-value)
        (- to-value from-value))))

(defun igo-board-changes-make-initial-board (board)
  "Return a igo-board-changes that create BOARD from empty board."
  (igo-board-changes-diff-board
   board
   (igo-board (igo-board-w board) (igo-board-h board))))

(defun igo-board-changes-make-move-undo
    (color pos istate-old removed-stones suicide-stones ko-pos-old turn-old)
  "Return a igo-board-changes that revert move."

  (let* (;; Calculate number of prisoners increased
         (num-removed-stones (length removed-stones))
         (num-suicide-stones (length suicide-stones))
         (num-black-prisoners
          (if (igo-black-p color) num-suicide-stones num-removed-stones))
         (num-white-prisoners
          (if (igo-white-p color) num-suicide-stones num-removed-stones))
         ;; Guess current istate at POS
         (istate-new (if (member pos suicide-stones) 'empty color))
         ;; Remove POS from SUICIDE-STONES
         (suicide-stones-without-pos (remove pos suicide-stones))
         ;; Make pos-list
         (black-pos-list
          (if (igo-black-p color) suicide-stones-without-pos removed-stones))
         (white-pos-list
          (if (igo-white-p color) suicide-stones-without-pos removed-stones))
         (empty-pos-list nil))

    ;; Restore istate of POS
    (if (not (igo-same-intersection-state-p istate-new istate-old))
        (cond
         ((igo-black-p istate-old) (push pos black-pos-list))
         ((igo-white-p istate-old) (push pos white-pos-list))
         ((igo-empty-p istate-old) (push pos empty-pos-list))))

    ;;@todo sort pos-list?

    (igo-board-changes
     black-pos-list
     white-pos-list
     empty-pos-list
     ko-pos-old
     turn-old ;; Not necessarily the same as COLOR if allow illegal moves
     (if (> num-black-prisoners 0) (- num-black-prisoners))
     (if (> num-white-prisoners 0) (- num-white-prisoners)))))

;; TEST
;; (igo-board-changes-make-changes-undo 
;;  (igo-board-changes '(1 2 3) nil nil nil 'white 2 0)
;;  (let ((board (igo-board 9 9))) (igo-board-put-stone board 2 'black) (igo-board-put-stone board 3 'white) board))
;; => [(nil (3) (1)) nil black -2 nil]

(defun igo-board-changes-make-changes-undo (changes board)
  "Return a igo-board-change that revert CHANGES."
  (let ((to-board (igo-board-clone board)))
    (igo-board-changes-apply changes to-board)
    (igo-board-changes-diff-board board to-board)))


;;
;; Node
;;

(defun igo-node (prev &optional move color)
  (vector
   prev ;;0:prev
   (or move igo-nmove) ;;1:move
   color ;;2:color
   nil ;;3:properties
   nil ;;4:next nodes
   nil ;;5:last-visited
   ))

(defconst igo-node--idx-prev 0)
(defconst igo-node--idx-move 1)
(defconst igo-node--idx-color 2)
(defconst igo-node--idx-properties 3)
(defconst igo-node--idx-next-nodes 4)
(defconst igo-node--idx-last-visited 5)

(defun igo-node-prev (node) (aref node igo-node--idx-prev))
(defun igo-node-move (node) (aref node igo-node--idx-move))
(defun igo-node-color (node) (aref node igo-node--idx-color))
(defun igo-node-properties (node) (aref node igo-node--idx-properties))
(defun igo-node-next-nodes (node) (aref node igo-node--idx-next-nodes))
(defun igo-node-last-visited (node) (aref node igo-node--idx-last-visited))

(defun igo-node-clone (node &optional new-prev)
  (let ((new-node (igo-node new-prev
                            (igo-node-move node)
                            (igo-node-color node))))
    ;; clone properties
    (aset new-node igo-node--idx-properties
          (igo-node-clone-properties (igo-node-properties node)))
    ;; clone next-nodes
    (aset new-node igo-node--idx-next-nodes
          (igo-node-clone-next-nodes (igo-node-next-nodes node) new-node))
    ;; clone last-visited
    (if-let ((index (seq-position (igo-node-next-nodes node)
                                  (igo-node-last-visited node) #'eq)))
        (aset new-node igo-node--idx-last-visited
              (nth index (igo-node-next-nodes new-node))))
    new-node))

(defun igo-node-clone-properties (props)
  ;; Copy properties
  ;; - ATOM (symbol, string, number)
  ;; - list
  ;; - vector
  ;; - igo-board-changes (vector)
  (copy-tree props))

(defun igo-node-clone-next-nodes (next-nodes new-prev)
  (mapcar (lambda (next)
            (igo-node-clone next new-prev))
          next-nodes))

;; Node - Node Type(Setup or Move(Resign, Pass, Placement))

(defun igo-node-set-move-and-color (node move color)
  (aset node igo-node--idx-move move)
  (aset node igo-node--idx-color color))
(defun igo-node-setup-p (node) (igo-nmove-p (igo-node-move node)))
(defun igo-node-move-p (node) (not (igo-nmove-p (igo-node-move node))))
(defun igo-node-pass-p (node) (igo-pass-p (igo-node-move node)))
(defun igo-node-resign-p (node) (igo-resign-p (igo-node-move node)))
(defun igo-node-placement-p (node) (igo-placement-p (igo-node-move node)))

(defun igo-node-second-consecutive-pass-p (node)
  (and
   (igo-node-pass-p node)
   (let ((prev-node (igo-node-previous-move node)))
     (and prev-node
          (not (igo-same-color-p (igo-node-color node)
                                 (igo-node-color prev-node)))
          (igo-node-pass-p prev-node)))))

;; Node - Make Root

(defun igo-node-make-root (node board)
  (when (not (igo-node-root-p node))
    (let ((old-root-node (igo-node-get-root node)))
      ;; Copy game info properties from old-root
      ;; (if igo-sgf-parser loaded)
      (if (boundp 'igo-sgf-game-info-properties)
          (dolist (prop igo-sgf-game-info-properties)
            (let* ((prop-id (igo-sgf-game-info-prop-id prop))
                   (value (igo-node-get-sgf-property old-root-node prop-id)))
              (if value
                  (igo-node-set-sgf-property node prop-id value)))))
      ;; Set move number property
      (if (null (igo-node-get-move-number-property node))
          (igo-node-set-move-number-property
           node
           (igo-node-move-number node)))
      ;; Unlink previous node
      (aset node igo-node--idx-prev nil)
      ;; Clear move (indicate this node is a setup node)
      (igo-node-set-move-and-color node igo-nmove nil)
      ;; Set setup property to reproduce BOARD (discard old setup if it exists)
      (igo-node-set-setup-property
       node
       (igo-board-changes-make-initial-board board))
      ))
  node)

;; Node - Previous Node(Parent)

(defun igo-node-root-p (node)
  (null (igo-node-prev node)))

(defun igo-node-get-root (node)
  (let (prev)
    (while (setq prev (igo-node-prev node))
      (setq node prev))
    node))

(defun igo-node-previous-move (node)
  "Return previous move of NODE. Skip setup nodes."
  (setq node (igo-node-prev node))
  (while (and node (not (igo-node-move-p node)))
    (setq node (igo-node-prev node)))
  node)

(defun igo-node-number-of-siblings (node)
  "Return number of sibling nodes (including NODE itself)."
  (let ((prev (igo-node-prev node)))
    (if prev (length (igo-node-next-nodes prev))
      1)))

(defun igo-node-first-sibling-p (node)
  (let ((prev (igo-node-prev node)))
    (if prev (eq (car (igo-node-next-nodes prev)) node))))

(defun igo-node-last-sibling-p (node)
  (let ((prev (igo-node-prev node)))
    (if prev (eq (car (last (igo-node-next-nodes prev))) node))))

(defun igo-node-move-number (node)
  "Return the depth of NODE from the root node or previous MN property, not include setup node."
  (let ((num 0) mn-prop)
    (while (and node (null (setq mn-prop (igo-node-get-move-number-property node))))
      (if (igo-node-move-p node)
          (setq num (1+ num)))
      (setq node (igo-node-prev node)))
    (+ (or mn-prop 0) num)))

(defun igo-node-depth (node)
  "Return the depth of NODE from the root node, including setup node."
  (let ((num 0))
    (while node
      (setq num (1+ num))
      (setq node (igo-node-prev node)))
    num))

(defun igo-node-next-color (node)
  (let (result)
    (while (and
            node
            (null
             (setq result
                   ;; opposite color or PL property
                   (if (igo-node-move-p node)
                       (igo-opposite-color (igo-node-color node))
                     (if-let ((changes (igo-node-get-setup-property node)))
                         (igo-board-changes-turn changes))))))
      (setq node (igo-node-prev node)))
    (or result 'black)))

(defun igo-node-path-from-root (node &optional fork-only)
  (let (dirs prev)
    (while (and node (setq prev (igo-node-prev node)))
      (if (or (not fork-only) (>= (length (igo-node-next-nodes prev)) 2))
          (push (seq-position (igo-node-next-nodes prev) node) dirs))
      (setq node prev))
    dirs))

(defun igo-node-find-move-back (node move)
  (while (and node (not (= (igo-node-move node) move)))
    (setq node (igo-node-prev node)))
  node)

(defun igo-node-move-number-at (node pos)
  (igo-node-move-number (igo-node-find-move-back node pos)))

(defun igo-node-prev-nth (node n &optional clamp-p)
  (let (next)
    (while (and (> n 0) node)
      (setq next node)
      (setq node (igo-node-prev node))
      (setq n (1- n)))
    (if (and clamp-p (null node)) next node)))

(defun igo-node-ancestor-p (node ancestor)
  (igo-node-descendant-p ancestor node))

;; Node - Next Nodes(Children)

(defun igo-node-set-last-visited (node next-node)
  ;; check next-node is in next-nodes
  (if (or (null next-node)
          (seq-find (lambda (n) (eq n next-node)) (igo-node-next-nodes node)))
      (aset node igo-node--idx-last-visited next-node)))

(defun igo-node-next-node-default (node)
  (if node
      (or
       ;; last visited node
       (igo-node-last-visited node)
       ;; no branch
       (let ((next-nodes (igo-node-next-nodes node)))
         (if (= (length next-nodes) 1)
             (car next-nodes))))))

(defun igo-node-find-next-by-move (node move color)
  "Return the node that matches MOVE and COLOR from the next nodes."
  (seq-find (lambda (next) (and (= (igo-node-move next) move)
                                (or (null color)
                                    (igo-same-color-p (igo-node-color next)
                                                      color))))
            (igo-node-next-nodes node)))

(defun igo-node-get-next-setup-node (node index)
  (let ((next-nodes (igo-node-next-nodes node))
        result)
    (while (and next-nodes (null result))
      (if (igo-node-setup-p (car next-nodes))
          (if (= index 0)
              (setq result (car next-nodes))
            (setq index (1- index))))
      (setq next-nodes (cdr next-nodes)))
    result))

(defun igo-node-add-next-node (node next-node)
  (aset node igo-node--idx-next-nodes
        (nconc (igo-node-next-nodes node)
               (list next-node)))
  next-node)

(defun igo-node-create-next-node (node &optional move color)
  (if (null move) (setq move igo-nmove))

  ;; Create a new node, add to NODE, return new node.
  (igo-node-add-next-node node (igo-node node move color)))

(defun igo-node-delete-next (node target-node)
  ;; remove TARGET-NODE from next-nodes
  (aset node igo-node--idx-next-nodes
        (delq target-node (igo-node-next-nodes node)))
  ;; ensure last-visited does not point to NODE
  (if (eq target-node (igo-node-last-visited node))
      (igo-node-set-last-visited node (car (igo-node-next-nodes node)))))

(defun igo-node-find-by-path (node dirs &optional fork-only)
  (while (and node dirs)
    (let* ((next-nodes (igo-node-next-nodes node))
           (len-next-nodes (length next-nodes))
           (dir (if (or (not fork-only) (>= len-next-nodes 2))
                   (pop dirs)
                 0)))
      (if (and (>= dir 0) (< dir len-next-nodes))
          (setq node (nth dir next-nodes))
        (setq dirs nil)
        (setq node nil))))
  node)

(defun igo-node-find-by-queries (node queries board-w board-h)
  (while queries
    (let ((q (car queries)))
      (cond
       ;; integer : nth node(choose first variation)
       ((integerp q)
        (setq node (igo-node-nth node q t)))
       ((stringp q)
        (cond
         ;; "xx" : find point breadth-first
         ((string-match-p "^[a-zA-Z][a-zA-Z]$" q)
          (let* ((pos (igo-sgf-value-as-move (igo-sgf-make-prop-value "" 0 q 0) board-w board-h))
                 (target (igo-node-find-breadth-first node (lambda (node) (= (igo-node-move node) pos)))))
            (if target
                (setq node target))))
         ;; "X" : find next fork
         ((string-match-p "^[A-Z]$" q)
          (let ((index (- (elt q 0) ?A))
                (fork (igo-node-find-first-fork-or-last node)))
            (if (and fork (< index (length (igo-node-next-nodes fork))))
                (setq node (nth index (igo-node-next-nodes fork))))))
         ;; "_" : first fork
         ((string= "_" q)
          (setq node (igo-node-find-first-fork-or-last node)))
         ;; "123" : nth node(choose first variation)
         ((string-match-p "^-?[0-9]+$" q)
          (setq node (igo-node-nth node (string-to-number q) t))))
        )))
    (setq queries (cdr queries)))
  node)

(defun igo-node-nth (node n &optional clamp-p)
  (if (>= n 0)
      (igo-node-next-nth node n clamp-p)
    (igo-node-prev-nth node (- n) clamp-p)))

(defun igo-node-next-nth (node n &optional clamp-p)
  (let (prev)
    (while (and (> n 0) node)
      (setq prev node)
      (setq node (car (igo-node-next-nodes node)))
      (setq n (1- n)))
    (if (and clamp-p (null node)) prev node)))

(defun igo-node-find-first-fork-or-last (node)
  (while (= (length (igo-node-next-nodes node)) 1)
    (setq node (car (igo-node-next-nodes node))))
  ;; (length (igo-node-next-nodes node)) is 0 or 2 or more
  node)

(defun igo-node-find-breadth-first (node pred)
  (let ((curr (list node))
        next
        result)
    (while curr
      (while (and curr (not (funcall pred (car curr))))
        (setq next (append next (igo-node-next-nodes (car curr))))
        (setq curr (cdr curr)))

      (if curr
          (progn
            (setq result (car curr))
            (setq curr nil))
        (setq curr next)
        (setq next nil)))
    result))

(defun igo-node-change-next-node-order(node index delta)
  (let* ((next-nodes (igo-node-next-nodes node))
         (size (length next-nodes))
         target-node)

    (cond
     ((vectorp index)
      (setq target-node index)
      (setq index (seq-position next-nodes index #'eq)))

     ((integerp index)
      (if (and (>= index 0) (< index size))
          (setq target-node (nth index next-nodes))
        ;; index out of range
        (setq index nil))))

    (when (integerp index)
      (let ((new-index (max 0 (min (1- size) (+ index delta)))))
        (when (/= new-index index)
          ;; Remove
          (if (= index 0)
              (setq next-nodes (cdr next-nodes))
            (let ((cell (nthcdr (1- index) next-nodes)))
              (setcdr cell (cddr cell))))
          ;; Insert
          (if (= new-index 0)
              (setq next-nodes (cons target-node next-nodes))
            (let ((cell (nthcdr (1- new-index) next-nodes)))
              (setcdr cell (cons target-node (cdr cell)))))
          (aset node igo-node--idx-next-nodes next-nodes)
          t)))))

(defun igo-node-descendant-p (node descendant)
  (cond
   ((null node) nil)
   ((null descendant) nil)
   ((eq node descendant) nil)
   (t
    (setq descendant (igo-node-prev descendant))
    (while (and descendant (not (eq descendant node)))
      (setq descendant (igo-node-prev descendant)))
    (not (null descendant)))))

;; Node - Properties

(defun igo-node-set-property (node symbol value)
  (if (stringp symbol)
      (setq symbol (intern symbol)))
  (aset node igo-node--idx-properties
        (plist-put (igo-node-properties node) symbol value)))

(defun igo-node-get-property (node symbol &optional default)
  (if (stringp symbol)
      (setq symbol (intern symbol)))
  (or (plist-get (igo-node-properties node) symbol) default))

;; Node - Properties - Move Number

(defun igo-node-get-move-number-property (node)
  (let ((str (car (igo-node-get-sgf-property node "MN"))))
    (if (stringp str)
        (string-to-number str))))

(defun igo-node-set-move-number-property (node number)
  (igo-node-set-sgf-property
   node "MN" (number-to-string number)))

(defun igo-node-delete-move-number-property (node)
  (igo-node-delete-sgf-property node "MN"))


;; Node - Properties - Comment

(defun igo-node-get-comment (node)
  (if node
      (let ((value (car (igo-node-get-sgf-property node "C"))))
        (if value
            (igo-sgf-value-as-text value)))))

(defun igo-node-set-comment (node text)
  (if (and node (stringp text))
      (igo-node-set-sgf-property node "C" (list (igo-sgf-text text))))
  text)

(defun igo-node-delete-comment (node)
  (igo-node-delete-sgf-property node "C"))

;; Node - Properties - Setup

(defun igo-node-set-setup-property (node board-changes)
  (igo-node-set-property node :setup board-changes))

(defun igo-node-get-setup-property (node)
  (igo-node-get-property node :setup))

;; Node - Properties - Marks

(defun igo-mark (type pos &optional text)
  (if text
      (list type pos text)
    (list type pos)))
(defun igo-mark-type (mark) (nth 0 mark))
(defun igo-mark-pos (mark) (nth 1 mark))
(defun igo-mark-text (mark) (nth 2 mark))

(defun igo-node-set-marks-property (node marks)
  (igo-node-set-property node :marks marks))

(defun igo-node-get-marks-property (node)
  (igo-node-get-property node :marks))

(defun igo-node-delete-mark-at (node pos)
  (let* ((mark-list (igo-node-get-marks-property node))
         (prev (igo--find-previous-of
                (lambda (mark) (= (igo-mark-pos mark) pos))
                mark-list))
         (cell (if prev (cdr prev) mark-list)))
    (when cell
      ;; remove cell from list
      (if prev
          (setcdr prev (cddr prev))
        (igo-node-set-marks-property node (cdr mark-list)))
      ;; deleted
      t)))

(defun igo-node-set-mark-at (node pos type &optional text)
  (let* ((mark-list (igo-node-get-marks-property node))
         (prev (igo--find-previous-of
                (lambda (mark) (= (igo-mark-pos mark) pos))
                mark-list))
         (cell (if prev (cdr prev) mark-list)))

    (if (and cell
             (eq (igo-mark-type (car cell)) type)
             (equal (igo-mark-text (car cell)) text))
        ;; already exists
        nil
      ;; remove cell from list
      (if cell
          (if prev
              (setcdr prev (cddr prev))
            (setq mark-list (cdr mark-list))))
      ;; add new mark
      (push (igo-mark type pos text) mark-list)
      (igo-node-set-marks-property node mark-list)
      ;; changed
      t)))

;; Node - Properties - SGF Location
;; 1)
;; tree-begin-opt=>                    <=tree-end-opt
;;                 (;B[aa];W[bb];B[cc])
;;      node-begin=>      <=node-end
;; 2)
;;            node-begin=>      <=node-end (tree-begin-opt = tree-end-opt = nil)
(defun igo-node-set-sgf-location (node tree-begin-opt node-begin node-end tree-end-opt)
  (igo-node-set-property
   node
   :sgf-location
   (if (and (null tree-begin-opt) (null tree-end-opt))
       (list node-begin node-end)
     (list node-begin node-end tree-begin-opt tree-end-opt))))
(defun igo-node-get-sgf-location (node)
  (igo-node-get-property node :sgf-location))
(defun igo-node-sgf-begin (node-loc) (nth 0 node-loc))
(defun igo-node-sgf-end (node-loc) (nth 1 node-loc))
(defun igo-node-sgf-tree-begin (node-loc) (nth 2 node-loc))
(defun igo-node-sgf-tree-end (node-loc) (nth 3 node-loc))

;; Node - Properties - SGF Properties

(defun igo-node-get-sgf-properties (node)
  (igo-node-get-property node :sgf-properties))

(defun igo-node-set-sgf-property (node prop-id-str prop-values)
  (if (stringp prop-values) (setq prop-values (list prop-values)))
  (igo-node-set-property
   node
   :sgf-properties
   (lax-plist-put (igo-node-get-sgf-properties node) prop-id-str prop-values)))

(defun igo-node-get-sgf-property (node prop-id-str)
  (lax-plist-get (igo-node-get-sgf-properties node) prop-id-str))

(defun igo-node-delete-sgf-property (node prop-id-str)
  (let ((props (igo-node-get-sgf-properties node))
        prev-cell)
    (while (and props
                (not (string= (car props) prop-id-str)))
      (setq prev-cell (cdr props))
      (setq props (cddr props)))

    (if props
        (if prev-cell
            (setcdr prev-cell (cddr props))
          (igo-node-set-property node :sgf-properties (cddr props))))))


;;
;; Game
;;

(defun igo-game (&optional w h game-tree)
  (let* ((root-node (or game-tree
                        (igo-node nil)))
         (board (igo-board w h)))

    ;; Apply root setup to board
    (if game-tree
        (igo-board-changes-apply (igo-node-get-setup-property game-tree) board))

    (vector
     nil ;;finished
     board ;;board
     nil ;;undo stack
     root-node ;;game tree
     root-node ;;current node
)))

(defconst igo-game--idx-finished 0)
(defconst igo-game--idx-board 1)
(defconst igo-game--idx-undo 2)
(defconst igo-game--idx-root-node 3)
(defconst igo-game--idx-current-node 4)

;; Game - Finished

(defun igo-game-finished-p (game)
  (aref game igo-game--idx-finished))
(defun igo-game-set-finished (game)
  (aset game igo-game--idx-finished t))
(defun igo-game-cancel-finish (game)
  (aset game igo-game--idx-finished nil))

;; Game - Board

(defun igo-game-board (game)
  (aref game igo-game--idx-board))
(defun igo-game-turn (game)
  (igo-board-turn (igo-game-board game)))
(defun igo-game-set-turn (game color)
  (igo-board-set-turn (igo-game-board game) color))
(defun igo-game-get-prisoners (game color)
  (igo-board-get-prisoners (igo-game-board game) color))

;; Game - Move

(defun igo-game-resign (game color &optional allow-illegal-move)
  (if (null color) (setq color (igo-game-turn game)))
  (when (and game
             (not (igo-game-finished-p game))
             (or allow-illegal-move
                 (igo-same-color-p color (igo-game-turn game))))
    ;;legal
    (igo-game-set-finished game)
    ;; push undo stack & game tree node
    (igo-game-push-undo game nil)
    (igo-game-push-node game igo-resign color)
    t))

(defun igo-game-pass (game color &optional allow-illegal-move)
  (when (and game
             (not (igo-game-finished-p game)))
    (let ((undo (igo-board-pass (igo-game-board game) color allow-illegal-move)))
      (when undo
        (igo-game-push-undo game undo)
        (igo-game-push-node game igo-pass color)
        (if (igo-node-second-consecutive-pass-p
             (igo-game-current-node game))
            (igo-game-set-finished game))
        t))))

(defun igo-game-put-stone (game pos color &optional allow-illegal-move)
  (when (and game
             (not (igo-game-finished-p game)))
    ;; not finished
    (let ((undo (igo-board-put-stone (igo-game-board game) pos color allow-illegal-move)))
      (when undo
        ;;legal move
        (igo-game-push-undo game undo)
        (igo-game-push-node game pos color)
        t))))

(defun igo-game-legal-move-p (game pos color)
  (and game
       (not (igo-game-finished-p game))
       (igo-board-legal-move-p (igo-game-board game) pos color)))

;; Game - Setup Node

(defun igo-game-add-setup-node (game)
  "Add a setup node to the current node. And change the current node to it."
  (when game
    (igo-game-push-undo game (igo-board-changes nil nil nil nil nil nil nil))
    (igo-game-push-node game igo-nmove nil)
    t))

;; Game - Undo

(defun igo-game-clear-undo (game)
  (aset game igo-game--idx-undo nil))

(defun igo-game-push-undo (game undo-change)
  (aset game igo-game--idx-undo
        (cons undo-change (aref game igo-game--idx-undo))))

(defun igo-game-pop-undo (game)
  (let ((top (aref game igo-game--idx-undo)))
    (aset game igo-game--idx-undo (cdr top))
    (car top)))

(defun igo-game-empty-undo-p (game)
  (null (aref game igo-game--idx-undo)))

(defun igo-game-get-undo-board-changes (game)
  (car (aref game igo-game--idx-undo)))

(defun igo-game-undo (game)
  ;; Move current-node to previous node
  (if (not (igo-node-root-p (igo-game-current-node game)))
      (let ((popped-curr-node (igo-game-pop-node game)))
        ;; Undo finish flag
        (if (or
             (igo-node-resign-p popped-curr-node)
             (igo-node-second-consecutive-pass-p popped-curr-node))
            (igo-game-cancel-finish game))))

  ;; Pop undo board change
  (if (not (igo-game-empty-undo-p game))
      (let ((undo (igo-game-pop-undo game)))
        ;; Undo board
        (if undo
            (igo-board-changes-apply undo (igo-game-board game)))
        )))

(defun igo-game-undo-all (game)
  (while (or (not (igo-node-root-p (igo-game-current-node game)))
             (not (igo-game-empty-undo-p game)))
    (igo-game-undo game)))

(defun igo-game-undo-to (game ancester)
  (while (and (or (not (igo-node-root-p (igo-game-current-node game)))
                  (not (igo-game-empty-undo-p game)))
              (not (eq (igo-game-current-node game) ancester)))
    (igo-game-undo game)))

;; Game - Game Tree

(defun igo-game-root-node (game)
  (aref game igo-game--idx-root-node))

(defun igo-game-current-node (game)
  (aref game igo-game--idx-current-node))

(defun igo-game-push-node (game move color)
  (let* ((curr-node (igo-game-current-node game))
         (next-node (or
                     ;; If MOVE is nmove(setup node), always add a new node
                     (if (igo-nmove-p move) (igo-node-create-next-node curr-node igo-nmove nil))
                     ;; If already added MOVE, use it
                     (igo-node-find-next-by-move curr-node move color)
                     ;; add a new node
                     (igo-node-create-next-node curr-node move color))))
    (igo-game-select-next-node game next-node)))

(defun igo-game-select-next-node (game next-node)
  ;; @todo check current-node contains next-node
  (igo-node-set-last-visited (igo-game-current-node game) next-node)
  (aset game igo-game--idx-current-node next-node))

(defun igo-game-pop-node (game)
  "Set current node to previsous node. Return current node before function was called."
  (let ((curr-node (igo-game-current-node game)))
    (if (not (igo-node-root-p curr-node))
        (progn
          (aset game igo-game--idx-current-node (igo-node-prev curr-node))
          curr-node)
      nil)))

(defun igo-game-make-current-node-root (game)
  (let ((curr-node (igo-game-current-node game)))
    ;; make current node root
    (igo-node-make-root curr-node (igo-game-board game))
    (aset game igo-game--idx-root-node curr-node)
    ;; clear undo stack
    (igo-game-clear-undo game)))

;; Game - Redo

(defun igo-game-redo-all (game)
  (while (igo-game-redo game)))

(defun igo-game-redo (game)
  (let ((current-node (igo-game-current-node game)))
    (igo-game-apply-node
     game
     (igo-node-next-node-default current-node))))

(defun igo-game-apply-node (game next-node)
  (cond
   ((null next-node) nil)
   ((igo-node-pass-p next-node) (igo-game-pass game (igo-node-color next-node) t)) ;;allow illegal move
   ((igo-node-resign-p next-node) (igo-game-resign game (igo-node-color next-node) t)) ;;allow illegal move
   ((igo-node-placement-p next-node) (igo-game-put-stone game (igo-node-move next-node) (igo-node-color next-node) t)) ;;allow illegal move
   ((igo-node-setup-p next-node) ;; setup node
    ;; @todo check current-node contains next-node
    (let* ((change (igo-node-get-setup-property next-node))
           ;; Create undo data
           (undo-change (if change
                            (igo-board-changes-make-changes-undo
                             change (igo-game-board game)))))
      ;; apply change
      (if change
          (igo-board-changes-apply change (igo-game-board game)))

      ;; push undo
      (igo-game-push-undo game undo-change)
      ;; select next-node to current
      ;; (next-node is already added to current-node)
      (igo-game-select-next-node game next-node)
      t
      ))))

(defun igo-game-redo-by-path (game dirs)
  (let (next-node)
    (while (and dirs (setq next-node (nth (car dirs) (igo-node-next-nodes (igo-game-current-node game)))))
      (igo-game-apply-node game next-node)
      (setq dirs (cdr dirs)))))

(defun igo-game-redo-by-queries (game queries)
  (let* ((board (igo-game-board game))
         (target (igo-node-find-by-queries
                  (igo-game-root-node game)
                  queries
                  (igo-board-w board)
                  (igo-board-h board))))
    (if target
        (igo-game-redo-by-path game (igo-node-path-from-root target)))))

(provide 'igo-model)
;;; igo-model.el ends here
