;;; igo-view.el --- Igo Game View                   -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'svg)
(require 'igo-model)

;;
;; Board View
;;

;; Board View - Sizes

(defcustom igo-board-view-size-max '(0.9 . 0.8)
  "Maximum pixel size of boards.

Specify an integer or a float or (width-int-or-float . height-int-or-float).

If the value is an integer, it directly specifies the maximum
board width and height.

If the value is an floating point number, it specifies the
maximum board width and height as a ratio to frame width and
height."
  :type '(choice integer
                 float
                 (cons (choice integer
                               float)
                       (choice integer
                               float)))
  :group 'el-igo)

(defcustom igo-board-view-grid-interval-default 32
  "Default interval of grid lines."
  :type '(integer)
  :group 'el-igo)
(defcustom igo-board-view-grid-interval-min 10
  "Minimum interval of grid lines."
  :type '(integer)
  :group 'el-igo)

(defun igo-board-view-size-max-spec (width-p)
  "Extract width or height from igo-svg-max-board-size."
  (let* ((spec igo-board-view-size-max)
         (max-size
          (cond
           ((consp spec) (if width-p (car spec) (cdr spec)))
           ((numberp spec))
           (t 0.8))))
    (if (floatp max-size)
        (ceiling (/ (* max-size (if width-p
                                    (frame-text-width)
                                  (frame-text-height)))
                    (image-compute-scaling-factor image-scaling-factor)))
      max-size)))

;; Board View - Constructor

(defun igo-board-view (board &optional interval)
  (let* ((w (igo-board-w board))
         (h (igo-board-h board))
         (max-w (igo-board-view-size-max-spec t))
         (max-h (igo-board-view-size-max-spec nil))
         (max-interval-w (/ max-w (+ (1- w) 3))) ;; 3=margin*2
         (max-interval-h (/ max-h (+ (1- h) 3))) ;; 3=margin*2
         (max-interval (min max-interval-w max-interval-h))
         (interval (max (min (or interval
                                 igo-board-view-grid-interval-default)
                             max-interval)
                        igo-board-view-grid-interval-min))
         (margin (/ (* 3 interval) 2)) ;; 3=margin*2
         (view (vector w h margin interval)))
    view))

;; Board View - Sizes

(defun igo-board-view-w (view) (aref view 0))
(defun igo-board-view-h (view) (aref view 1))
(defun igo-board-view-margin (view) (aref view 2))
(defun igo-board-view-interval (view) (aref view 3))

(defun igo-board-view-pixel-w (view)
  (+ (* 2 (igo-board-view-margin view))
     (* (1- (igo-board-view-w view)) (igo-board-view-interval view))))
(defun igo-board-view-pixel-h (view)
  (+ (* 2 (igo-board-view-margin view))
     (* (1- (igo-board-view-h view)) (igo-board-view-interval view))))

(defun igo-board-view-clickable-left (view &optional x)
  (+
   (- (igo-board-view-margin view) (/ (igo-board-view-interval view) 2))
   (or x 0)))
(defun igo-board-view-clickable-top (view &optional y)
  (+
   (- (igo-board-view-margin view) (/ (igo-board-view-interval view) 2))
   (or y 0)))
(defun igo-board-view-clickable-width (view)
  (* (igo-board-view-interval view) (igo-board-view-w view)))
(defun igo-board-view-clickable-height (view)
  (* (igo-board-view-interval view) (igo-board-view-h view)))

(defun igo-board-view-to-grid-x (view x &optional board-left)
  (/ (- x (igo-board-view-clickable-left view board-left))
     (igo-board-view-interval view)))

(defun igo-board-view-to-grid-y (view y &optional board-top)
  (/ (- y (igo-board-view-clickable-top view board-top))
     (igo-board-view-interval view)))


;; Board View - Update

(defun igo-board-view-create-board (view svg left top)
  (igo-svg-board
   svg left top
   (igo-board-view-w view)
   (igo-board-view-h view)
   (igo-board-view-interval view)
   (igo-board-view-margin view)))

(defun igo-board-view-update-stones (view svg board)
  (igo-svg-stones svg board (igo-board-view-interval view)))

(defun igo-board-view-update-move-numbers (view svg visible game)
  (if visible
      (igo-svg-move-numbers svg
                            (igo-game-board game)
                            (igo-board-view-interval view)
                            (igo-game-current-node game))
    (igo-svg-remove-move-numbers svg)))

(defun igo-board-view-update-branches (view svg visible board move-color node
                                            &optional fun-pass fun-resign fun-setup)
  (if visible
      (igo-svg-branches
       svg
       board
       move-color
       (igo-board-view-interval view)
       node
       fun-pass fun-resign fun-setup)
    (igo-svg-remove-branches svg)))

(defun igo-board-view-update-last-move-mark (view svg visible game)
  (if visible
      (igo-svg-last-move svg game (igo-board-view-interval view))
    (igo-svg-remove-last-move svg)))

(defun igo-board-view-update-marks (view svg visible game)
  (if visible
      (igo-svg-marks
       svg
       (igo-game-board game)
       (igo-board-view-interval view)
       (igo-node-get-marks-property (igo-game-current-node game)))
    (igo-svg-remove-marks svg)))




;;
;; SVG Board
;;

;; Board

(defun igo-svg-board (svg left top w h grid-interval grid-margin)
  (let* ((pixel-w (+ (* 2 grid-margin) (* grid-interval (1- w))))
         (pixel-h (+ (* 2 grid-margin) (* grid-interval (1- h))))
         (line-w 1)
         (star-radius 2.5)
         ;; Board Root
         (svg-board-root (svg-node svg 'g
                                   :class "board"
                                   :transform (format "translate(%s %s)"
                                                      left
                                                      top)))
         ;; Board Rect
         (_svg-board-rect (svg-rectangle svg-board-root 0 0 pixel-w pixel-h :fill "#e3aa4e"))
         ;; Game Area
         (svg-game-area (svg-node svg-board-root 'g
                                  :class "game-area"
                                  :transform (format "translate(%s %s)"
                                                     (- grid-margin 0.5)
                                                     (- grid-margin 0.5))))
         ;; Grid
         (svg-grid (svg-node svg-game-area 'g :class "grid")))
    ;; Lines
    (loop for x to (1- w) do
          (svg-line svg-grid
                    (* x grid-interval)
                    (/ line-w -2.0)
                    (* x grid-interval)
                    (+ (* (1- h) grid-interval) (/ line-w 2.0))
                    :stroke "black" :stroke-width line-w))
    (loop for y to (1- h) do
          (svg-line svg-grid
                    (/ line-w -2.0)
                    (* y grid-interval)
                    (+ (* (1- w) grid-interval) (/ line-w 2.0))
                    (* y grid-interval)
                    :stroke "black" :stroke-width line-w))
    ;; Stars
    (when (and (= (logand w 1) 1) (= (logand h 1) 1))
      (svg-circle svg-grid (* grid-interval (/ (1- w) 2.0)) (* grid-interval (/ (1- h) 2.0)) star-radius))
    (when (and (>= w 13) (>= h 13))
      (svg-circle svg-grid (* grid-interval 3)       (* grid-interval 3) star-radius)
      (svg-circle svg-grid (* grid-interval (- w 4)) (* grid-interval 3) star-radius)
      (svg-circle svg-grid (* grid-interval 3)       (* grid-interval (- h 4)) star-radius)
      (svg-circle svg-grid (* grid-interval (- w 4)) (* grid-interval (- h 4)) star-radius))
    (when (and (>= w 19) (>= h 19) (= (logand w 1) 1))
      (svg-circle svg-grid (* grid-interval (/ (1- w) 2.0)) (* grid-interval 3) star-radius)
      (svg-circle svg-grid (* grid-interval (/ (1- w) 2.0)) (* grid-interval (- h 4)) star-radius))
    (when (and (>= w 19) (>= h 19) (= (logand h 1) 1))
      (svg-circle svg-grid (* grid-interval 3)       (* grid-interval (/ (1- h) 2.0)) star-radius)
      (svg-circle svg-grid (* grid-interval (- w 4)) (* grid-interval (/ (1- h) 2.0)) star-radius))
    ;; Layers
    (svg-node svg-game-area 'g :class "shadows")
    (svg-node svg-game-area 'g :class "stones")
    (svg-node svg-game-area 'g :class "move-numbers")
    (svg-node svg-game-area 'g :class "overlays")
    ;; Stone's Gradient
    (igo-svg-stone-gradient svg-board-root) ;;@todo insert to svg root?
    ;; Return Board Group
    svg-board-root))

;; Stone

(defun igo-svg-shadows-group (svg) (car (dom-by-class svg "shadows")))
(defun igo-svg-stones-group (svg) (car (dom-by-class svg "stones")))
(defun igo-svg-move-numbers-group (svg) (car (dom-by-class svg "move-numbers")))
(defun igo-svg-overlays-group (svg) (car (dom-by-class svg "overlays")))

(defun igo-svg-stone-gradient (svg)
  (igo--svg-gradient
   svg "stone-black" 'radial '((0 . "#606060") (100 . "#000000"))
   :cx 0.5 :cy 0.5 :fx 0.7 :fy 0.3 :r 0.55)
  (igo--svg-gradient
   svg "stone-white" 'radial '((0 . "#ffffff") (80 . "#e0e0e0") (100 . "#b0b0b0"))
   :cx 0.5 :cy 0.5 :fx 0.7 :fy 0.3 :r 0.6)
  svg)

(defun igo-svg-stone (svg x y color grid-interval)
  (let* ((cx (* x grid-interval))
         (cy (* y grid-interval))
         (r (* grid-interval 0.98 0.5
               (/ (if (igo-white-p color) 21.9 22.2) 22.2)))) ;; diameter of white stone is 21.9mm
    ;; Shadow
    (svg-circle (igo-svg-shadows-group svg) (- cx 2) (- cy 2) r
                :id (igo-svg-shadow-id x y)
                :fill "rgba(0,0,0,0.3)")
    ;; Stone
    (svg-circle (igo-svg-stones-group svg) cx cy r
                :id (igo-svg-stone-id x y)
                :gradient (if (igo-white-p color) "stone-white" "stone-black"))))

(defun igo-svg-stone-id (x y) (format "stone-%s-%s" x y))
(defun igo-svg-shadow-id (x y) (format "shadow-%s-%s" x y))
(defun igo-svg-move-number-id (x y) (format "mvnum-%s-%s" x y))

(defun igo-svg-stones (svg board grid-interval)
  (loop for pos to (1- (igo-board-intersection-count board)) do
        (let ((x (igo-board-pos-to-x board pos))
              (y (igo-board-pos-to-y board pos)))
          (if (igo-board-empty-p-at board pos)
              (progn
                (svg-remove svg (igo-svg-stone-id x y))
                (svg-remove svg (igo-svg-shadow-id x y)))
            (igo-svg-stone svg x y (igo-board-get-at board pos) grid-interval)))))

;; Text

(defvar igo-svg-font-family "Arial")
(defvar igo-svg-font-weight "bold")
(defvar igo-svg-font-baseline-shift "-40%")
(defvar igo-svg-font-size-scale 0.52)

(defun igo-svg-font-size-on-board (grid-interval &optional scale)
  "Calculate font size."
  (ceiling (* grid-interval
              (or scale igo-svg-font-size-scale))))

(defun igo-svg-text-on-board (svg center-x center-y grid-interval text text-color &optional attributes font-size-scale)
  (let* ((font-size (igo-svg-font-size-on-board grid-interval font-size-scale))
         (text-w (string-width text)))
    ;; Shrink width
    (if (>= text-w 3)
        (setq attributes
              (nconc
               (list :transform
                     (format "translate(%s 0) scale(%.4f 1) translate(%s 0)"
                             (+ center-x)
                             (/ 1 (* 0.5 text-w))
                             (- center-x)))
               attributes)))

    (apply #'svg-text svg text
           :x center-x :y center-y :fill text-color
           :font-size (number-to-string font-size)
           :font-family igo-svg-font-family
           :font-weight igo-svg-font-weight
           :text-anchor "middle"
           ;; :alignment-baseline "central" ;;librsvg issue
           :baseline-shift igo-svg-font-baseline-shift
           attributes)))

(defun igo-svg-text-at-intersection (svg x y grid-interval text text-color &optional attributes font-size-scale)
  (igo-svg-text-on-board
   svg
   (* x grid-interval)
   (* y grid-interval)
   grid-interval
   text text-color
   attributes font-size-scale))

;; Move Number

(defun igo-svg-move-number (svg x y grid-interval num text-color)
  (igo-svg-text-at-intersection
   (igo-svg-move-numbers-group svg)
   x y grid-interval
   (number-to-string num)
   text-color
   (list :id (igo-svg-move-number-id x y)
         :class "mvnum")))

(defun igo-svg-move-numbers (svg board grid-interval current-node)
  (igo-svg-remove-move-numbers svg)

  (let ((numbered (make-vector (igo-board-intersection-count board) nil))
        (move-number (igo-node-move-number current-node))
        (node current-node))
    (while node
      ;; skip pass, resign, setup
      (if (igo-node-placement-p node)
          (let ((pos (igo-node-move node)))
            (when (not (aref numbered pos))
              (aset numbered pos t)
              (let* ((x (igo-board-pos-to-x board pos))
                     (y (igo-board-pos-to-y board pos))
                     (istate (igo-board-get-at board pos))
                     (color (cond
                             ;; last move
                             ((eq node current-node) "#f00")
                             ;; stone
                             ((igo-black-p istate) "#fff")
                             ((igo-white-p istate) "#000")
                             ;; emtpy
                             ((igo-white-p (igo-node-color node)) "#fff")
                             (t "#000"))))
                (igo-svg-move-number svg x y grid-interval move-number color)))))
      ;; previous node
      (let ((prev (igo-node-prev node))
            (mn (igo-node-get-move-number-property node)))
        (setq move-number
              (cond
               ((and prev mn) (igo-node-move-number prev)) ;; recalculate
               ((igo-node-move-p node) (1- move-number)))) ;; decrement
        (setq node prev)))))

(defun igo-svg-remove-move-numbers (svg)
  ;;@todo 1.get (igo-svg-move-numbers-group svg), 2.delete children(class="mvnum")
  (dolist (node (dom-by-class svg "^mvnum$"))
    (dom-remove-node svg node)))

;; Branchs(Next Nodes) Text

(defun igo-svg-branches (svg board move-color grid-interval node &optional fun-pass fun-resign fun-setup)
  ;; Remove old text
  (igo-svg-remove-branches svg)

  ;; Add new text
  (let* ((branch-index 0)
         (next-nodes (igo-node-next-nodes node))
         (num-next-nodes (length next-nodes))
         (text-color (if (igo-black-p move-color) "#000" "#fff"))
         (class-name "branches"))
    (dolist (next next-nodes)
      (let ((text (if (= num-next-nodes 1) "X" (string (+ ?A branch-index))))) ;;@todo x => svg-line?
        (cond
         ((igo-node-placement-p next)
          (if (igo-same-color-p (igo-node-color next) move-color)
              (igo-svg-text-at-intersection
               (igo-svg-overlays-group svg)
               (igo-board-pos-to-x board (igo-node-move next))
               (igo-board-pos-to-y board (igo-node-move next))
               grid-interval text text-color (list :class class-name))))
         ((igo-node-pass-p next)
          (if (and (igo-same-color-p (igo-node-color next) move-color)
                   (functionp fun-pass))
              (funcall fun-pass branch-index num-next-nodes text text-color move-color class-name)))
         ((igo-node-resign-p next)
          (if (and (igo-same-color-p (igo-node-color next) move-color)
                   (functionp fun-resign))
              (funcall fun-resign branch-index num-next-nodes text text-color move-color class-name)))
         ((igo-node-setup-p next)
          (if (functionp fun-setup)
              (funcall fun-setup branch-index num-next-nodes text text-color move-color class-name)))))
      (setq branch-index (1+ branch-index)))))

(defun igo-svg-remove-branches (svg)
  ;;@todo 1.get (igo-svg-overlays-group svg), 2.delete children(class="branches")
  (dolist (node (dom-by-class svg "^branches$"))
    (dom-remove-node svg node)))

;; Last Move Mark

(defun igo-svg-last-move (svg game grid-interval)
  (igo-svg-remove-last-move svg)

  (let* ((curr-node (igo-game-current-node game))
         (move (igo-node-move curr-node)))
    (if (igo-placement-p move)
        (let* ((board (igo-game-board game))
               (x (igo-board-pos-to-x board move))
               (y (igo-board-pos-to-y board move))
               (cx (* x grid-interval))
               (cy (* y grid-interval))
               (r (ceiling (* grid-interval 0.15)))
               ;;(color (igo-opposite-color (igo-game-turn game)))
               )
          (svg-rectangle
           ;;(igo-svg-overlays-group svg)
           (igo-svg-stones-group svg)
           (- cx r)
           (- cy r)
           (* 2 r)
           (* 2 r)
           :id "last-move"
           :fill (cond
                  ;;((igo-black-p color) "rgba(255,255,255,0.5)")
                  ;;((igo-white-p color) "rgba(0,0,0,0.5)")
                  (t "rgba(255,0,0,0.8)")))))))

(defun igo-svg-remove-last-move (svg)
  (dom-remove-node svg (car (dom-by-id svg "^last-move$"))))

;; Marker

(defun igo-svg-intersection-text-color (board x y)
  (let* ((pos (igo-board-xy-to-pos board x y))
         (istate (igo-board-get-at board pos)))
    (cond
     ((igo-black-p istate) "#fff")
     ((igo-white-p istate) "#000")
     (t "#fff"))))

(defun igo-svg-cross-mark (svg x y mark-color board grid-interval)
  (let* ((cx (* x grid-interval))
         (cy (* y grid-interval))
         (r (* 0.25 grid-interval)))
    (svg-path
     svg
     (list (list 'moveto (list (cons (- cx r) (- cy r))))
           (list 'lineto (list (cons (+ cx r) (+ cy r))))
           (list 'moveto (list (cons (+ cx r) (- cy r))))
           (list 'lineto (list (cons (- cx r) (+ cy r)))))
     :stroke-width 3
     :stroke (or mark-color (igo-svg-intersection-text-color board x y))
     :fill "none"
     :class "mark")))

(defun igo-svg-circle-mark (svg x y mark-color board grid-interval)
  (svg-circle
   svg
   (* x grid-interval)
   (* y grid-interval)
   (* 0.3 grid-interval)
   :stroke-width 3
   :stroke (or mark-color (igo-svg-intersection-text-color board x y))
   :fill "none"
   :class "mark"))

(defun igo-svg-square-mark (svg x y mark-color board grid-interval)
  (let* ((r (* 0.25 grid-interval)))
    (svg-rectangle
     svg
     (- (* x grid-interval) r)
     (- (* y grid-interval) r)
     (* 2 r)
     (* 2 r)
     :stroke-width 3
     :stroke (or mark-color (igo-svg-intersection-text-color board x y))
     :fill "none"
     :class "mark")))

(defun igo-svg-triangle-mark (svg x y mark-color board grid-interval)
  (let* ((cx (* x grid-interval))
         (cy (* y grid-interval))
         (r (* 0.3 grid-interval))
         (rt3 (sqrt 3)))
    (svg-polygon
     svg
     (list (cons cx (+ cy (* r rt3 -0.55)))
           (cons (+ cx r) (+ cy (* r rt3 0.45)))
           (cons (- cx r) (+ cy (* r rt3 0.45))))
     :stroke-width 3
     :stroke (or mark-color (igo-svg-intersection-text-color board x y))
     :fill "none"
     :class "mark")))

(defun igo-svg-marks (svg board grid-interval marks)
  (igo-svg-remove-marks svg)

  (dolist (mark marks)
    (let* ((type (igo-mark-type mark))
           (pos (igo-mark-pos mark))
           (x (igo-board-pos-to-x board pos))
           (y (igo-board-pos-to-y board pos))
           (parent (igo-svg-overlays-group svg)))
      (cond
       ((eq type 'cross) (igo-svg-cross-mark parent x y nil board grid-interval))
       ((eq type 'circle) (igo-svg-circle-mark parent x y nil board grid-interval))
       ((eq type 'square) (igo-svg-square-mark parent x y nil board grid-interval))
       ((eq type 'triangle) (igo-svg-triangle-mark parent x y nil board grid-interval))
       ((eq type 'text) (igo-svg-text-at-intersection
                         parent x y grid-interval
                         (igo-mark-text mark)
                         (igo-svg-intersection-text-color board x y)
                         '(:class "mark")))
       ))))

(defun igo-svg-remove-marks (svg)
  (let ((parent (igo-svg-overlays-group svg)))
    (dolist (node (dom-by-class parent "^mark$"))
      (dom-remove-node parent node))))


;;
;; SVG DOM Utilities
;;

(defun igo--svg-gradient (svg id type stops &rest args)
  "Instead of svg-gradient. svg-gradient not support additional attributes(cx, cy, fx, fy, r, etc...)"
  (svg--def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
	'linearGradient
      'radialGradient)
    `((id . ,id)
      ,@(svg--arguments svg args))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
			 (stop-color . ,(cdr stop)))))
     stops))))



(provide 'igo-view)
;;; igo-view.el ends here
