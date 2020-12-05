;;; igo-sgf-parser.el --- SGF Parser                 -*- lexical-binding: t; -*-

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

(require 'igo-model)

;;
;; Input Stream Functions:
;;
;; - (igo-sgf-scan strm)
;; - (igo-sgf-get strm)
;; - (igo-sgf-discard strm)
;; - (igo-sgf-strm-pos strm)
;; - (igo-sgf-error strm message)

(defun igo-sgf-scan (strm)
  (char-after))

(defun igo-sgf-get (strm)
  (when (< (point) (point-max))
    (forward-char)
    (char-before)))

(defun igo-sgf-discard (strm)
  (if (< (point) (point-max))
      (forward-char)))

(defun igo-sgf-strm-pos (strm)
  (point))

(defun igo-sgf-error (strm message)
  (error "%s: %s" (igo-sgf-strm-pos strm) message))

;;
;; Character Classification
;;

(defun igo-sgf-ws-p (c)
  (and
   (integerp c)
   (or (= c ? )
       (= c ?\t)
       (= c ?\v)
       (= c ?\n)
       (= c ?\r))))

(defun igo-sgf-ucletter-p (c)
  (and (integerp c) (>= c ?A) (<= c ?Z)))

;;
;; Stream Utilities
;;

(defun igo-sgf-skip-ws (strm)
  (let (ch wss)
    (while (igo-sgf-ws-p (setq ch (igo-sgf-scan strm)))
      (push ch wss)
      (igo-sgf-discard strm))
    (concat (nreverse wss))))

(defun igo-sgf-match-char (strm ch)
  (let ((strm-ch (igo-sgf-scan strm)))
    (if (not (equal strm-ch ch))
        (igo-sgf-error strm (format "Unexpected %s (expecting '%c')" (if strm-ch (concat "'" (char-to-string strm-ch) "'") "end of SGF") ch))
      (igo-sgf-discard strm))))

(defun igo-sgf-scan-ucletters (strm)
  "Scan [A-Z]* from strm."
  (let (ch chars)
    (while (igo-sgf-ucletter-p (setq ch (igo-sgf-scan strm)))
      (push ch chars)
      (igo-sgf-discard strm))
    (concat (nreverse chars))))

;;
;; Parse SGF
;;
;; Syntax:
;;
;;  Collection = GameTree+
;;  GameTree   = "(" Sequence GameTree* ")"
;;  Sequence   = Node+
;;  Node       = ";" Property*
;;  Property   = PropIdent PropValue+
;;  PropIdent  = UcLetter+
;;  PropValue  = "[" CValueType "]"
;;  CValueType = (ValueType | Compose)
;;  ValueType  = (None | Number | Real | Double | Color | SimpleText |
;;                Text | Point  | Move | Stone)
;;  (see: https:www.red-bean.com/sgf/sgf4.html )
;;
;; Functions corresponding to non-terminal symbols:
;; - igo-sgf-parse-tree-list :: GameTree*
;; - igo-sgf-parse-tree :: GameTree
;; - igo-sgf-parse-node-list :: Node*
;; - igo-sgf-parse-node :: Node
;; - igo-sgf-parse-property-list :: Property*
;; - igo-sgf-parse-property :: Property
;; - igo-sgf-parse-prop-value :: PropValue

(defun igo-sgf-parse-tree-list (strm)
  "Parse 0 or more GameTree and return list of tree object."
  ;; Tree*
  ;; (...) (...) (...)
  (let (trees wss)
    (while (progn (setq wss (igo-sgf-skip-ws strm)) (equal (igo-sgf-scan strm) ?\())
      (push (igo-sgf-parse-tree strm wss) trees))
    (nreverse trees)))

(defun igo-sgf-parse-tree (strm &optional leading-wss)
  "Parse GameTree and return a tree object [leading-wss begin-pos nodes subtrees end-pos]."

  (setq leading-wss (concat leading-wss (igo-sgf-skip-ws strm)))

  ;; ( Node+ Tree* )
  ;; ( ;... ;... ;... (...) (...) (...) )
  (let (begin-pos nodes subtrees end-pos)
    ;; (
    (setq begin-pos (igo-sgf-strm-pos strm))
    (igo-sgf-match-char strm ?\()

    ;; Node+
    (setq nodes (igo-sgf-parse-node-list strm))
    (if (null nodes)
        (igo-sgf-error strm "GameTree requires one or more Nodes"))

    ;; Tree*
    (setq subtrees (igo-sgf-parse-tree-list strm))

    ;; )
    ;;;@todo skip white spaces?
    (igo-sgf-match-char strm ?\))
    (setq end-pos (igo-sgf-strm-pos strm))

    (vector leading-wss begin-pos nodes subtrees end-pos)))

(defun igo-sgf-parse-node-list (strm) ;;Sequence
  "Parse 0 or more Node and return list of node object."
  ;; Node*
  ;; ;... ;... ;...
  (let (nodes wss)
    (while (progn (setq wss (igo-sgf-skip-ws strm)) (equal (igo-sgf-scan strm) ?\;))
      (push (igo-sgf-parse-node strm wss) nodes))
    (nreverse nodes)))

(defun igo-sgf-parse-node (strm &optional leading-wss)
  "Parse Node and return a node object [leading-wss begin-pos properties end-pos]."

  (setq leading-wss (concat leading-wss (igo-sgf-skip-ws strm)))

  ;; ; Property*
  ;; ; PID[PValue][PValue][PValue]PID[PValue][PValue][PValue]...
  (let (begin-pos properties end-pos)
    ;; ;
    (setq begin-pos (igo-sgf-strm-pos strm))
    (igo-sgf-match-char strm ?\;)

    ;; Property*
    (setq properties (igo-sgf-parse-property-list strm))

    (setq end-pos (igo-sgf-strm-pos strm))

    (vector leading-wss begin-pos properties end-pos)))

(defun igo-sgf-parse-property-list (strm)
  ;; Property*
  ;; PID[PValue][PValue][PValue]PID[PValue][PValue][PValue]...
  (let (properties wss)
    (while (progn (setq wss (igo-sgf-skip-ws strm)) (igo-sgf-ucletter-p (igo-sgf-scan strm)))
      (push (igo-sgf-parse-property strm wss) properties))
    (nreverse properties)))

(defun igo-sgf-parse-property (strm &optional leading-wss)
  "Return property object [leading-wss begin-pos id values end-pos]"

  (setq leading-wss (concat leading-wss (igo-sgf-skip-ws strm)))

  ;; PropIdent PropValue+
  (let (begin-pos id values end-pos)
    (setq begin-pos (igo-sgf-strm-pos strm))

    ;; PropIdent
    (setq id (igo-sgf-scan-ucletters strm))
    (if (string-empty-p id)
        (igo-sgf-error strm "No property ID"))

    ;; PropValue+
    (let (wss)
      (while (progn (setq wss (igo-sgf-skip-ws strm)) (equal (igo-sgf-scan strm) ?\[))
        (push (igo-sgf-parse-prop-value strm wss) values)))
    (setq values (nreverse values))
    (if (null values)
        (igo-sgf-error strm "Property requires one or more PropValues"))

    (setq end-pos (igo-sgf-strm-pos strm))

    (vector leading-wss begin-pos id values end-pos)))

(defun igo-sgf-parse-prop-value (strm &optional leading-wss)

  (setq leading-wss (concat leading-wss (igo-sgf-skip-ws strm)))

  (let (begin-pos ch chars end-pos)
    (setq begin-pos (igo-sgf-strm-pos strm))
    (igo-sgf-match-char strm ?\[)

    (while (not (equal (setq ch (igo-sgf-get strm)) ?\]))
      ;; end of strm
      (if (null ch)
          (igo-sgf-error strm "Unexpected end of SGF in Property Value"))
      ;; push ch
      (push ch chars)
      ;; NOTE: Do not resolve compose value here(colon separation). Depends on property type.

      ;; skip \], convert \r \n\r \r\n to \n
      (if (= ch ?\\)
          (let ((escaped-ch (igo-sgf-get strm)))
            (cond
             ;; convert \\\n\r and \\\r\n to single \n
             ((or (and (= escaped-ch ?\n) (equal (igo-sgf-scan strm) ?\r))
                  (and (= escaped-ch ?\r) (equal (igo-sgf-scan strm) ?\n)))
              (igo-sgf-discard strm)
              (push ?\n chars))
             ;; convert single \r to \n
             ((or (= escaped-ch ?\r))
              (push ?\n chars))
             ;; ], \, :, \n, spaces, etc...
             (t (push escaped-ch chars))))))

    (setq end-pos (igo-sgf-strm-pos strm))

    ;; return value
    (igo-sgf-make-prop-value leading-wss begin-pos (concat (nreverse chars)) end-pos)))


;;
;; Syntax Tree Accessors
;;

(defun igo-sgf-format-location (begin end)
  (cond
   ((and begin end) (format "%s-%s: " begin end))
   (begin (format "%s: " begin))
   (end (format "%s: " end))
   (t "")))

;; Tree
(defun igo-sgf-tree-leading-white-spaces (tree) (aref tree 0))
(defun igo-sgf-tree-begin (tree) (aref tree 1))
(defun igo-sgf-tree-nodes (tree) (aref tree 2))
(defun igo-sgf-tree-subtrees (tree) (aref tree 3))
(defun igo-sgf-tree-end (tree) (aref tree 4))
(defun igo-sgf-tree-each-nodes (tree func)
  (dolist (node (igo-sgf-tree-nodes tree))
    (funcall func node tree))
  (dolist (subtree (igo-sgf-tree-subtrees tree))
    (igo-sgf-tree-each-nodes subtree)))

(defun igo-sgf-tree-get-board-size (tree)
  (let* ((root-node (car (igo-sgf-tree-nodes tree)))
         (sz-value (igo-sgf-node-get-property-value root-node "SZ" "19"))
         (sz (igo-sgf-split-compose sz-value))
         (w (string-to-number (igo-sgf-prop-value-content (if (consp sz) (car sz) sz))))
         (h (string-to-number (igo-sgf-prop-value-content (if (consp sz) (cdr sz) sz)))))
    (if (not (and (>= w 1) (<= w 52) (>= h 1) (<= h 52)))
        (error "%sInvalid board size(w=%s h=%s)." (igo-sgf-prop-value-location-string sz-value) w h))
    (cons w h)))

;; Node
(defun igo-sgf-node-leading-white-spaces (node) (aref node 0))
(defun igo-sgf-node-begin (node) (aref node 1))
(defun igo-sgf-node-properties (node) (aref node 2))
(defun igo-sgf-node-end (node) (aref node 3))
(defun igo-sgf-node-get-property (node id)
  (seq-find (lambda (prop) (string= (igo-sgf-property-id prop) id))
            (igo-sgf-node-properties node)))
(defun igo-sgf-node-get-property-value (node id &optional default)
  (let* ((property (igo-sgf-node-get-property node id)))
    (if (null property)
        (igo-sgf-make-prop-value "" nil default nil)
      (igo-sgf-property-ensure-one-value property)
      (let* ((values (igo-sgf-property-values property)))
        (car values)))))
(defun igo-sgf-node-location-string (node)
  (igo-sgf-format-location
   (igo-sgf-node-begin node)
   (igo-sgf-node-end node)))

;; Property
(defun igo-sgf-property-leading-white-spaces (prop) (aref prop 0))
(defun igo-sgf-property-begin (prop) (aref prop 1))
(defun igo-sgf-property-id (prop) (aref prop 2))
(defun igo-sgf-property-values (prop) (aref prop 3))
(defun igo-sgf-property-end (prop) (aref prop 4))
(defun igo-sgf-property-location-string (prop)
  (igo-sgf-format-location
   (igo-sgf-property-begin prop)
   (igo-sgf-property-end prop)))

(defun igo-sgf-property-ensure-one-value (prop)
  (if (/= (length (igo-sgf-property-values prop)) 1)
      (error "%sProperty %s has only one value."
             (igo-sgf-property-location-string prop)
             (igo-sgf-property-id prop))))

;; Value
(defun igo-sgf-make-prop-value (leading-wss begin-pos value-str end-pos)
  (vector leading-wss begin-pos value-str end-pos))
(defun igo-sgf-prop-value-leading-white-spaces (prop-value) (aref prop-value 0))
(defun igo-sgf-prop-value-begin (prop-value) (aref prop-value 1))
(defun igo-sgf-prop-value-content (prop-value) (aref prop-value 2))
(defun igo-sgf-prop-value-end (prop-value) (aref prop-value 3))
(defun igo-sgf-prop-value-location-string (prop-value)
  (igo-sgf-format-location
   (igo-sgf-prop-value-begin prop-value)
   (igo-sgf-prop-value-end prop-value)))

(defun igo-sgf-split-compose (value)
  "Split string VALUE with a colon.

(pv \"ABCD\") => (pv \"ABCD\")
(pv \"AB:CD\") => ((pv \"AB\") . (pv \"CD\"))
(pv \"AB\\:):CD\") => ((pv \"AB:)\") . (pv \"CD\"))
(pv \"AB:CD:EF\") => error

NOTE: (pv str)=(igo-sgf-make-prop-value "" nil str nil)
"
  (let* ((value-str (igo-sgf-prop-value-content value))
         (value-length (length value-str))
         colon-pos
         (i 0))
    (while (< i value-length)
      (let ((ch (elt value-str i)))
        (cond
         ;; skip next escaped char
         ((= ch ?\\)
          (setq i (1+ i)))
         ;; colon
         ((= ch ?:)
          (if colon-pos
              (error "%sToo many colons in compose value : %s"
                     (igo-sgf-prop-value-location-string value)
                     value-str))
          (setq colon-pos i)))
        (setq i (1+ i))))
    (if (null colon-pos)
        value
      (let ((value-begin (igo-sgf-prop-value-begin value)))
        (cons (igo-sgf-make-prop-value
               (igo-sgf-prop-value-leading-white-spaces value)
               value-begin
               (substring value-str 0 colon-pos)
               (if (integerp value-begin) (+ value-begin colon-pos)))
              (igo-sgf-make-prop-value
               ""
               (if (integerp value-begin) (+ value-begin colon-pos 1))
               (substring value-str (1+ colon-pos))
               (igo-sgf-prop-value-end value)) )))))

(defun igo-sgf-value-as-text (value)
  (if (vectorp value) (setq value (igo-sgf-prop-value-content value)))
  ;; Delete escaped line-break
  (setq value (replace-regexp-in-string "\\\\\\(\n\r?\\|\r\n?\\)" "" value))
  ;; Escape character
  (setq value (replace-regexp-in-string "\\\\\\(.\\)" "\\1" value))
  ;; Convert Spaces
  (setq value (replace-regexp-in-string "\t\\|\v" " " value))
  value)

(defun igo-sgf-value-as-simple-text (value)
  (if (vectorp value) (setq value (igo-sgf-prop-value-content value)))
  ;; Delete escaped line-break
  (setq value (replace-regexp-in-string "\\\\\\(\n\r?\\|\r\n?\\)" "" value))
  ;; Escape character
  (setq value (replace-regexp-in-string "\\\\\\(.\\)" "\\1" value))
  ;; Convert Spaces
  (setq value (replace-regexp-in-string "\t\\|\v\\|\n\r?\\|\r\n?" " " value))
  value)

(defun igo-sgf-value-as-color (value)
  (let ((value-str (igo-sgf-prop-value-content value)))
    (cond
     ((string= value-str "B") 'black)
     ((string= value-str "W") 'white)
     (t (error "%sNot a color '%s'"
               (igo-sgf-prop-value-location-string value)
               value-str)))))

(defun igo-sgf-point-letter-to-number (ch pos)
  "Convert a point type(same as move type) letter to an integer.

?a-?z ?A-?Z to 0-25 26-51"
  (cond
   ((and (>= ch ?a) (<= ch ?z)) (- ch ?a))
   ((and (>= ch ?A) (<= ch ?Z)) (+ (- ch ?A) 26))
   (t (error "%s: Invalid point letter '%c'" pos ch))))

(defun igo-sgf-value-as-point-xy (value &optional w h)
  "Convert point type(same as move type) value to (x . y).

ex: (igo-sgf-value-as-point-xy (igo-sgf-make-prop-value "" nil \"da\" nil)) => (3 . 0)"
  (let ((value-str (igo-sgf-prop-value-content value)))
    (if (/= (length value-str) 2)
        (error "%sPoint value is not two letters." (igo-sgf-prop-value-location-string value)))
    (let ((x (igo-sgf-point-letter-to-number (elt value-str 0) (+ (igo-sgf-prop-value-begin value) 1)))
          (y (igo-sgf-point-letter-to-number (elt value-str 1) (+ (igo-sgf-prop-value-begin value) 2))))
      (if (not (and (>= x 0) (>= y 0) (or (null w) (< x w)) (or (null h) (< y h))))
          (error "%sPoint value %s is out of board (x=%s y=%s)" (igo-sgf-prop-value-location-string value) value-str x y))
      (cons x y))))

(defun igo-sgf-value-as-point-pos (value w h)
  (let ((xy (igo-sgf-value-as-point-xy value w h)))
    (igo-xy-to-pos (car xy) (cdr xy) w)))

(defun igo-sgf-value-as-move (value w h)
  (let ((value-str (igo-sgf-prop-value-content value)))

    (cond
     ((null value-str)
      (error "%sNo property value for move"
             (igo-sgf-prop-value-location-string value)))
     ((or (string= value-str "") (and (= w 19)
                                      (= h 19)
                                      (string= value-str "tt")))
      igo-pass)
     (t
      (igo-sgf-value-as-point-pos value w h)))))

(defun igo-sgf-expand-compressed-point (value &optional w h)
  "https://www.red-bean.com/sgf/sgf4.html#3.5.1
ex:
(igo-sgf-expand-compressed-point (igo-sgf-make-prop-value "" nil \"jk\" nil)) => ((9 . 10))
(igo-sgf-expand-compressed-point (igo-sgf-make-prop-value "" nil \"jk:lm\" nil) => ((9 . 10) (10 . 10) (11 . 10) (9 . 11) (10 . 11) (11 . 11) (9 . 12) (10 . 12) (11 . 12))
"
  ;;(if (vectorp value) (setq value (igo-sgf-prop-value-content value)))

  (let ((values (igo-sgf-split-compose value)))
    (if (vectorp values)
        ;; not composed
        (list (igo-sgf-value-as-point-xy values w h))
      ;; composed (rectangle)
      (let ((lt (igo-sgf-value-as-point-xy (car values) w h))
            (rb (igo-sgf-value-as-point-xy (cdr values) w h))
            points)
        (loop for y from (cdr lt) to (cdr rb) do
              (loop for x from (car lt) to (car rb) do
                    (push (cons x y) points)))
        (nreverse points)))))

(defun igo-sgf-values-as-point-pos-list (values w h)
  "ex: (igo-sgf-values-as-point-pos-list (mapcar (lambda (v) (igo-sgf-make-prop-value \"\" nil v nil)) '(\"aa\" \"ba\" \"ab\" \"ca:da\")) 9 9) => (0 1 9 2 3)"
  (loop for value in values nconc
        ;; convert (x . y) to pos
        (seq-map
         (lambda (xy) (igo-xy-to-pos (car xy) (cdr xy) w))
         ;; expand compose
         (igo-sgf-expand-compressed-point value w h))))

;;
;; Convert to igo-node
;;

(defun igo-sgf-root-tree-to-game-tree (sgf-root-tree)
  (let* ((size (igo-sgf-tree-get-board-size sgf-root-tree))
         (w (car size))
         (h (cdr size)))
    (igo-sgf-tree-to-game-tree sgf-root-tree w h 0 'black nil)))

(defun igo-sgf-tree-to-game-tree (sgf-tree w h move-number turn prev-node)
  "Convert parsed SGF syntax tree to igo-node."
  (when sgf-tree
    (let (seq-head-node)
      ;; Sequence
      (dolist (sgf-node (igo-sgf-tree-nodes sgf-tree))
        ;; Create a node
        (let ((curr-node (if prev-node (igo-node-create-next-node prev-node) (igo-node nil)))
              (moved nil)
              setup-black setup-white setup-empty setup-turn)
          ;; For each properties
          (dolist (sgf-prop (igo-sgf-node-properties sgf-node))
            (let ((prop-id (igo-sgf-property-id sgf-prop))
                  (prop-values (igo-sgf-property-values sgf-prop)))
              (cond
               ;; Move
               ((or (string= prop-id "B") (string= prop-id "W"))
                (if moved
                    (error "%sOnly one move in one node"
                           (igo-sgf-property-location-string sgf-prop)))
                (if (not (string= (if (igo-black-p turn) "B" "W") prop-id))
                    (error "%sTurn mismatch"
                           (igo-sgf-property-location-string sgf-prop)))
                (igo-sgf-property-ensure-one-value sgf-prop)
                (let ((move (igo-sgf-value-as-move (car prop-values) w h)))
                  (if (and prev-node (igo-node-find-next-by-move prev-node move))
                      (error "%sMove %s already exists"
                             (igo-sgf-property-location-string sgf-prop)
                             (igo-sgf-prop-value-content (car prop-values))))
                  (igo-node-set-move curr-node move))
                (setq move-number (1+ move-number))
                (setq turn (igo-opposite-color turn))
                (setq moved t))
               ;; Setup
               ((string= prop-id "AE")
                (setq setup-empty (nconc setup-empty (igo-sgf-values-as-point-pos-list prop-values w h))))
               ((string= prop-id "AB")
                (setq setup-black (nconc setup-black (igo-sgf-values-as-point-pos-list prop-values w h))))
               ((string= prop-id "AW")
                (setq setup-white (nconc setup-white (igo-sgf-values-as-point-pos-list prop-values w h))))
               ((string= prop-id "PL")
                (setq setup-turn (igo-sgf-value-as-color (car prop-values))))
               ;; Discard FF, GM, SZ Property
               ((string= prop-id "FF") )
               ((string= prop-id "GM") )
               ((string= prop-id "SZ") )
               ;; Other SGF Properties
               (t (igo-node-set-sgf-property curr-node prop-id
                                             (mapcar #'igo-sgf-prop-value-content prop-values)))
               )))
          ;; Record setup property
          (when (or setup-black setup-white setup-empty setup-turn)
            (if moved
                (error "%sSetup properties and move properties cannot be mixed in same node"
                       (igo-sgf-node-location-string sgf-node) ))
            (igo-node-set-setup-property curr-node (igo-board-changes setup-black setup-white setup-empty nil setup-turn nil nil))
            (if setup-turn (setq turn setup-turn)) )
          ;; Record location
          (igo-node-set-sgf-location
           curr-node
           (if (null seq-head-node) (igo-sgf-tree-begin sgf-tree))
           (igo-sgf-node-begin sgf-node)
           (igo-sgf-node-end sgf-node)
           (if (null seq-head-node) (igo-sgf-tree-end sgf-tree)))
          ;; Record head of sequence
          (if (null seq-head-node) (setq seq-head-node curr-node))
          ;; Curr to Prev
          (setq prev-node curr-node)))
      ;; Subtrees
      (dolist (sgf-subtree (igo-sgf-tree-subtrees sgf-tree))
        (igo-sgf-tree-to-game-tree sgf-subtree w h move-number turn prev-node))

      seq-head-node)))


;;
;; To SGF String
;;

(defun igo-sgf-color (color)
  (cond
   ((igo-black-p color) "B")
   ((igo-white-p color) "W")
   (t (error "Cannot convert to SGF from invalid color %s" color))))

(defun igo-sgf-point-letter (n)
  (cond
   ((and (>= n 0) (<= n 25)) (+ ?a n))
   ((and (>= n 26) (<= n 51)) (+ ?A (- n 26)))
   (t (error "Cannot convert to SGF from invalid point number %s" n))))

(defun igo-sgf-point (pos w)
  (string
   (igo-sgf-point-letter (igo-pos-to-x pos w))
   (igo-sgf-point-letter (igo-pos-to-y pos w))))

(defun igo-sgf-string-from-game-tree (node w h turn)
  (let ((sgf-tree-p (or (igo-node-root-p node) ;; is root
                        (> (igo-node-number-of-siblings node) 1)))) ;;has siblings

    (concat
     (if sgf-tree-p "(")
     ";"

     ;; Root Properties
     (if (igo-node-root-p node)
         (concat "FF[4]GM[1]SZ[" (if (= w h) (number-to-string w) (format "%s:%s" w h)) "]"))

     ;; Move Property
     (let* ((move (igo-node-move node)))
       (cond
        ((igo-placement-p move) (concat (igo-sgf-color turn) "[" (igo-sgf-point move w) "]" ))
        ((igo-pass-p move) (concat (igo-sgf-color turn) "[]"))
        ((igo-resign-p move) nil))) ;;Ignore

     ;; Properties
     ;; (let ((prop (igo-node-properties node)))
     ;;   (while prop
     ;;     (let ((key (car prop)) (value (cadr prop)))
     ;;       (cond
     ;;        ((eq key ??))))))

     ;; Setup Properties
     (let ((board-change (igo-node-get-setup-property node)))
       (when board-change
         (let ((change-black (igo-board-changes-black board-change))
               (change-white (igo-board-changes-white board-change))
               (change-empty (igo-board-changes-empty board-change))
               (change-turn (igo-board-changes-turn board-change)))

           (if change-turn
             (setq turn change-turn))

           (concat
            (mapconcat (lambda (pos) (concat "AB[" (igo-sgf-point pos w) "]")) change-black "")
            (mapconcat (lambda (pos) (concat "AW[" (igo-sgf-point pos w) "]")) change-white "")
            (mapconcat (lambda (pos) (concat "AE[" (igo-sgf-point pos w) "]")) change-empty "")
            (if change-turn (concat "PL[" (igo-sgf-color change-turn) "]"))))))

     ;; Other SGF Properties
     (let ((props (igo-node-get-sgf-properties node))
           strs)
       (while props
         (let ((key (car props)) (values (cadr props)))
           (push (concat key (mapconcat (lambda (v) (concat "[" v "]")) values "")) strs))
         (setq props (cddr props)))
       (apply #'concat (nreverse strs)))

     ;; Sub-Nodes
     (cl-loop for subnode in (igo-node-next-nodes node) concat
              (igo-sgf-string-from-game-tree subnode w h (if (or (igo-node-placement-p node) (igo-node-pass-p node)) (igo-opposite-color turn) turn)))
     (if sgf-tree-p ")"))))


(defun igo-sgf-text (text &optional compose-type)
  ;; see: https://www.red-bean.com/sgf/sgf4.html#text
  (if compose-type
      (replace-regexp-in-string "\\([]:\\\\]\\)" "\\\\\\1" text) ;;escape :
    (replace-regexp-in-string "\\([]\\\\]\\)" "\\\\\\1" text)))


(provide 'igo-sgf-parser)
;;; igo-sgf-parser.el ends here
