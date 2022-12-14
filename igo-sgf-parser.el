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

(require 'cl-lib)
(require 'subr-x)

;;
;; Input Stream Functions:
;;
;; - (igo-sgf-scan strm)
;; - (igo-sgf-get strm)
;; - (igo-sgf-discard strm)
;; - (igo-sgf-strm-pos strm)
;; - (igo-sgf-error strm message)

(defun igo-sgf-scan (_strm)
  (char-after))

(defun igo-sgf-get (_strm)
  (when (< (point) (point-max))
    (forward-char)
    (char-before)))

(defun igo-sgf-discard (_strm)
  (if (< (point) (point-max))
      (forward-char)))

(defun igo-sgf-strm-pos (_strm)
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
  "Parse GameTree and return a tree object [leading-wss begin-pos
nodes subtrees end-pos]."

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
    (igo-sgf-tree-each-nodes subtree func)))

(defun igo-sgf-tree-root-node (tree)
  (car (igo-sgf-tree-nodes tree)))

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

;; Root Node

(defun igo-sgf-node-get-board-size (node)
  (let* ((sz-value (igo-sgf-node-get-property-value node "SZ" "19"))
         (sz (igo-sgf-split-compose sz-value))
         (w (string-to-number (igo-sgf-prop-value-content (if (consp sz) (car sz) sz))))
         (h (string-to-number (igo-sgf-prop-value-content (if (consp sz) (cdr sz) sz)))))
    (if (not (and (>= w 1) (<= w 52) (>= h 1) (<= h 52)))
        (error "%sInvalid board size(w=%s h=%s)." (igo-sgf-prop-value-location-string sz-value) w h))
    (cons w h)))

(defun igo-sgf-node-check-game-type (node)
  (let* ((gm-value (igo-sgf-node-get-property-value node "GM" "1"))
         (gm-str (igo-sgf-prop-value-content gm-value)))
    (if (not (string= gm-str "1"))
        (error "%sGame type is not Go(GM[1])." (igo-sgf-prop-value-location-string gm-value))
      1)))

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
        (cl-loop for y from (cdr lt) to (cdr rb) do
                 (cl-loop for x from (car lt) to (car rb) do
                          (push (cons x y) points)))
        (nreverse points)))))


;; Game Info Properties

(defconst igo-sgf-game-info-properties
  '(
    ("CP" "Copyright")
    ("US" "Enterer Name")
    ("AN" "Annotator Name")
    ("SO" "Source")
    ("EV" "Event Name")
    ("GN" "Game Name")
    ("RO" "Round Number")
    ("DT" "Date")
    ("PC" "Place")
    ("BT" "Black Team" :player black)
    ("PB" "Black Player" :player black)
    ("BR" "Black Rank" :player black)
    ("WT" "White Team" :player white)
    ("PW" "White Player" :player white)
    ("WR" "White Rank" :player white)
    ("RU" "Rule")
    ("OT" "Overtime Method")
    ("TM" "Time Limit" :type real)
    ("HA" "Handicap Stones" :type number)
    ("KM" "Komi" :type real)
    ("RE" "Result")
    ("ON" "Opening Moves")
    ("GC" "Comment" :type text)
    ))

(defun igo-sgf-game-info-prop-id (prop)
  (car prop))
(defun igo-sgf-game-info-prop-title (prop)
  (cadr prop))
(defun igo-sgf-game-info-prop-type (prop)
  (or (plist-get (cddr prop) :type) 'simpletext))


;; Encode Text

(defun igo-sgf-text (text &optional compose-type)
  ;; see: https://www.red-bean.com/sgf/sgf4.html#text
  (if compose-type
      (replace-regexp-in-string "\\([]:\\\\]\\)" "\\\\\\1" text) ;;escape :
    (replace-regexp-in-string "\\([]\\\\]\\)" "\\\\\\1" text)))

(defun igo-sgf-simple-text (text &optional compose-type)
  ;;@todo same as igo-sgf-text?
  ;; see: https://www.red-bean.com/sgf/sgf4.html#text
  (if compose-type
      (replace-regexp-in-string "\\([]:\\\\]\\)" "\\\\\\1" text) ;;escape :
    (replace-regexp-in-string "\\([]\\\\]\\)" "\\\\\\1" text)))


(provide 'igo-sgf-parser)
;;; igo-sgf-parser.el ends here
