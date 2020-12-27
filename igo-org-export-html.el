;;; igo-org-export-html.el --- Org HTML Exporter for Igo Special Block  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  AKIYAMA Kouhei

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

;; There are three ways to export igo special blocks (begin_igo ~ end_igo).
;;
;; - igo-org-set-html-filters-local :: Individual file settings
;; - igo-org-define-html-backend :: Define backend derived from HTML
;; - igo-org-modify-html-backend :: Modify HTML backend direct

;;; Code:

(require 'igo-org)
(require 'ox-html)

(defcustom igo-org-js-path
  "./"
  "The path to js_igo."
  :group 'el-igo
  :type 'string)

(defcustom igo-org-js-template
  "<script src=\"%PATH%igo.js\"></script>
<script src=\"%PATH%igo_view.js\"></script>
<link rel=\"stylesheet\" href=\"%PATH%igo.css\" />
<script>
window.addEventListener(\"load\", ev=>{
  for(elem of document.querySelectorAll(\"div.igo[data-igo-js]\")){
    let sgf = elem.textContent;
    while(elem.hasChildNodes()){
      elem.removeChild(elem.firstChild);
    }
    new igo.GameView(elem, sgf,
      elem.dataset.igoOpt ? JSON.parse(elem.dataset.igoOpt) :
      {\"showBranchText\": true, \"showLastMoveMark\": true, \"showComment\": true, \"path\":1000});
  }});
</script>
"
  "The js_igo template."
  :group 'el-igo
  :type 'string)

(defcustom igo-org-lazy-js-template
  "<script>
//<!--
!function(e,t){var n=function(e,t){for(var n=0;n<e.length;++n)t(e[n])},r=Promise;e.loadScriptOnViewport=function(i,o){return\"string\"==typeof i&&(i=t.getElementById(i)),new r(l=>{(function(n){return new r(r=>{function i(o){var l=n.getBoundingClientRect();l.bottom+50>=0&&l.top-50<=(e.innerHeight||t.documentElement.clientHeight)&&(e.removeEventListener(\"load\",i,!1),e.removeEventListener(\"scroll\",i,!1),r(n))}e.addEventListener(\"load\",i,!1),e.addEventListener(\"scroll\",i,!1)})})(i).then(e=>{(function e(i,o){return\"string\"==typeof i?new r(e=>{var r={};n(t.getElementsByTagName(\"link\"),e=>{\"stylesheet\"==e.getAttribute(\"rel\")&&(r[e.getAttribute(\"href\")]=e)}),n(t.getElementsByTagName(\"script\"),e=>{r[e.getAttribute(\"src\")]=e});var o=t.head,l=r[i];if(l)if(l.isUrlLoading){var s=l.onload;l.onload=s?()=>{s(),e()}:e}else e();else/\\.css/.test(i)?((l=t.createElement(\"link\")).isUrlLoading=!0,l.rel=\"stylesheet\",l.type=\"text/css\",l.href=i):((l=t.createElement(\"script\")).isUrlLoading=!0,l.type=\"text/javascript\",l.src=i),l.onload=function(t){l.isUrlLoading&&(console.log(\"loaded: \"+i),l.isUrlLoading=!1,e())},o.appendChild(l)}):o?new r(t=>{var n=()=>{0==i.length?t():e(i.shift(),!1).then(n)};n()}):r.all(i.map(t=>e(t,!0)))})(o).then(()=>{l(e)})})})}}(window,document);

var currScript = document.currentScript;
var div = document.createElement(\"div\");
currScript.parentNode.insertBefore(div, currScript);

loadScriptOnViewport(div, [\"%PATH%igo.css\", [\"%PATH%igo.js\", \"%PATH%igo_view.js\"]]).then(
    function(elem){
        var gameView = new igo.GameView(
            elem,
            igo.Game.fromSGF(\"%LITERAL_SGF%\"), %OPT_JSON%);
});
//--></script>"
  "The js_igo template for lazy loading."
  :group 'el-igo
  :type 'string)

(defcustom igo-org-header-template
  ""
  "The header template used when display option is custom."
  :group 'el-igo
  :type 'string)

(defcustom igo-org-block-template
  "%HTML_SGF%"
  "The block template used when display option is custom."
  :group 'el-igo
  :type 'string)

(defcustom igo-org-html-display
  'lazy-js
  "How to display igo special blocks."
  :group 'el-igo
  :type '(choice (const :tag "Not display" none)
                 (const :tag "Not convert(SGF text)" noconv)
                 (const :tag "SVG image" svg)
                 (const :tag "Use js_igo" js)
                 (const :tag "Use js_igo with lazy loading" lazy-js)
                 (const :tag "Use custom template" custom)))

(defvar igo-org-js-options-alist
  '((:igo-js-path "IGO_JS_PATH" nil igo-org-js-path)
    (:igo-js-template "IGO_JS_TEMPLATE" nil igo-org-js-template)
    (:igo-lazy-js-template "IGO_LAZY_JS_TEMPLATE" nil igo-org-lazy-js-template)
    (:igo-header-template "IGO_HEADER_TEMPLATE" nil igo-org-header-template)
    (:igo-block-template "IGO_BLOCK_TEMPLATE" nil igo-org-block-template)
    (:igo-enable "IGO_ENABLE" nil "t")
    (:igo-display "IGO_DISPLAY" nil igo-org-html-display)))


;; Modify Special Block

(defun igo-org-html-filter-parse-tree (data backend info)
  "A filter function to set to org-export-filter-parse-tree-functions variable.

Modify all igo special blocks in the DATA tree."
  (when (or (eq backend 'html)
            (org-export-derived-backend-p
             (org-export-get-backend backend) 'html))
    (when (and ;;(not (memq 'body-only (plist-get info :export-options)))
               (not (member (plist-get info :igo-enable) '(nil "" "nil" "no"))))

      (let ((display-types-used (igo-org-modify-all-special-blocks data info)))

        ;; Add HTML header
        (if (memq 'js display-types-used)
            (igo-org-add-to-html-head-extra
             info
             (igo-org-instantiate-header-template
              (or (plist-get info :igo-js-template) "")
              info)))
        (if (memq 'custom display-types-used)
            (igo-org-add-to-html-head-extra
             info
             (igo-org-instantiate-header-template
              (or (plist-get info :igo-header-template) "")
              info))))))
  data)

(defun igo-org-add-to-html-head-extra (info header-html)
  (if (and (stringp header-html) (not (string= header-html "")))
      (if-let ((head-extra (plist-get info :html-head-extra)))
          (plist-put info :html-head-extra
                     (concat head-extra "\n" header-html))
        ;; Add :html-head-extra property to last of INFO
        (if info
            (setcdr (last info) (list :html-head-extra header-html))
          ;; @todo if info is null
          (error "info is null")))))

(defun igo-org-modify-all-special-blocks (data info)
  (let (display-types-used)
    (org-element-map data 'special-block
      (lambda (special-block)
        (let ((display-type (igo-org-modify-special-block special-block info)))
          (if (null (memq display-type display-types-used))
              (push display-type display-types-used)))))
    display-types-used))

(defun igo-org-modify-special-block (special-block info)
  (when (string= "igo" (org-element-property :type special-block))
    (let* ((options (igo-org-get-special-block-header-options special-block))
           (display
            (or
             (let ((v (igo-org-opt-value :display options)))
               (cond ((stringp v) (intern (downcase v)))
                     ((symbolp v) v)))
             (let ((v (plist-get info :igo-display)))
               (cond ((stringp v) (intern (downcase v)))
                     ((symbolp v) v))))))
      (cond
       ((eq display 'none)
        (org-element-set-contents special-block nil))
       ((or (eq display 'noconv)
            (eq display 'sgf)))
       ((eq display 'js)
        (igo-org-modify-special-block-js special-block info))
       ((eq display 'lazy-js)
        (igo-org-modify-special-block-lazy-js special-block info))
       ((eq display 'svg)
        (igo-org-modify-special-block-svg special-block info))
       ((eq display 'custom)
        (igo-org-modify-special-block-custom special-block info)))
      display)))

(defun igo-org-modify-special-block-js (special-block info)
  ;; Add attr_html
  (igo-org-js-add-html-attr-to-special-block special-block)
  ;; Remove <p> </p>
  (igo-org-replace-element-contents-to-html
   special-block
   (igo-org-get-special-block-text special-block info)))

(defun igo-org-js-add-html-attr-to-special-block (special-block)
  (let ((opt-json (igo-org-make-options-for-js_igo
                   (igo-org-get-special-block-header-options
                    special-block))))
    (if (not (string= opt-json ""))
        (org-element-put-property
         special-block :attr_html
         (list (concat (car (org-element-property :attr_html special-block))
                       " :data-igo-js t :data-igo-opt " opt-json))))))

(defun igo-org-modify-special-block-lazy-js (special-block info)
  (igo-org-replace-element-contents-to-html
   special-block
   (igo-org-instantiate-block-template
    (or (plist-get info :igo-lazy-js-template) "")
    special-block info)))

(defun igo-org-modify-special-block-svg (special-block info)
  (igo-org-replace-element-contents-to-html
   special-block
   (igo-org-create-svg-from-special-block special-block info)))

(defun igo-org-modify-special-block-custom (special-block info)
  (igo-org-replace-element-contents-to-html
   special-block
   (igo-org-instantiate-block-template
    (or (plist-get info :igo-block-template) "")
    special-block info)))


(defun igo-org-replace-element-contents-to-html (element html)
  "Replace the contents of ELEMENT to HTML."
  ;;@todo support replace option? (org-element-set-element element new-elem)
  (org-element-set-contents
   element
   ;; Wrap export block (#+begin_export html ~ #+end_export)
   (org-element-create 'export-block
                       (list :type "HTML"
                             :value html))))

;;
;; Template Instantiation
;;

(defun igo-org-instantiate-template (template vars)
  (replace-regexp-in-string
   "%[A-Z_]+%"
   (lambda (var-spec)
     (save-match-data
       (string-match "\\`%\\(\\(LITERAL\\|ATTR\\|HTML\\)_\\|\\)\\([A-Z_]+\\)%\\'" var-spec)
       (let* ((prefix (match-string 2 var-spec))
              (name (match-string 3 var-spec))
              (rep (cdr (assoc name vars))))
         (cond
          ((functionp rep) (igo-org-instantiate-template-escape prefix (funcall rep)))
          ((stringp rep) (igo-org-instantiate-template-escape prefix rep))
          (t var-spec)))))
   template
   t t))

(defun igo-org-instantiate-template-escape (type string)
  (save-match-data
    (cond
     ;; for HTML Attribute
     ((string= type "ATTR")
      (replace-regexp-in-string
       "\"" "&quot;"
       (org-html-encode-plain-text string)
       t t))
     ;; for JavaScript String Literal
     ((string= type "LITERAL")
      (dolist (pair '(("\\\\" . "\\\\")
                      ("\n" . "\\n")
                      ("\"" . "\\\"")
                      ("<!--" . "<\\!--")
                      ("<script" . "<\\script")
                      ("</script" . "<\\/script")))
        (setq string (replace-regexp-in-string (car pair) (cdr pair)
                                               string t t)))
      string)
     ;; for HTML Text
     ((string= type "HTML") (org-html-encode-plain-text string))

     ;; Raw Text
     (t string))))

;; Header Template

(defun igo-org-instantiate-header-template (template info)
  (igo-org-instantiate-template
   template
   (list
    (cons "PATH" (lambda () (or (plist-get info :igo-js-path) ""))))))

;; Block Template

(defun igo-org-instantiate-block-template (template special-block info)
  (igo-org-instantiate-template
   template
   (list
    (cons "PATH"
          (lambda ()
            (or (plist-get info :igo-js-path) "")))
    (cons "SGF"
          (lambda ()
            (or (igo-org-get-special-block-text special-block info) "")))
    (cons "OPT_JSON"
          (lambda ()
            (igo-org-make-options-for-js_igo
             (igo-org-get-special-block-header-options
              special-block))))
    (cons "SVG"
          (lambda ()
            (igo-org-create-svg-from-special-block special-block info)))
    )))


(defun igo-org-get-special-block-text (special-block info)
  "Return SGF string in SPECIAL-BLOCK."
  (org-export-data
   ;;@todo check plain text only
   (let* ((contents (org-element-contents special-block)))
     (if (and (eq (org-element-type (car contents)) 'paragraph)
              (null (cdr contents)))
         (org-element-contents (car contents))
       contents))
   info))

(defun igo-org-get-special-block-header (special-block)
  "Return header string in SPECIAL-BLOCK."
  (let* (;; #+begin_igo line
         (header-line
          (buffer-substring-no-properties
           (org-element-property :post-affiliated special-block)
           (org-element-property :contents-begin special-block)))
         ;; string after #+begin_igo
         (options-str
          (if (string-match
               "^[[:blank:]]*#\\+[A-Za-z]+_[A-Za-z]+[[:blank:]]*\\([^\n]*\\)"
               header-line)
              (match-string-no-properties 1 header-line)
            "")))
    options-str))

(defun igo-org-get-special-block-header-options (special-block)
  "Return alist specified by header line of SPECIAL-BLOCK."
  (org-babel-parse-header-arguments
   (igo-org-get-special-block-header special-block) t))

(defun igo-org-make-options-for-js_igo (options)
  (let ((json
         (concat
          "\"ui\": {\"top\":["
          (if (igo-org-opt-bool-value :status-bar options igo-editor-status-bar-visible)
              "\"GameStatus\"")
          "], \"bottom\":[[\"UndoAll\",\"Undo\",\"Redo\",\"RedoAll\",\"Pass\",\"Menu\"], \"ViewControl\", \"Comment\"]},"
          (if (igo-org-opt-bool-value :move-number options)
              "\"showMoveNumber\": true,")
          (if (igo-org-opt-bool-value :branch-text options "t")
              "\"showBranchText\": true,")
          (if (or (not (igo-org-opt-bool-value :editable options "t"))
                  (igo-org-opt-bool-value :read-only options))
              "\"editable\": false,")
          (if-let ((path (igo-org-opt-value :path options)))
              (concat
               "\"path\": ["
               (mapconcat
                (lambda (q)
                  (cond
                   ((numberp q) (number-to-string q))
                   ((string-match "\\`-?[0-9]+\\'" q) q)
                   (t (concat "\"" q "\""))))
                (igo-org-opt-split-path path)
                ",")
               "],")))))
    (if (string= json "") "" (concat "{" (substring json 0 -1) "}"))))

(defun igo-org-create-svg-from-special-block (special-block info)
  (when-let ((sgf (igo-org-get-special-block-text special-block info)))
    (let* ((sgf-tree (with-temp-buffer
                       (insert sgf)
                       (goto-char (point-min))
                       (igo-sgf-parse-tree (current-buffer))))
           (game-tree (igo-sgf-root-tree-to-game-tree sgf-tree))
           (size (igo-sgf-node-get-board-size
                  (igo-sgf-tree-root-node sgf-tree)))
           (game (igo-game (car size) (cdr size) game-tree))
           (board (igo-game-board game))
           (options (igo-org-get-special-block-header-options special-block))
           (board-view (igo-board-view board))
           (svg (svg-create (igo-board-view-pixel-w board-view)
                            (igo-board-view-pixel-h board-view))))

      ;; path
      (if-let ((queries (igo-org-opt-split-path (igo-org-opt-value :path options))))
          (igo-game-redo-by-queries game queries))

      ;;@todo support status bar?

      ;; board
      (igo-board-view-create-board board-view svg 0 0)

      ;; stones
      (igo-board-view-update-stones board-view svg board)

      ;; move numbers
      (igo-board-view-update-move-numbers
       board-view svg
       (igo-org-opt-bool-value :move-number options nil)
       game)

      ;; branch text
      (igo-board-view-update-branches
       board-view
       svg
       (igo-org-opt-bool-value :branch-text options t)
       board (igo-game-current-node game)
       nil nil nil)

      ;; last move
      (igo-board-view-update-last-move-mark
       board-view svg
       (and (igo-org-opt-bool-value :last-move options t)
            (not (igo-org-opt-bool-value :move-number options nil)))
       game)

      ;; marks
      (igo-board-view-update-marks board-view svg t game)

      ;; Create SVG text
      (with-temp-buffer
        (svg-print svg)
        (buffer-string)))))



;;
;; Apply Filters for Individual Files
;;
;; Usage:
;; 1. Add the following code to your org document.
;;    #+begin_src emacs-lisp :exports results :results none
;;    (require 'igo-org-export-html)
;;    (igo-org-set-html-filters-local)
;;    #+end_src
;; 2. Export as HTML (Eval source block)

(defun igo-org-set-html-filters-local ()
  "Set exporter functions and variables as buffer local.

To use this function, add the following code to your org document.
Then export as HTML.

#+begin_src emacs-lisp :exports results :results none
(igo-org-set-html-filters-local)
#+end_src
"
  (setq-local org-export-options-alist
              (append igo-org-js-options-alist org-export-options-alist))
  (setq-local org-export-filter-parse-tree-functions
              (append (list #'igo-org-html-filter-parse-tree)
                      org-export-filter-parse-tree-functions)))

;;
;; Derived from HTML Backend
;;
;; Usage:
;; 1. (igo-org-define-html-backend t)
;; 2. Type C-c C-e g in org-mode

(defun igo-org-define-html-backend (&optional add-to-menu-p)
  "Define a backend that transforms igo special blocks, derived from the HTML backend."
  (apply #'org-export-define-derived-backend
         'igo-html 'html
         :filters-alist '((:filter-parse-tree . igo-org-html-filter-parse-tree))
         :options-alist igo-org-js-options-alist

         (if add-to-menu-p
             '(
               :menu-entry
               (?g "Export to Go Game HTML"
                   ((?G "As HTML buffer" igo-org-export-as-html)
                    (?g "As HTML file" igo-org-export-to-html)
                    (?o "As HTML file and open"
                        (lambda (a s v b)
                          (if a (igo-org-export-to-html t s v b)
                            (org-open-file (igo-org-export-to-html nil s v b)))))))))))

;;;###autoload
(defun igo-org-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'igo-html "*Org Go Game HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun igo-org-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
                                    org-html-extension
                                    "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'igo-html file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun igo-org-publish-to-html (plist filename pub-dir)
  (org-publish-org-to 'igo-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))


;;
;; Modify HTML Backend Direct
;;
;; Usage:
;; 1. Add the following code to init.el:
;;    (with-eval-after-load "ox-html"
;;      (require 'igo-org-export-html)
;;      (igo-org-modify-html-backend))
;; 2. Export as HTML

(defun igo-org-modify-html-backend ()
  "Modify registered HTML backend."
  (require 'ox-html)
  (let ((backend (org-export-get-backend 'html)))
    ;; Add igo-org-js-options-alist to backend's options
    (let ((backend-options (org-export-backend-options backend))
          (new-option-names (mapcar #'car igo-org-js-options-alist)))
      (setf (org-export-backend-options backend)
            (nconc
             (seq-remove (lambda (elem) (memq (car elem) new-option-names))
                         backend-options)
             igo-org-js-options-alist)))
    ;; Add igo-org-html-filter-parse-tree to backend's filter-parse-tree
    (let ((filter-parse-tree (assq :filter-parse-tree
                                   (org-export-backend-filters backend))))
      ;; null => (:filter-parse-tree . ())
      (if (null filter-parse-tree)
          (push (setq filter-parse-tree (list :filter-parse-tree))
                (org-export-backend-filters backend)))
      ;; (:filter-parse-tree . function) => (:filter-parse-tree . (function))
      (if (functionp (cdr filter-parse-tree))
          (setcdr filter-parse-tree (list (cdr filter-parse-tree))))
      ;; Add my filter function
      (if (not (memq 'igo-org-html-filter-parse-tree (cdr filter-parse-tree)))
          (push 'igo-org-html-filter-parse-tree (cdr filter-parse-tree))))))


(provide 'igo-org-export-html)
;;; igo-org-export-html.el ends here
