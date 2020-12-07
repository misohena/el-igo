;;; igo-org.el --- SGF Editor for org-mode           -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'igo-editor)

(defface igo-org-error-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "SGF face for errors."
  :group 'igo-org-faces)


(defun igo-org-setup ()
  (interactive)
  (with-eval-after-load "org-src"
    (igo-org-hook-fontify-block)))

;;
;; Hook Fontify
;;

(defun igo-org-hook-fontify-block ()
  ;;(advice-add #'org-src-font-lock-fontify-block :around #'igo-org--fontify-src-block-advice)
  (advice-add #'org-fontify-meta-lines-and-blocks-1 :around #'igo-org--fontify-igo-block-advice)
  (advice-add #'org-unfontify-region :around #'igo-org--unfontify-region-advice))

(defun igo-org-unhook-fontify-block ()
  (advice-remove #'org-src-font-lock-fontify-block #'igo-org--fontify-src-block-advice)
  (advice-remove #'org-fontify-meta-lines-and-blocks-1 #'igo-org--fontify-igo-block-advice)
  (advice-remove #'org-unfontify-region #'igo-org--unfontify-region-advice))


;; Fontify

(defun igo-org-fontify-block (start end &optional options)
  ;; Skip leading&trailing line break
  ;; (#+begin_src sgf[start]\n ... \n[end]#+end_src)
  (if (and (< start end) (= (char-after start) ?\n))
      (setq start (1+ start)))
  (if (and (< start end) (= (char-before end) ?\n))
      (setq end (1- end)))

  (let ((editor (seq-some
                 (lambda (o) (overlay-get o 'igo-editor))
                 (overlays-in start end))))

    (if (null editor)
        ;; Create a new editor
        (setq editor (igo-org-create-editor start end options))
      ;; Cover region.
      (igo-editor-set-region editor start end)
      ;; Update editor state from region text.
      (igo-editor-update editor))

    ;; Highlight error place
    (if (igo-editor-last-error editor)
        (put-text-property (igo-editor-last-error-begin editor)
                           (igo-editor-last-error-end editor)
                           'face 'igo-org-error-face))))

(defun igo-org-create-editor (start end options-str)
  (let ((editor (igo-editor start end nil nil t))
        (options (org-babel-parse-header-arguments options-str t)))
    ;; Apply options to editor
    (dolist (opt options)
      (cond
       ((eq (car opt) :status-bar)
        (igo-editor-set-status-bar-visible editor (igo-org-opt-bool (cdr opt))))
       ((eq (car opt) :move-number)
        (igo-editor-set-move-number-visible editor (igo-org-opt-bool (cdr opt))))
       ((eq (car opt) :branch-text)
        (igo-editor-set-branch-text-visible editor (igo-org-opt-bool (cdr opt))))
       ))
    editor))

(defun igo-org-opt-bool (value)
  (null (member value '("no" "nil" nil))))

;; Fontify src block (#+begin_src sgf ~ #+end_src)

(defun igo-org--fontify-src-block-advice (old-func lang start end)
  (if (string= lang "sgf")
      (igo-org-fontify-block start end)
    (funcall old-func lang start end)))

;; Fontify igo block (#+begin_igo ~ #+end_igo)

(defun igo-org--fontify-igo-block-advice (old-func limit)
  (let* ((p (point))
         (org-protecting-blocks (cons "igo" org-protecting-blocks))
         ;; Fontify meta and blocks
         (ret-val (funcall old-func limit)))
    (if ret-val
        ;; Overwrite between #+begin_igo and #+end_igo if exists
        (save-excursion
          (goto-char p)
          (igo-org-fontify-igo-block limit)))
    ret-val))

(defun igo-org-fontify-igo-block (limit)
  "Fontify #+begin_igo block. Call after org-fontify-meta-lines-and-blocks-1. Overwrite text property between #+begin_igo and #+end_igo."

  ;; The following code is derived from org-fontify-meta-lines-and-blocks-1
  (let ((case-fold-search t))
    (when (re-search-forward
	   (rx bol (group (zero-or-more blank) "#"
			  (group (group (or (seq "+" (one-or-more (any "a-zA-Z")) (optional ":"))
					    space
					    eol))
				 (optional (group "_" (group (one-or-more (any "a-zA-Z"))))))
			  (zero-or-more blank)
                          ;; options
			  (group (zero-or-more any))))
	   limit t)
      (let ((block-start (match-end 0))  ; includes the \n at end of #+begin line
	    (block-end nil)              ; will include \n after end of block content
	    (dc3 (downcase (match-string 3)))
            (options (match-string 6))
	     block-type)
	(cond
	 ((and (match-end 4) (equal dc3 "+begin"))
	  ;; Truly a block
	  (setq block-type (downcase (match-string 5)))
	  (when (re-search-forward
		 (rx-to-string `(group bol (or (seq (one-or-more "*") space)
					       (seq (zero-or-more blank)
						    "#+end"
						    ,(match-string 4)
						    word-end
						    (zero-or-more any)))))
		 nil t)  ;; on purpose, we look further than LIMIT
	    ;; We do have a matching #+end line
	    (setq block-end (match-beginning 0)) ; includes the final newline.

            ;;
            ;; Fontify begin_igo block
            ;;
            (when (string= block-type "igo")
              (igo-org-fontify-block block-start block-end options)
              t))))))))

;; Unfontify

(defun igo-org--unfontify-region-advice (old-func beg end &optional _maybe_loudly)
  (funcall old-func beg end _maybe_loudly)
  (igo-org-unfontify-region beg end))

(defun igo-org-unfontify-region (beg end)
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'igo-editor)
      ;; keep overlay if src block still exists
      ;; (surrounded by begin_src and end_src)
      (when (not
             (and
              (save-excursion
                (goto-char (overlay-start ov))
                (forward-line -1)
                (looking-at "[ \t]*#\\+begin_\\(src +sgf\\|igo\\)"))
              (save-excursion
                (goto-char (overlay-end ov))
                (forward-line 1)
                (looking-at "[ \t]*#\\+end_\\(src\\|igo\\)"))))
        ;;(message "delete overlay %s" ov)
        (delete-overlay ov)))))


(provide 'igo-org)
;;; igo-org.el ends here
