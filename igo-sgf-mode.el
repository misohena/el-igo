;;; igo-sgf-mode.el --- SGF Editing Mode  -*- lexical-binding: t; -*-

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

;; (add-to-list 'auto-mode-alist '("\\.sgf$" . igo-sgf-mode))

;;; Code:

(require 'igo-editor)

(setq igo-sgf-mode-font-lock-keywords
      '((igo-sgf-mode-fontify)))

(defun igo-sgf-mode ()
  (interactive)

  (setq-local igo-sgf-mode-editor (igo-editor (point-min) (point-max) nil nil t))
  (igo-editor-update igo-sgf-mode-editor)

  ;;(use-local-map igo-sgf-mode-map)
  (setq-local mode-name "SGF(Go Game)")
  (setq-local major-mode 'igo-sgf-mode)

  (setq-local font-lock-defaults '(igo-sgf-mode-font-lock-keywords t))

  (run-mode-hooks 'igo-sgf-mode-hook))


(defun igo-sgf-mode-fontify (limit)
  (let ((editor igo-sgf-mode-editor))
    ;; Cover whole buffer
    (igo-editor-set-region editor (point-min) (point-max))
    ;; Update editor state from buffer text.
    (igo-editor-update editor)
    ;; Highlight error place
    (if (igo-editor-last-error editor)
        (put-text-property (igo-editor-last-error-begin editor)
                           (igo-editor-last-error-end editor)
                           'face 'igo-org-error-face))))

(defun igo-sgf-mode-fontify-buffer ()
  (save-excursion
    (goto-char (point-min))
    (igo-sgf-mode-fontify (point-max))))

(provide 'igo-sgf-mode)
;;; igo-sgf-mode.el ends here
