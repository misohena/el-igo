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

(defvar-local igo-sgf-mode-editor nil)

(defun igo-sgf-mode ()
  "Major mode for editing SGF files.

The following commands are available:

\\{igo-sgf-mode-map}"
  (interactive)

  ;; Create new igo-editor
  (setq igo-sgf-mode-editor (igo-editor (point-min) (point-max) nil nil t))
  (igo-sgf-mode-transfer-overlay-keymap-to-local-map igo-sgf-mode-editor)
  (igo-sgf-mode-track-buffer-read-only igo-sgf-mode-editor)

  ;; Set major mode
  (setq mode-name "SGF(Go Game)")
  (setq major-mode 'igo-sgf-mode)
  (setq font-lock-defaults '(igo-sgf-mode-font-lock-keywords t))


  (run-mode-hooks 'igo-sgf-mode-hook))

;; Font Lock

(defvar igo-sgf-mode-font-lock-keywords
  '((igo-sgf-mode-fontify)))

(defun igo-sgf-mode-fontify (_limit)
  (let ((editor igo-sgf-mode-editor))
    ;; Cover whole buffer
    (igo-editor-set-region editor (point-min) (point-max))
    ;; Update editor state from buffer text.
    (igo-editor-update editor)
    ;; Set font-lock properties
    (add-text-properties (point-min) (point-max)
                         '(font-lock-fontified t font-lock-multiline t))
    ;; Highlight error place
    (if (igo-editor-last-error editor)
        (put-text-property (igo-editor-last-error-begin editor)
                           (igo-editor-last-error-end editor)
                           'face 'igo-org-error-face))))

(defun igo-sgf-mode-fontify-buffer ()
  (save-excursion
    (goto-char (point-min))
    (igo-sgf-mode-fontify (point-max))))


;; Keymap

(defvar-local igo-sgf-mode-map nil)

(defun igo-sgf-mode-transfer-overlay-keymap-to-local-map (editor)
  (let* ((ov (igo-editor-overlay editor))
         (keymap (overlay-get ov 'keymap)))
    ;; Transfer overlay's keymap property to local-map.
    (setq-local igo-sgf-mode-map keymap)
    (use-local-map keymap)
    (overlay-put ov 'keymap nil) ;; remove keymap property
    ;; Track keymap changes.
    (igo-editor-add-hook editor 'keymap-change #'igo-sgf-mode-on-keymap-change)

    editor))

(defun igo-sgf-mode-on-keymap-change (_editor keymap)
  (setq-local igo-sgf-mode-map keymap)
  (use-local-map keymap)
  ;; Return t. EDITOR does not change the overlay's keymap.
  t)

;; Read Only

(defun igo-sgf-mode-track-buffer-read-only (editor)
  (igo-sgf-mode-update-buffer-read-only editor)
  (igo-editor-add-hook editor 'text-mode
                       #'igo-sgf-mode-update-buffer-read-only)
  (igo-editor-add-hook editor 'graphical-mode
                       #'igo-sgf-mode-update-buffer-read-only))

(defun igo-sgf-mode-update-buffer-read-only (editor)
  (setq buffer-read-only (not (igo-editor-text-mode-p editor))))



(provide 'igo-sgf-mode)
;;; igo-sgf-mode.el ends here
