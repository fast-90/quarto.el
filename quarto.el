;;; quarto.el --- Package to use quarto within emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Duy Nguyen

;; Author: Duy Nguyen <duynguyen@gmx.com>
;; Maintainer: Duy Nguyen <duynguyen@gmx.com>
;; Created: 19 Jun 2023
;; License: GPL-3.0-or-later
;; package-requires: ((emacs "25.1") (polymode "0.2.2") (poly-markdown "0.2.2") (markdown-mode "2.3") (request "0.3.2"))
;; Keywords: quarto
;; URL: https://github.com/fast-90/quarto.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


(require 'polymode)
(require 'markdown-mode)
(require 'poly-markdown)
(require 'shell)

;;;###autoload (autoload 'poly-quarto-mode "quarto")
(define-polymode poly-quarto-mode poly-markdown-mode
  "Minor mode for editing quarto files."
  :lighter " Quarto"
  :keymap nil
  :innermodes '(:inherit poly-markdown-inline-code-innermode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))

(defvar quarto-preview-url nil
  "URL of the quarto preview server.")

;;;###autoload (autoload 'quarto-eval-block "quarto")
(defalias 'quarto-eval-block #'polymode-eval-chunk
    "Evaluate code block.")

(defun quarto-eval-block-and-next ()
  "Evaluate current current block and move to next."
  (interactive)
  (progn
    (call-interactively #'quarto-eval-block)
    (quarto-next-code-block)))

(defun quarto-eval-all-blocks-below ()
  "Evaluate all blocks below point."
  (interactive)
  (let ((current (save-excursion (point))))
    (while (progn
             (quarto-next-code-block)
             (call-interactively #'quarto-eval-block)
             (search-forward-regexp "^```{.*?}$" nil t)))
    (goto-char current)))

(defun quarto-eval-all-blocks ()
  "Evaluate all blocks in current file."
  (interactive)
  (let ((current (save-excursion (point))))
    (goto-char (point-min))
    (quarto-eval-all-blocks-below)
    (goto-char current)))

(defvar-keymap quarto-code-block-repeat-map
  :repeat t
  "n" #'quarto-next-code-block
  "p" #'quarto-previous-code-block
  "e" #'quarto-eval-block
  "E" #'quarto-eval-block-and-next
  "a" #'quarto-eval-all-blocks-below
  "A" #'quarto-eval-all-blocks)

(keymap-set poly-quarto-mode-map "M-n M-n" #'quarto-next-code-block)
(keymap-set poly-quarto-mode-map "M-n M-p" #'quarto-previous-code-block)
(keymap-set poly-quarto-mode-map "M-n M-e" #'quarto-eval-block)
 
(defun quarto-preview ()
  "Start a quarto preview server."
  (interactive)
  (let* ((shell-command-switch "-ic")
         (process (quarto-preview--start-process)))
    (set-process-filter process #'quarto--output-filter)
    (set-process-sentinel process #'quarto--process-sentinel)))

(defun quarto-preview--start-process ()
  "Start the quarto preview process."
  (message "Starting quarto preview server...")
  (start-process-shell-command
   "quarto-preview"
   "*quarto-preview*"
   (concat "quarto preview " buffer-file-name " --no-browser")))

(defun quarto-preview--check-output (output)
  "Check whether the preview server is ready via Regex."
  (string-match "\\(Browse at.*\\)\\(https?://.+:[0-9]\\{4\\}\\)" output))

(defun quarto--output-filter (process output)
  "Process filter to check whether the quarto preview server is ready."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert output))
    (when (quarto-preview--check-output output)
      (setq quarto-preview-url (match-string 2 output))
      (message "Quarto preview server ready!")
      ;; Remove the xwidget filter if it was added in different functions.
      (advice-remove 'quarto--output-filter #'quarto--open-preview-xwidget-filter))))

(defun quarto--process-sentinel (process event)
  "Process sentinel to handle Quarto server events.

Unsets `quarto-preview-url'."
  (when (memq (process-status process) '(exit signal))
    (setq quarto-preview-url nil)
    (message "Quarto preview process killed.")))

(defun quarto--open-preview-xwidget-filter (process output)
  "Process filter to open the preview file in xwidget if the server is ready."
  (when (quarto-preview--check-output output)
    (let ((previous-buffer (buffer-name)))
      (xwidget-webkit-browse-url quarto-preview-url)
      (switch-to-buffer-other-window previous-buffer))))

(defun quarto-preview-file ()
  "Open the preview file with xwidget-webkit."
  (interactive)
  (advice-add 'quarto--output-filter :after #'quarto--open-preview-xwidget-filter)
  (quarto-preview))

(defun quarto-next-code-block ()
  "Move the cursor to the beginning of the line under the next code block."
  (interactive)
  (goto-char (line-beginning-position))
  (search-forward-regexp "^```{.*}$" nil t)
  (forward-line))

(defun quarto-previous-code-block ()
  "Move the cursor to the beginning of the line under the next code block."
  (interactive)
  (search-backward-regexp "^```$" nil t)
  (forward-line -1)
  (search-backward "```{" nil t)
  (forward-line))

(provide 'quarto)

;;; quarto.el ends here
