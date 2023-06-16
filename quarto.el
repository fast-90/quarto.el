;;; quarto.el --- Support for the Foo programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Your Name

;; Author: Your Name <yourname@example.com>
;; Maintainer: Someone Else <someone@example.com>
;; Created: 14 Jul 2010
;; package-requires: ((emacs "25.1") (polymode "0.2.2") (poly-markdown "0.2.2") (markdown-mode "2.3") (request "0.3.2"))
;; Keywords: languages
;; URL: https://example.com/foo

;; This file is not part of GNU Emacs.

;; This file is free software…

;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

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
