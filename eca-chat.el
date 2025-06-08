;;; eca-chat.el --- ECA (Editor Code Assistant) chat -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) chat.
;;
;;; Code:

(require 'eca-api)

;; Variables

(defcustom eca-chat-mode-hook '()
  "Hooks to run after entering in eca chat mode hook."
  :type 'hook
  :group 'eca)

(defcustom eca-chat-window-width 50
  "The width of `eca' dedicated chat window."
  :type 'integer
  :group 'eca)

(defcustom eca-chat-prompt-prefix "​> "
  "The prompt string used in eca chat buffer."
  :type 'string
  :group 'eca)

;; Internal

(defvar-local eca--chat-history '(""))
(defvar-local eca--chat-history-index 0)

(defvar eca-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") #'eca-chat--backward-delete-char)
    (define-key map (kbd "DEL") #'eca-chat--backward-delete-char)
    (define-key map (kbd "S-<return>") #'eca-chat--send-newline)
    (define-key map (kbd "<return>") #'eca-chat--send-return)
    ;; TODO
    map)
  "Keymap used by `eca-chat-mode'.")

(defun eca-chat--send-newline ()
  "Insert a newline character at point."
  (interactive)
  (insert "\n"))

(defun eca-chat--backward-delete-char ()
  "Delete the character before point, unless at the prompt boundary.
This is similar to `backward-delete-char' but protects the prompt line."
  (interactive)
  (let ((prompt-start (save-excursion
                        (goto-char (line-beginning-position))
                        (when (looking-at (regexp-quote eca-chat-prompt-prefix))
                          (point)))))
    (if (and prompt-start
             (<= (point) (+ prompt-start (length eca-chat-prompt-prefix))))
        (ding)
      (delete-char -1))))

(defun eca-chat--send-return ()
  "Send the current prompt to eca process if in prompt."
  (interactive)
  (let ((prompt (save-excursion
                  (goto-char (point-max))
                  (search-backward-regexp (concat "^" eca-chat-prompt-prefix) nil t)
                  (forward-char (length eca-chat-prompt-prefix))
                  (string-trim (buffer-substring-no-properties (point) (point-max))))))
    (unless (string-empty-p prompt)
      (setcdr eca--chat-history (cons prompt (cdr eca--chat-history)))
      (setq eca--chat-history-index 0)

      (goto-char (point-max)) ;; Go to end before searching back
      (when (search-backward-regexp (concat "^" (regexp-quote eca-chat-prompt-prefix)) nil t)
        (forward-char (length eca-chat-prompt-prefix))
        (delete-region (point) (point-max)))
      (eca-api-send prompt))))

(define-derived-mode eca-chat-mode fundamental-mode "eca-chat"
  "Major mode for ECA chat sessions.
\\{eca-chat-mode-map}"
  :group 'eca
  (setq major-mode 'eca-chat-mode)
  (setq mode-name "eca-chat")
  (use-local-map eca-chat-mode-map)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward-regexp (concat "^" eca-chat-prompt-prefix) nil t)
      (insert (propertize (concat "\n\n" eca-chat-prompt-prefix) 'face font-lock-keyword-face))))
  (goto-char (point-max))
  (run-hooks 'eca-chat-mode-hook))

(defun eca--chat-get-or-create-buffer (session)
  "Get the eca chat buffer for current project for SESSION."
  (get-buffer-create (concat "*eca:" session "*")))

(defun eca--chat-select-window ()
  "Select the Window."
  (select-window (get-buffer-window (buffer-name)))
  (set-window-dedicated-p (selected-window) t))

(defun eca--chat-pop-window ()
  "Pop eca dedicated window if it exists."
  (display-buffer (buffer-name) `(display-buffer-in-side-window (side . right) (window-width . ,eca-chat-window-width)))
  (select-window (get-buffer-window (buffer-name)))
  (set-window-buffer (get-buffer-window (buffer-name)) (buffer-name))
  (set-window-dedicated-p (selected-window) t))

;; Public

(defun eca-chat-open (session)
  "Open or create dedicated eca chat window for SESSION."
  (with-current-buffer (eca--chat-get-or-create-buffer session)
    (setq-local eca--session-name session)
    (eca-chat-mode)
    (if (window-live-p (get-buffer-window (buffer-name)))
        (eca--chat-select-window)
      (eca--chat-pop-window))))

(provide 'eca-chat)
;;; eca-chat.el ends here
