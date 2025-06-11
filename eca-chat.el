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

(require 'eca-util)
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

(defcustom eca-chat-prompt-prefix "​>"
  "The prompt prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-prompt-separator " "
  "The prompt sufix used in eca chat buffer."
  :type 'string
  :group 'eca)

(defface eca-chat-prompt-prefix-face
  '((t (:foreground "green" :weight bold)))
  "Face for the `eca-chat-prompt-prefix`.")

;; Internal

(defvar-local eca--chat-history '(""))
(defvar-local eca--chat-history-index 0)

(defun eca-chat--prompt-prefix ()
  "The full prompt prefix"
  (concat eca-chat-prompt-prefix eca-chat-prompt-separator))

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
                        (when (looking-at (regexp-quote (eca-chat--prompt-prefix)))
                          (point)))))
    (if (and prompt-start
             (<= (point) (+ prompt-start (length (eca-chat--prompt-prefix)))))
        (ding)
      (delete-char -1))))

(defun eca-chat--send-return ()
  "Send the current prompt to eca process if in prompt."
  (interactive)
  (let ((prompt (save-excursion
                  (goto-char (point-max))
                  (search-backward-regexp (concat "^" (eca-chat--prompt-prefix)) nil t)
                  (forward-char (length (eca-chat--prompt-prefix)))
                  (string-trim (buffer-substring-no-properties (point) (point-max))))))
    (unless (string-empty-p prompt)
      (setcdr eca--chat-history (cons prompt (cdr eca--chat-history)))
      (setq eca--chat-history-index 0)

      (goto-char (point-max)) ;; Go to end before searching back
      (when (search-backward-regexp (concat "^" (regexp-quote (eca-chat--prompt-prefix))) nil t)
        (forward-char (length (eca-chat--prompt-prefix)))
        (delete-region (point) (point-max)))
      ;; TODO
      (eca-api-request-async :method "chat/prompt"
                             :params (list :message prompt)))))

(define-derived-mode eca-chat-mode fundamental-mode "eca-chat"
  "Major mode for ECA chat sessions.
\\{eca-chat-mode-map}"
  :group 'eca
  (setq major-mode 'eca-chat-mode)
  (setq mode-name "eca-chat")
  (use-local-map eca-chat-mode-map)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward-regexp (concat "^" (eca-chat--prompt-prefix)) nil t)
      (insert "\n\n")
      (insert (propertize eca-chat-prompt-prefix 'face 'eca-chat-prompt-prefix-face))
      (insert (propertize eca-chat-prompt-separator 'face nil))))
  (goto-char (point-max))
  (run-hooks 'eca-chat-mode-hook))

(defun eca--chat-get-or-create-buffer ()
  "Get the eca chat buffer for current project."
  (get-buffer-create "*eca-chat*"))

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

(defun eca-chat-open ()
  "Open or create dedicated eca chat window."
  (with-current-buffer (eca--chat-get-or-create-buffer)
    (unless (eca--session-chat eca--session)
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (setf (eca--session-chat eca--session) (current-buffer)))
    (eca-chat-mode)
    (if (window-live-p (get-buffer-window (buffer-name)))
        (eca--chat-select-window)
      (eca--chat-pop-window))))

(defun eca-chat-exit ()
  "Exit the ECA chat."
  (with-current-buffer (eca--chat-get-or-create-buffer)
    (goto-char (point-max))
    (insert "*Closed session*")
    (read-only-mode)
    (rename-buffer (concat (buffer-name) ":closed"))))

(provide 'eca-chat)
;;; eca-chat.el ends here
