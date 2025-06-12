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

(require 'f)

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

(defcustom eca-chat-prompt-prefix "> "
  "The prompt prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-context-prefix "@"
  "The context prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-model nil
  "Which model to use during chat, nil means auto, let server decide.
Must be a valid model supported by server."
  :type 'string
  :group 'eca)

(defface eca-chat-prompt-prefix-face
  '((t (:foreground "green" :weight bold)))
  "Face for the `eca-chat-prompt-prefix`."
  :group 'eca)

(defface eca-chat-context-face
  '((t (:foreground "cyan" :underline t)))
  "Face for the `eca-chat-context-prefix`."
  :group 'eca)

(defface eca-chat-user-messages-face
  '((t :inherit font-lock-doc-face))
  "Face for the user sent messages in chat."
  :group 'eca)

(defface eca-chat-system-messages-face
  '((t :inherit font-lock-comment-face))
  "Face for the system messages in chat."
  :group 'eca)

(defface eca-chat-welcome-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for the welcome message in chat."
  :group 'eca)

(defface eca-chat-option-key-face
  '((t :inherit font-lock-doc-face))
  "Face for the option keys in header-line of the chat."
  :group 'eca)

(defface eca-chat-option-value-face
  '((t :weight bold))
  "Face for the option values in header-line of the chat."
  :group 'eca)

;; Internal

(defvar-local eca-chat--history '())
(defvar-local eca-chat--history-index 0)
(defvar-local eca-chat--id nil)
(defvar-local eca-chat--context '())

(defun eca-chat--insert-prompt-string ()
  ""
  (let ((prompt-area-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-area-ov 'eca-chat-prompt-area t))
  (let ((prompt-context-area-ov (make-overlay (line-beginning-position) (line-end-position) (current-buffer))))
    (overlay-put prompt-context-area-ov 'eca-chat-context-area t))
  (insert (propertize eca-chat-context-prefix 'face 'eca-chat-context-face))
  (insert "\n")
  (let ((prompt-field-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-field-ov 'eca-chat-prompt-field t)
    (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'face 'eca-chat-prompt-prefix-face))))

(defun eca-chat--clear ()
  "Clear the chat."
  (with-current-buffer (eca--chat-get-or-create-buffer)
    (delete-region (point-min) (point-max))
    (remove-overlays (point-min) (point-max))
    (insert "\n")
    (eca-chat--insert-prompt-string)))

(defvar eca-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") #'eca-chat--backward-delete-char)
    (define-key map (kbd "DEL") #'eca-chat--backward-delete-char)
    (define-key map (kbd "S-<return>") #'eca-chat--send-newline)
    (define-key map (kbd "S-<return>") #'eca-chat--send-newline)
    (define-key map (kbd "<return>") #'eca-chat--send-return)
    map)
  "Keymap used by `eca-chat-mode'.")

(defun eca-chat--send-newline ()
  "Insert a newline character at point."
  (interactive)
  (when (eq (line-beginning-position) (eca-chat--prompt-field-start-point))
    (insert "\n")))

(defun eca-chat--prompt-field-start-point ()
  ""
  (overlay-start
   (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-field)))
           (overlays-in (point-min) (point-max)))))

(defun eca-chat--prompt-area-start-point ()
  ""
  (overlay-start
   (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-area)))
           (overlays-in (point-min) (point-max)))))

(defun eca-chat--backward-delete-char ()
  "Delete the character before point, unless at the prompt boundary.
This is similar to `backward-delete-char' but protects the prompt line."
  (interactive)
  (let ((cur-ov (car (overlays-in (line-beginning-position) (point)))))
    (if (and cur-ov
             (<= (point) (overlay-start cur-ov)))
        (ding)
      (delete-char -1))))

(defun eca-chat--send-return ()
  "Send the current prompt to eca process if in prompt."
  (interactive)
  (let* ((prompt-start (eca-chat--prompt-field-start-point))
         (prompt (save-excursion
                   (goto-char prompt-start)
                   (string-trim (buffer-substring-no-properties (point) (point-max))))))
    (unless (string-empty-p prompt)
      (when (seq-empty-p eca-chat--history) (eca-chat--clear))
      (add-to-list 'eca-chat--history prompt)
      (setq eca-chat--history-index 0)
      (goto-char prompt-start)
      (delete-region (point) (point-max))
      (eca-api-request-async
       :method "chat/prompt"
       :params (list :message prompt
                     :chatId eca-chat--id
                     :model (when eca-chat-model eca-chat-model))
       :success-callback (-lambda (res)
                           (setq-local eca-chat--id (plist-get res :chatId)))))))

(defun eca-chat--header-line--string ()
  "Update chat header line."
  (let ((model-str (or eca-chat-model "auto")))
    (list (propertize "model:" 'face 'eca-chat-option-key-face)
          (propertize model-str 'face 'eca-chat-option-value-face)
          "  ")))

(define-derived-mode eca-chat-mode fundamental-mode "eca-chat"
  "Major mode for ECA chat sessions.
\\{eca-chat-mode-map}"
  :group 'eca
  (setq major-mode 'eca-chat-mode)
  (setq mode-name "eca-chat")
  (setq-local mode-line-format '())
  (visual-line-mode)
  (use-local-map eca-chat-mode-map)
  (setq-local eca-chat--history '())
  (setq-local eca-chat--history-index 0)
  (unless (listp header-line-format)
    (setq-local header-line-format (list header-line-format)))
  (add-to-list 'header-line-format '(t (:eval (eca-chat--header-line--string))))
  (when (eq 0 (length (string-trim (buffer-string))))
    (save-excursion
      (goto-char (point-min))
      (insert "\n")
      (insert (propertize (eca--session-chat-welcome-message eca--session)
                          'face 'eca-chat-welcome-face))
      (eca-chat--insert-prompt-string)))
  (goto-char (point-max))
  (run-hooks 'eca-chat-mode-hook))

(defun eca--chat-get-or-create-buffer ()
  "Get the eca chat buffer for current project."
  (get-buffer-create "*eca-chat*"))

(defun eca--chat-select-window ()
  "Select the Window."
  (select-window (get-buffer-window (buffer-name))))

(defun eca--chat-pop-window ()
  "Pop eca dedicated window if it exists."
  (let ((buffer (current-buffer)))
    (display-buffer buffer
                   '((display-buffer-in-side-window)
                     (side . right)
                     (window-width . ,eca-chat-window-width)))
    (select-window (get-buffer-window buffer))
    (set-window-buffer (get-buffer-window buffer) buffer)))

(defun eca--chat-add-content (content)
  "Add CONTENT to the chat."
  (with-current-buffer (eca--chat-get-or-create-buffer)
    (let ((context-start (eca-chat--prompt-area-start-point)))
      (save-excursion
        (goto-char context-start)
        (goto-char (1- (point)))
        (insert content)))))

(defun eca-chat--refresh-context ()
  ""
  (save-excursion
    (goto-char
     (overlay-start
      (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-context-area)))
              (overlays-in (point-min) (point-max)))))
    (delete-region (point) (line-end-position))
    (seq-doseq (context eca-chat--context)
      (pcase (plist-get context :type)
        ('file (insert (propertize (concat "@" (f-filename (plist-get context :value))) 'face 'eca-chat-context-face)))))
    (insert " ")
    (insert (propertize "@" 'face 'eca-chat-context-face))))

(defun eca-chat--add-context (type value)
  "Add to chat context VALUE of TYPE."
  (pcase type
    ('file (progn
             (add-to-list 'eca-chat--context (list :type 'file :value value))
             (eca-chat--refresh-context)))))

;; Public

(defun eca-chat-content-received (params)
  "..."
  (let* ((role (plist-get params :role))
         (is-complete? (plist-get params :isComplete))
         (content (plist-get params :content))
         (type (plist-get content :type))
         (text (plist-get content :text)))
    (pcase type
      ("text" (progn
                (when is-complete?
                  (eca--chat-add-content (propertize "\n" 'line-spacing 10))
                  (setq-local mode-line-format '()))
                (when text
                  (eca--chat-add-content
                   (pcase role
                     ("user" (propertize text
                                         'face 'eca-chat-user-messages-face
                                         'line-prefix (propertize eca-chat-prompt-prefix 'face 'eca-chat-user-messages-face)
                                         'line-spacing 10))
                     (_ (propertize text
                                    'line-height 20)))))))
      ("temporary-text" (setq-local mode-line-format `(,(propertize text 'face 'eca-chat-system-messages-face)))))))

(defun eca-chat-open ()
  "Open or create dedicated eca chat window."
  (let ((source-buffer (current-buffer)))
    (with-current-buffer (eca--chat-get-or-create-buffer)
      (unless (eca--session-chat eca--session)
        (read-only-mode -1)
        (delete-region (point-min) (point-max))
        (setf (eca--session-chat eca--session) (current-buffer)))
      (eca-chat-mode)
      (eca-chat--add-context 'file (buffer-file-name source-buffer))
      (if (window-live-p (get-buffer-window (buffer-name)))
          (eca--chat-select-window)
        (eca--chat-pop-window)))))

(defun eca-chat-exit ()
  "Exit the ECA chat."
  (with-current-buffer (eca--chat-get-or-create-buffer)
    (goto-char (point-max))
    (insert "*Closed session*")
    (read-only-mode)
    (setq-local eca-chat--id nil)
    (rename-buffer (concat (buffer-name) ":closed"))))

;;;###autoload
(defun eca-chat-clear ()
  "Clear the eca chat."
  (interactive)
  (eca-chat--clear))

(provide 'eca-chat)
;;; eca-chat.el ends here
