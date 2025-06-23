;;; eca-chat.el --- ECA (Editor Code Assistant) chat -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) chat.
;;
;;; Code:

(require 'f)
(require 'markdown-mode)
(require 'compat)

(require 'eca-util)
(require 'eca-api)

;; Variables

(defcustom eca-chat-mode-hook '()
  "Hooks to run after entering in eca chat mode hook."
  :type 'hook
  :group 'eca)

(defcustom eca-chat-window-width 0.35
  "The width of `eca' dedicated chat window."
  :type 'integer
  :group 'eca)

(defcustom eca-chat-position-params `((display-buffer-in-side-window)
                                      (side . right)
                                      (window-width . ,eca-chat-window-width))
  "Position params for each chat display."
  :type 'alist
  :group 'eca)

(defcustom eca-chat-prompt-prefix "> "
  "The prompt prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-context-prefix "@"
  "The context prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-custom-model nil
  "Which model to use during chat, nil means use server's default.
Must be a valid model supported by server, check `eca-chat-select-model`."
  :type 'string
  :group 'eca)

(defcustom eca-chat-custom-behavior nil
  "Which chat behavior to use, if nil use server's default."
  :type 'string
  :group 'eca)

(defface eca-chat-prompt-prefix-face
  '((t (:foreground "lime green" :weight bold)))
  "Face for the `eca-chat-prompt-prefix`."
  :group 'eca)

(defface eca-chat-context-unlinked-face
  '((t (:foreground "gold")))
  "Face for the `eca-chat-context-prefix`."
  :group 'eca)

(defface eca-chat-context-linked-face
  '((t (:inherit eca-chat-context-unlinked-face :underline t)))
  "Face for the `eca-chat-context-prefix`."
  :group 'eca)

(defface eca-chat-user-messages-face
  '((t :inherit font-lock-doc-face))
  "Face for the user sent messages in chat."
  :group 'eca)

(defface eca-chat-system-messages-face
  '((t :inherit font-lock-builtin-face))
  "Face for the system messages in chat."
  :group 'eca)

(defface eca-chat-mcp-tool-call-face
  '((t :inherit font-lock-keyword-face))
  "Face for the MCP tool calls in chat."
  :group 'eca)

(defface eca-chat-mcp-tool-call-name-face
  '((t :inherit eca-chat-mcp-tool-call-face :underline t))
  "Face for the MCP tool calls's name in chat."
  :group 'eca)

(defface eca-chat-welcome-face
  '((t :inherit font-lock-builtin-face))
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
(defvar-local eca-chat--last-request-id 0)
(defvar-local eca-chat--context '())
(defvar-local eca-chat--spinner-string "")
(defvar-local eca-chat--spinner-timer nil)
(defvar-local eca-chat--progress-text "")

(defvar eca-chat-buffer-name "<eca-chat>")

(defvar eca-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") #'eca-chat--backward-delete-char)
    (define-key map (kbd "DEL") #'eca-chat--backward-delete-char)
    (define-key map (kbd "S-<return>") #'eca-chat--send-newline)
    (define-key map (kbd "S-<return>") #'eca-chat--send-newline)
    (define-key map (kbd "C-k") #'eca-chat-clear)
    (define-key map (kbd "<return>") #'eca-chat--send-return)
    map)
  "Keymap used by `eca-chat-mode'.")

(defun eca-chat--spinner-start ()
  "Start modeline spinner."
  (setq eca-chat--spinner-timer
        (run-with-timer
         0
         0.5
         (lambda ()
           (when eca-chat--spinner-timer
             (with-current-buffer (eca-chat--get-buffer)
               (if (eq 3 (length eca-chat--spinner-string))
                   (setq eca-chat--spinner-string ".")
                 (setq eca-chat--spinner-string (concat eca-chat--spinner-string ".")))
               (force-mode-line-update)))))))

(defun eca-chat--spinner-stop ()
  "Stop modeline spinner."
  (when eca-chat--spinner-timer
    (cancel-timer eca-chat--spinner-timer)
    (setq eca-chat--spinner-timer nil))
  (setq eca-chat--spinner-string ""))

(defun eca-chat--behavior ()
  "The chat behavior considering what's in session and user option."
  (or eca-chat-custom-behavior
      (eca--session-chat-default-behavior eca--session)))

(defun eca-chat--model ()
  "The chat model considering what's in session and user option."
  (or eca-chat-custom-model
      (eca--session-chat-default-model eca--session)))

(defun eca-chat--insert-prompt-string ()
  "Insert the prompt and context string adding overlay metadatas."
  (let ((prompt-area-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-area-ov 'eca-chat-prompt-area t))
  (let ((prompt-context-area-ov (make-overlay (line-beginning-position) (line-end-position) (current-buffer))))
    (overlay-put prompt-context-area-ov 'eca-chat-context-area t))
  (insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))
  (insert "\n")
  (let ((prompt-field-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-field-ov 'eca-chat-prompt-field t)
    (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-prompt-prefix-face))))

(defun eca-chat--clear ()
  "Clear the chat."
  (with-current-buffer (eca-chat--get-buffer)
    (erase-buffer)
    (remove-overlays (point-min) (point-max))
    (insert "\n")
    (eca-chat--insert-prompt-string)
    (eca-chat--refresh-context)))

(defun eca-chat--send-newline ()
  "Insert a newline character at point."
  (interactive)
  (when (eq (line-beginning-position) (eca-chat--prompt-field-start-point))
    (insert "\n")))

(defun eca-chat--prompt-field-start-point ()
  "Return the metadata overlay for the prompt field start point."
  (overlay-start
   (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-field)))
           (overlays-in (point-min) (point-max)))))

(defun eca-chat--prompt-area-start-point ()
  "Return the metadata overlay for the prompt area start point."
  (-some->
      (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-area)))
              (overlays-in (point-min) (point-max)))
    (overlay-start)))

(defun eca-chat--backward-delete-char ()
  "Delete the character before point, unless at the prompt or context boundary.
Checks if it's in a context, removing it if so.
This is similar to `backward-delete-char' but protects the prompt/context line."
  (interactive)
  (let* ((cur-ov (car (overlays-in (line-beginning-position) (point))))
         (text (thing-at-point 'symbol))
         (context-item (-some->> text
                                 (get-text-property 0 'eca-chat-context-item))))
    (cond
     ((and cur-ov
           context-item)
        (setq-local eca-chat--context (delete context-item eca-chat--context))
        (eca-chat--refresh-context)
        (end-of-line))

     ((and cur-ov
           (<= (point) (overlay-start cur-ov)))
      (ding))

     ((and cur-ov
           (overlay-get cur-ov 'eca-chat-context-area)
           (or (string= " " (string (char-before (point))))
               (string= eca-chat-context-prefix (string (char-before (point))))))
      ;; trying to remove a space or context-prefix
      )

     (t (delete-char -1)))))

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
                     :request-id (cl-incf eca-chat--last-request-id)
                     :chatId eca-chat--id
                     :model (eca-chat--model)
                     :behavior (eca-chat--behavior)
                     :contexts (eca-chat--contexts->request))
       :success-callback (-lambda (res)
                           (setq-local eca-chat--id (plist-get res :chatId)))))))

(defun eca-chat--point-at-new-context-p ()
  "Return non-nil if point is at the context area."
  (and (eq (line-number-at-pos (point))
           (line-number-at-pos (eca-chat--prompt-area-start-point)))
       (eolp)))

(defun eca-chat--header-line-string ()
  "Update chat header line."
  (let ((model-keymap (make-sparse-keymap))
        (behavior-keymap (make-sparse-keymap)))
    (define-key model-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-model)
    (define-key behavior-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-behavior)
    (list (propertize "model:"
                      'font-lock-face 'eca-chat-option-key-face
                      'pointer 'hand
                      'keymap model-keymap)
          (propertize (eca-chat--model)
                      'font-lock-face 'eca-chat-option-value-face
                      'pointer 'hand
                      'keymap model-keymap)
          "  "
          (propertize "behavior:"
                      'font-lock-face 'eca-chat-option-key-face
                      'pointer 'hand
                      'keymap behavior-keymap)
          (propertize (eca-chat--behavior)
                      'font-lock-face 'eca-chat-option-value-face
                      'pointer 'hand
                      'keymap behavior-keymap))))

(defun eca-chat--mode-line-string ()
  "Update chat mode line."
  (concat eca-chat--progress-text eca-chat--spinner-string))

(defun eca-chat--get-buffer ()
  "Get the eca chat buffer for current session."
  (get-buffer eca-chat-buffer-name))

(defun eca-chat--create-buffer ()
  "Create the eca chat buffer for current session."
  (get-buffer-create (generate-new-buffer-name eca-chat-buffer-name)))

(defun eca-chat--select-window ()
  "Select the Window."
  (select-window (get-buffer-window (buffer-name))))

(defun eca-chat--pop-window ()
  "Pop eca dedicated window if it exists."
  (let ((buffer (current-buffer)))
    (display-buffer buffer eca-chat-position-params)
    (select-window (get-buffer-window buffer))
    (set-window-buffer (get-buffer-window buffer) buffer)))

(defun eca-chat--add-content (content)
  "Add CONTENT to the chat."
  (let ((context-start (eca-chat--prompt-area-start-point)))
    (save-excursion
      (goto-char context-start)
      (goto-char (1- (point)))
      (insert content)
      (point))))

(defun eca-chat--relativize-filename-for-workspace-root (filename roots)
  "Relativize the FILENAME if a workspace root is found for ROOTS."
  (or (-some->> (-first (lambda (root) (f-ancestor-of? root filename)) roots)
        (f-relative filename))
      filename))

(defun eca-chat--refresh-context ()
  "Refresh chat context."
  (save-excursion
    (-some-> (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-context-area)))
                     (overlays-in (point-min) (point-max)))
      (overlay-start)
      (goto-char))
    (delete-region (point) (line-end-position))
    (seq-doseq (context eca-chat--context)
      (-let (((&plist :value value :type type) context))
        (pcase type
          ("file" (let ((text (concat eca-chat-context-prefix (f-filename value))))
                    (insert (propertize text
                                        'eca-chat-context-item context
                                        'font-lock-face 'eca-chat-context-linked-face))
                    (insert " ")))
          ("directory" (let ((text (concat eca-chat-context-prefix (f-filename value))))
                         (insert (propertize text
                                             'eca-chat-context-item context
                                             'font-lock-face 'eca-chat-context-linked-face))
                         (insert " "))))))
    (insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))))

(defun eca-chat--contexts->request ()
  "Convert contexts to a requestable structure."
  (vconcat (-map (-lambda ((&plist :type type :value value))
                   (pcase type
                     ("file" (list :type "file" :path value))
                     ("directory" (list :type "directory" :path value))))
                 eca-chat--context)) )

(defconst eca-chat--kind->symbol
  '(("file" . file)
    ("directory" . folder)))

(defun eca-chat--completion-candidate-kind (item)
  "Return the kind for ITEM."
  (alist-get (plist-get (get-text-property 0 'eca-chat-completion-item item) :type)
             eca-chat--kind->symbol
             nil
             nil
             #'string=))

(defun eca-chat--add-context (type value)
  "Add to chat context VALUE of TYPE."
  (pcase type
    ("file" (add-to-list 'eca-chat--context (list :type "file" :value value) t))
    ("directory" (add-to-list 'eca-chat--context (list :type "directory" :value value) t)))
  (eca-chat--refresh-context))

(defun eca-chat--completion-annotate (roots item-label)
  "Annonate ITEM-LABEL detail for ROOTS."
  (-let (((&plist :type type :path path) (get-text-property 0 'eca-chat-completion-item item-label)))
    (pcase type
      ("file" (eca-chat--relativize-filename-for-workspace-root path roots))
      ("directory" (eca-chat--relativize-filename-for-workspace-root path roots)))))

(defun eca-chat--completion-exit-function (item _status)
  "Add to context the selected ITEM."
  (-let (((&plist :type type :path path) (get-text-property 0 'eca-chat-completion-item item)))
    (pcase type
      ("file" (eca-chat--add-context type path))
      ("directory" (eca-chat--add-context type path))))
  (end-of-line))

(defun eca-chat--context-to-completion (context)
  "Convert CONTEXT to a completion item."
  (propertize
   (pcase (plist-get context :type)
     ("file" (f-filename (plist-get context :path)))
     ("directory" (f-filename (plist-get context :path))))
   'eca-chat-completion-item context))

;; Public

(define-derived-mode eca-chat-mode markdown-mode "eca-chat"
  "Major mode for ECA chat sessions.
\\{eca-chat-mode-map}"
  :group 'eca
  (setq major-mode 'eca-chat-mode)
  (visual-line-mode)
  (hl-line-mode -1)
  (setq-local eca-chat--history '())
  (setq-local eca-chat--history-index 0)

  (make-local-variable 'completion-at-point-functions)
  (setq-local completion-at-point-functions (list #'eca-chat-completion-at-point))
  (when (fboundp 'company-mode)
    (company-mode 1)
    (setq-local company-backends '(company-capf)))

  (unless (listp header-line-format)
    (setq-local header-line-format (list header-line-format)))
  (add-to-list 'header-line-format '(t (:eval (eca-chat--header-line-string))))
  (force-mode-line-update)

  (when (eq 0 (length (string-trim (buffer-string))))
    (save-excursion
      (goto-char (point-min))
      (insert "\n")
      (insert (propertize (eca--session-chat-welcome-message eca--session)
                          'font-lock-face 'eca-chat-welcome-face))
      (eca-chat--insert-prompt-string)))

  (run-with-timer
   0.05
   nil
   (lambda ()
     (with-current-buffer (eca-chat--get-buffer)
       (display-line-numbers-mode -1)
       (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode -1))
       (setq-local mode-line-format '(t (:eval (eca-chat--mode-line-string))))
       (force-mode-line-update))))

  (face-remap-add-relative 'markdown-line-break-face
                           '(:underline nil))

  (goto-char (point-max))
  (run-hooks 'eca-chat-mode-hook))

(defun eca-chat-completion-at-point ()
  "Complete at point in the chat."
  (let ((candidates (lambda ()
                      (cond
                       ((eca-chat--point-at-new-context-p)
                        (-let (((&plist :contexts contexts) (eca-api-request-sync
                                                             :method "chat/queryContext"
                                                             :params (list :chatId eca-chat--id
                                                                           :query (thing-at-point 'symbol t)
                                                                           :contexts (eca-chat--contexts->request)))))
                          (-map #'eca-chat--context-to-completion contexts)))
                       (t nil)))))
    (list
     (or (cl-first (bounds-of-thing-at-point 'symbol))
         (point))
     (point)
     (lambda (probe pred action)
       (cond
          ((eq action 'metadata)
           '(metadata (category . eca-capf)
                      (display-sort-function . identity)
                      (cycle-sort-function . identity)))
          ((eq (car-safe action) 'boundaries) nil)
          (t
           (complete-with-action action (funcall candidates) probe pred))))
     :company-kind #'eca-chat--completion-candidate-kind
     :annotation-function (-partial #'eca-chat--completion-annotate (eca--session-workspace-folders eca--session))
     :exit-function #'eca-chat--completion-exit-function)))

(defun eca-chat-content-received (params)
  "Handle the content received notification with PARAMS."
  (let* ((role (plist-get params :role))
         (content (plist-get params :content)))
    (with-current-buffer (eca-chat--get-buffer)
      (pcase (plist-get content :type)
        ("text" (when-let* ((text (plist-get content :text)))
                  (pcase role
                    ("user" (progn
                              (eca-chat--add-content
                               (propertize text
                                           'font-lock-face 'eca-chat-user-messages-face
                                           'line-prefix (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-user-messages-face)
                                           'line-spacing 10))
                              (font-lock-ensure)))
                    ("system" (progn
                                (eca-chat--add-content
                                 (propertize text
                                             'line-height 20
                                             'font-lock-face 'eca-chat-system-messages-face))))
                    (_ (eca-chat--add-content text)))))
        ("url" (eca-chat--add-content
                (concat
                 "\n"
                 (buttonize
                  (concat "🌐" (plist-get content :title))
                  (lambda(_) (browse-url (plist-get content :url)))
                  nil
                  (plist-get content :url))
                 "\n")))
        ("mcpToolCall" (let ((name (plist-get content :name)))
                         (eca-chat--add-content
                          (format (propertize "\n%s %s\n"
                                              'line-spacing 10)
                                  (propertize "Calling MCP tool"
                                              'line-prefix (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-mcp-tool-call-face)
                                              'font-lock-face 'eca-chat-mcp-tool-call-face)
                                  (propertize name
                                              'font-lock-face 'eca-chat-mcp-tool-call-name-face)))))
        ("progress" (pcase (plist-get content :state)
                      ("running" (progn
                                   (unless eca-chat--spinner-timer
                                     (eca-chat--spinner-start))
                                   (setq-local eca-chat--progress-text (propertize (plist-get content :text) 'font-lock-face 'eca-chat-system-messages-face))))
                      ("finished" (progn
                                    (eca-chat--spinner-stop)
                                    (eca-chat--add-content (propertize "\n" 'line-spacing 10))
                                    (setq-local eca-chat--progress-text "")))))))))

(defun eca-chat-open ()
  "Open or create dedicated eca chat window."
  (let ((source-buffer (current-buffer)))
    (unless (buffer-live-p (eca-chat--get-buffer))
      (eca-chat--create-buffer))
    (with-current-buffer (eca-chat--get-buffer)
      (unless (derived-mode-p 'eca-chat-mode)
        (eca-chat-mode))
      (unless (eca--session-chat eca--session)
        (setf (eca--session-chat eca--session) (current-buffer)))
      (eca-chat--add-context "file" (buffer-file-name source-buffer))
      (if (window-live-p (get-buffer-window (buffer-name)))
          (eca-chat--select-window)
        (eca-chat--pop-window)))))

(defun eca-chat-exit ()
  "Exit the ECA chat."
  (when (buffer-live-p (get-buffer eca-chat-buffer-name))
    (with-current-buffer (eca-chat--get-buffer)
      (goto-char (point-max))
      (setq-local mode-line-format `(,(propertize "*Closed session*" 'font-lock-face 'eca-chat-system-messages-face)))
      (rename-buffer (concat (buffer-name) ":closed") t)
      (quit-window nil (get-buffer-window (eca-chat--get-buffer))))))

;;;###autoload
(defun eca-chat-clear ()
  "Clear the eca chat."
  (interactive)
  (eca-chat--clear))

;;;###autoload
(defun eca-chat-select-model ()
  "Select which model to use in the chat from what server supports."
  (interactive)
  (when-let* ((model (completing-read "Select a model:" (append (eca--session-models eca--session) nil) nil t)))
    (setq eca-chat-custom-model model)))

;;;###autoload
(defun eca-chat-select-behavior ()
  "Select which chat behavior to use from what server supports."
  (interactive)
  (when-let* ((behavior (completing-read "Select a behavior:" (append (eca--session-chat-behaviors eca--session) nil) nil t)))
    (setq eca-chat-custom-behavior behavior)))

(provide 'eca-chat)
;;; eca-chat.el ends here
