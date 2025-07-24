;;; eca.el --- AI pair programming via ECA (Editor Code Assistant) -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Maintainer: Eric Dallo <ercdll1337@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (dash "2.18.0") (f "0.20.0") (markdown-mode "2.3") (compat "30.1"))
;; Keywords: tools
;; Homepage: https://github.com/editor-code-assistant/eca-emacs
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) client for Emacs to
;;  add AI code assistant tools.  Heavily insipired on
;;  lsp-mode for parsing and handling jsonrpc messages.
;;
;;; Code:

(require 'cl-lib)

(require 'eca-util)
(require 'eca-process)
(require 'eca-api)
(require 'eca-chat)
(require 'eca-mcp)

(defgroup eca nil
  "ECA group."
  :group 'eca)

;; Variables

(defcustom eca-before-initialize-hook nil
  "List of functions to be called before ECA has been initialized."
  :type 'hook
  :group 'eca)

(defcustom eca-after-initialize-hook nil
  "List of functions to be called after ECA has been initialized."
  :type 'hook
  :group 'eca)

;; Internal

(defun eca--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (if (plist-member json-data :id)
      (if (plist-member json-data :error)
          'response-error
        (if (plist-member json-data :method)
            'request
          'response))
    'notification))

(defun eca--handle-show-message (params)
  "Handle the show-message notification with PARAMS."
  (let ((type (plist-get params :type))
        (msg (plist-get params :message)))
    (pcase type
      ("error" (eca-error msg))
      ("warning" (eca-warn msg))
      ("info" (eca-info msg)))))

(defun eca--tool-server-updated (server)
  "Handle tool server updated message with SERVER."
  (setf (eca--session-tool-servers eca--session)
        (eca-assoc (eca--session-tool-servers eca--session)
                   (plist-get server :name)
                   server))
  (eca-chat--handle-mcp-server-updated server)
  (eca-mcp--handle-mcp-server-updated server))

(defun eca--handle-server-notification (notification)
  "Handle NOTIFICATION sent by server."
  (let ((method (plist-get notification :method))
        (params (plist-get notification :params)))
    (pcase method
      ("chat/contentReceived" (eca-chat-content-received params))
      ("tool/serverUpdated" (eca--tool-server-updated params))
      ("$/showMessage" (eca--handle-show-message params))
      (_ (eca-warn "Unknown notification %s" method)))))

(defun eca--handle-server-request (_request)
  "Handle REQUEST sent by server."
  ;; TODO
  )

(defun eca--handle-message (json-data)
  "Handle raw message JSON-DATA."
  (let ((id (plist-get json-data :id))
        (result (plist-get json-data :result)))
    (pcase (eca--get-message-type json-data)
      ('response (-let [(success-callback) (plist-get (eca--session-response-handlers eca--session) id)]
                   (when success-callback
                     (cl-remf (eca--session-response-handlers eca--session) id)
                     (funcall success-callback result))))
      ('response-error (-let [(_ error-callback) (plist-get (eca--session-response-handlers eca--session) id)]
                         (when error-callback
                           (cl-remf (eca--session-response-handlers eca--session) id)
                           (funcall error-callback (plist-get json-data :error)))))
      ('notification (eca--handle-server-notification json-data))
      ('request (eca--handle-server-request json-data)))))

(defun eca--initialize ()
  "Sent the initialize request."
  (run-hooks 'eca-before-initialize-hook)
  (setf (eca--session-status eca--session) 'starting)
  (eca-api-request-async
   :method "initialize"
   :params (list :processId (emacs-pid)
                 :clientInfo (list :name "emacs"
                                   :version (emacs-version))
                 :capabilities (list :codeAssistant (list :chat t))
                 :initializationOptions (list :chatBehavior eca-chat-custom-behavior)
                 :workspaceFolders (vconcat (-map (lambda (folder)
                                                    (list :uri (eca--path-to-uri folder)
                                                          :name (file-name-nondirectory (directory-file-name folder))))
                                                  (eca--session-workspace-folders eca--session))))
   :success-callback (-lambda ((&plist :chatWelcomeMessage msg
                                       :chatBehaviors chat-behaviors
                                       :chatDefaultBehavior chat-default-behavior
                                       :chatDefaultModel chat-default-model
                                       :models models))
                       (setf (eca--session-status eca--session) 'started)
                       (setf (eca--session-chat-welcome-message eca--session) msg)
                       (setf (eca--session-models eca--session) models)
                       (setf (eca--session-chat-behaviors eca--session) chat-behaviors)
                       (setf (eca--session-chat-default-model eca--session) chat-default-model)
                       (setf (eca--session-chat-default-behavior eca--session) chat-default-behavior)
                       (eca-api-notify :method "initialized")
                       (eca-info "Started!")
                       (eca-chat-open)
                       (run-hooks 'eca-after-initialize-hook))
   :error-callback (lambda (e) (eca-error e))))

;;;###autoload
(defun eca ()
  "Start or switch to a eca session."
  (interactive)
  (unless eca--session
    (setq eca--session (eca-create-session)))
  (pcase (eca--session-status eca--session)
    ('stopped (eca-process-start (lambda ()
                                    (eca--initialize))
                                 #'eca--handle-message))
    ('started (eca-chat-open))
    ('starting (eca-info "eca server is already starting"))))

;;;###autoload
(defun eca-stop ()
  "Stop eca if running."
  (interactive)
  (when (eca-process-running-p)
    (eca-info "Shutting down...")
    (eca-api-request-sync :method "shutdown")
    (eca-api-notify :method "exit")
    (eca-process-stop))
  (eca-chat-exit)
  (eca-mcp-details-exit)
  (setq eca--session nil))

;;;###autoload
(defun eca-workspaces ()
  "Return workspaces used by current session."
  (interactive)
  (when eca--session
    (eca-info "Workspaces: %s" (eca--session-workspace-folders eca--session))))

(provide 'eca)
;;; eca.el ends here
