;;; eca-util.el --- ECA (Editor Code Assistant) util -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) utils.
;;
;;; Code:

(require 'cl-lib)
(require 'vc-git)
(require 'dash)

(defun eca--project-root ()
  "Get the project root using git falling back to file directory."
  (-some-> (or (vc-git-root default-directory)
               (when buffer-file-name (file-name-directory buffer-file-name))
               default-directory)
    (file-truename)))

(defvar eca--session nil)

(cl-defstruct eca--session
  ;; The status of this session
  (status 'stopped)

  ;; The eca <process>
  (process nil)

  ;; the chat buffer
  (chat nil)

  ;; A list of workspace folders of this session
  (workspace-folders '())

  ;; A plist of request method names (strings) -> handlers used when
  ;; receiving requests from server.
  (request-handlers '())

  ;; A plist of client request ids -> handlers for pending requests used when
  ;; receiving responses from server.
  (response-handlers '())

  ;; The suported models by the server.
  (models '())

  ;; The chat behavior like agent, manual, ask.
  (chat-behavior nil)

  ;; The welcome message for new chats.
  (chat-welcome-message ""))

(defun eca-create-session ()
  "Create a new ECA session."
  (let ((session (make-eca--session)))
    (setf (eca--session-workspace-folders session) (list (eca--project-root)))
    session))

(defun eca-info (format &rest args)
  "Display eca info message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "ECA" 'face 'success) (apply #'format format args)))

(defun eca-warn (format &rest args)
  "Display eca warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "ECA" 'face 'warning) (apply #'format format args)))

(defun eca-error (format &rest args)
  "Display eca error message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "ECA" 'face 'error) (apply #'format format args)))

(provide 'eca-util)
;;; eca-util.el ends here
