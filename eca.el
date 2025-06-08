;;; eca.el --- AI pair programming via ECA (Editor Code Assistant) -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Maintainer: Eric Dallo <ercdll1337@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai emacs llm eca ai-pair-programming tools
;; Homepage: https://github.com/ericdallo/eca-emacs
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) client for Emacs to
;;  add AI code assistant tools.
;;
;;; Code:

(require 'vc-git)

(require 'eca-chat)

(defgroup eca nil
  "ECA group."
  :group 'eca)

;; Variables

;; Internal

(defvar-local eca--session-name nil)

(defun eca--project-root ()
  "Get the project root using git falling back to file directory."
  (or (vc-git-root default-directory)
      (when buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun eca--start-process (session)
  ""
  ;; TODO
  )

;;;###autoload
(defun eca ()
  "Start or switch to eca session."
  (interactive)
  (let ((session (file-truename (eca--project-root))))
    (eca--start-process session)
    (eca-chat-open session)))

(provide 'eca)
;;; eca.el ends here
