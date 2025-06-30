;;; eca-mcp.el --- ECA (Editor Code Assistant) mcp -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) mcp.
;;
;;; Code:

(require 'compat)

(require 'eca-util)
(require 'eca-process)

(defcustom eca-mcp-details-position-params `((display-buffer-in-side-window)
                                             (side . right)
                                             (window-width . 0.35))
  "Position params for mcp details display."
  :type 'alist
  :group 'eca)

;; Internal

(defvar eca-mcp-details-buffer-name "<eca-mcp-details>")

(defun eca-mcp--get-details-buffer ()
  "Get the eca mcp buffer for current session."
  (get-buffer eca-mcp-details-buffer-name))

(defun eca-mcp--create-details-buffer ()
  "Create the eca mcp details buffer for current session."
  (get-buffer-create (generate-new-buffer-name eca-mcp-details-buffer-name)))

;; Public

(define-derived-mode eca-mcp-details-mode fundamental-mode "eca-mcp-details"
  "Major mode for ECA mcp details."
  :group 'eca
  (insert (propertize "MCP servers" 'font-lock-face 'helpful-heading))
  (insert "\n\n")
  (seq-doseq (server (eca-vals (eca--session-mcp-servers eca--session)))
    (-let (((&plist :name name :command command :args args
                    :status status :tools tools) server))
      (insert (propertize name 'font-lock-face 'bold))
      (insert " - ")
      (insert (propertize status
                          'font-lock-face (pcase status
                                            ("running" 'success)
                                            ("starting" 'warning)
                                            ("failed" 'error)
                                            ("stopped" 'default)
                                            ("disabled" 'shadow))))
      (insert "\n")
      (insert (if (seq-empty-p tools)
                  (propertize "No tools available." 'font-lock-face font-lock-doc-face)
                "..."))
      (insert "\n")
      (insert (propertize "Command: " 'font-lock-face font-lock-doc-face))
      (insert (concat command " " (string-join args " ")))
      (when (string= "failed" status)
        (insert "\n")
        (insert (propertize (format "Failed to start, check %s for details"
                                    (buttonize
                                     "eca stderr buffer"
                                     (lambda(_) (eca-process-show-stderr))))
                            'font-lock-face 'error))))
    (insert "\n\n")))

;;;###autoload
(defun eca-mcp-details ()
  "List MCP servers with their status and options."
  (interactive)
  (eca-assert-session-running)
  (unless (buffer-live-p (eca-mcp--get-details-buffer))
    (eca-mcp--create-details-buffer))
  (with-current-buffer (eca-mcp--get-details-buffer)
    (unless (derived-mode-p 'eca-mcp-details-mode)
      (eca-mcp-details-mode))
    (if (window-live-p (get-buffer-window (buffer-name)))
        (select-window (get-buffer-window (buffer-name)))
      (let ((buffer (current-buffer)))
        (display-buffer buffer eca-mcp-details-position-params)
        (select-window (get-buffer-window buffer))
        (set-window-buffer (get-buffer-window buffer) buffer)))))

(provide 'eca-mcp)
;;; eca-mcp.el ends here
