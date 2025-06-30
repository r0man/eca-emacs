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

(defface eca-mcp-details-tool-face
  '((t (:inherit hl-line :slant italic)))
  "Face for tools showed in mcp details buffer."
  :group 'eca)

;; Internal

(defvar eca-mcp-details-buffer-name "<eca-mcp-details>")

(defun eca-mcp--get-details-buffer ()
  "Get the eca mcp buffer for current session."
  (get-buffer eca-mcp-details-buffer-name))

(defun eca-mcp--create-details-buffer ()
  "Create the eca mcp details buffer for current session."
  (get-buffer-create (generate-new-buffer-name eca-mcp-details-buffer-name)))

(defun eca-mcp--refresh-server-details ()
  "Refresh the MCP server details."
  (when (buffer-live-p (get-buffer eca-mcp-details-buffer-name))
    (with-current-buffer (eca-mcp--get-details-buffer)
      (erase-buffer)
      (insert (propertize "MCP servers" 'font-lock-face 'helpful-heading))
      (insert "\n\n")
      (seq-doseq (server (-sort  (lambda (a b)
                                   (string-lessp (plist-get a :name)
                                                 (plist-get b :name)))
                                 (eca-vals (eca--session-mcp-servers eca--session))))
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
          (if (seq-empty-p tools)
              (insert (propertize "No tools available" 'font-lock-face font-lock-doc-face))
            (progn
              (insert (propertize "Tools: " 'font-lock-face font-lock-doc-face))
              (seq-doseq (tool tools)
                (insert (propertize (plist-get tool :name) 'font-lock-face 'eca-mcp-details-tool-face) " "))))
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
        (insert "\n\n")))))

;; Public

(define-derived-mode eca-mcp-details-mode fundamental-mode "eca-mcp-details"
  "Major mode for ECA mcp details."
  :group 'eca
  (eca-mcp--refresh-server-details))

(defun eca-mcp--handle-mcp-server-updated (_server)
  "Handle mcp SERVER updated."
  (eca-mcp--refresh-server-details))

(defun eca-mcp-details-exit ()
  "Exit the ECA mcp details."
  (when (buffer-live-p (get-buffer eca-mcp-details-buffer-name))
    (with-current-buffer (eca-mcp--get-details-buffer)
      (goto-char (point-max))
      (setq-local mode-line-format '("*Closed session*"))
      (rename-buffer (concat (buffer-name) ":closed") t)
      (when-let* ((window (get-buffer-window (eca-mcp--get-details-buffer))))
        (quit-window nil window)))))

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
      (display-buffer (current-buffer) eca-mcp-details-position-params)
      (select-window (get-buffer-window (current-buffer)))
      (set-window-buffer (get-buffer-window (current-buffer)) (current-buffer)))))

(provide 'eca-mcp)
;;; eca-mcp.el ends here
