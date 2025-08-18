;;; eca-editor.el --- ECA (Editor Code Assistant) editor -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) editor.
;;
;;; Code:

(require 'dash)
(require 'f)

(require 'lsp-mode nil t)
(require 'flymake nil t)

(require 'eca-util)

(declare-function lsp-diagnostics "lsp-mode" (current-workspace?))

(defun eca-editor--lsp-to-eca-severity (severity)
  "Convert lsp SEVERITY to eca one."
  (pcase severity
    (1 "error")
    (2 "warning")
    (3 "info")
    (4 "hint")
    (_ "unknown")))

(defun eca-editor--lsp-mode-diagnostics (uri workspace)
  "Find all lsp-mode diagnostics found for WORKSPACE.
If URI is nil find all diagnostics otherwise filter to that uri."
  (let* ((eca-diagnostics '())
         (all-diagnostics (car (--keep (when (and (buffer-file-name it)
                                                  (f-ancestor-of? workspace (buffer-file-name it)))
                                         (with-current-buffer it
                                           (lsp-diagnostics t)))
                                       (buffer-list))))
         (diagnostics (if uri
                          (gethash (eca--uri-to-path uri) all-diagnostics)
                        all-diagnostics)))
    (maphash (lambda (path lsp-diagnostics)
               (let ((uri (eca--path-to-uri path)))
                 (--map (let* ((range (gethash "range" it))
                               (start-r (gethash "start" range))
                               (end-r (gethash "end" range)))
                          (push (list :uri uri
                                      :severity (eca-editor--lsp-to-eca-severity (gethash "severity" it))
                                      :code (gethash "code" it)
                                      :range (list :start (list :line (gethash "line" start-r)
                                                                :character (gethash "character" start-r))
                                                   :end (list :line (gethash "line" end-r)
                                                              :character (gethash "character" end-r)))
                                      :source (gethash "source" it)
                                      :message (gethash "message" it))
                                eca-diagnostics))
                        lsp-diagnostics)))
             diagnostics)
    eca-diagnostics))

(defun eca-editor--flymake-diagnostics (_uri _workspace)
  "Find all flymake diagnostics found for WORKSPACE.
If URI is nil find all diagnostics otherwise filter to that uri."
  ;; TODO
  )

(defun eca-editor--get-lsp-diagnostics (uri workspaces)
  "Return lsp diagnostics for URI in WORKSPACES.
If URI is nil, return all workspaces diagnostics."
  (let ((eca-diagnostics '()))
    (seq-doseq (workspace workspaces)
      (cond
       ((featurep 'lsp-mode) (setq eca-diagnostics (append eca-diagnostics (eca-editor--lsp-mode-diagnostics uri workspace))))
       ((featurep 'flymake) (setq eca-diagnostics (append eca-diagnostics (eca-editor--flymake-diagnostics uri workspace))))
       (t nil)))
    eca-diagnostics))

(defun eca-editor-get-diagnostics (session params)
  "Return all diagnostics for SESSION from PARAMS."
  (let* ((uri (plist-get params :uri))
         (lsp-diags (eca-editor--get-lsp-diagnostics uri (eca--session-workspace-folders session))))
    (list :diagnostics (vconcat lsp-diags))))

(provide 'eca-editor)
;;; eca-editor.el ends here
