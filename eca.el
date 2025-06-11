;;; eca.el --- AI pair programming via ECA (Editor Code Assistant) -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Maintainer: Eric Dallo <ercdll1337@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (dash "2.18.0"))
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
;;  add AI code assistant tools. Heavily insipired on
;;  lsp-mode for parsing and handling jsonrpc messages.
;;
;;; Code:

(require 'cl-lib)
(require 's)

(require 'eca-util)
(require 'eca-api)
(require 'eca-chat)

(defgroup eca nil
  "ECA group."
  :group 'eca)

;; Variables

(defcustom eca-custom-command '("~/dev/eca/eca" "server")
  "The eca server command.
If not provided, download and start eca automatically."
  :group 'eca
  :risky t
  :type '(repeat string))

(defcustom eca-before-initialize-hook nil
  "List of functions to be called before ECA has been initialized."
  :type 'hook
  :group 'eca)

(defcustom eca-after-initialize-hook nil
  "List of functions to be called after ECA has been initialized."
  :type 'hook
  :group 'eca)

;; Internal

(defun eca--path-to-uri (path)
  "Convert a PATH to a uri."
  (concat "file://"
          (--> path
               (expand-file-name it)
               (or (file-remote-p it 'localname t) it)
               (url-hexify-string it lsp--url-path-allowed-chars))))

(defun eca--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (if (plist-member json-data :id)
      (if (plist-member json-data :error)
          'response-error
        (if (plist-member json-data :method)
            'request
          'response))
    'notification))

(defun eca--parse-header (s)
  "Parse string S as a ECA (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'eca-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(setq a (make-hash-table :test 'equal))

(puthash 2 :foo a)
(gethash 2 a)
(remhash 2 a)

(setq b nil)

(plist-put! b 2 :foo)
(plist-put! b 3 :bar)
(plist-get b 2)
(cl-remf b 2)

(defun eca--handle-message (json-data)
  "Handle raw message JSON-DATA."
  (let ((id (plist-get json-data :id))
        (result (plist-get json-data :result)))
    (pcase (eca--get-message-type json-data)
      ('response (-let [(success-callback) (plist-get (eca--session-response-handlers eca--session) id)]
                   (when success-callback
                     (cl-remf (eca--session-response-handlers eca--session) id)
                     (funcall success-callback result))))
      ('response-error ())
      ('notification ())
      ('request ()))))

(defun eca--process-filter (_proc raw-output)
  "The process filter to parse eca's stdout RAW-OUTPUT."
  (let ((body-received 0)
        leftovers body-length body chunk)
    (setf chunk (if (s-blank? leftovers)
                    (encode-coding-string raw-output 'utf-8-unix t)
                  (concat leftovers (encode-coding-string raw-output 'utf-8-unix t))))
    (let (messages)
      (while (not (s-blank? chunk))
        (if (not body-length)
            ;; Read headers
            (if-let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                ;; We've got all the headers, handle them all at once:
                (setf body-length (let* ((headers (mapcar #'eca--parse-header
                                                          (split-string
                                                           (substring-no-properties chunk
                                                                                    (or (string-match-p "Content-Length" chunk)
                                                                                        (error "Unable to find Content-Length header"))
                                                                                    body-sep-pos)
                                                           "\r\n")))
                                         (content-length (cdr (assoc "Content-Length" headers))))
                                    (if content-length
                                        (string-to-number content-length)
                                      ;; This usually means either the server or our parser is
                                      ;; screwed up with a previous Content-Length
                                      (error "No Content-Length header")))
                      body-received 0
                      leftovers nil
                      chunk (substring-no-properties chunk (+ body-sep-pos 4)))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf leftovers chunk
                    chunk nil))
          (let* ((chunk-length (string-bytes chunk))
                 (left-to-receive (- body-length body-received))
                 (this-body (if (< left-to-receive chunk-length)
                                (prog1 (substring-no-properties chunk 0 left-to-receive)
                                  (setf chunk (substring-no-properties chunk left-to-receive)))
                              (prog1 chunk
                                (setf chunk nil))))
                 (body-bytes (string-bytes this-body)))
            (push this-body body)
            (setf body-received (+ body-received body-bytes))
            (when (>= chunk-length left-to-receive)
              (condition-case err
                  (with-temp-buffer
                    (apply #'insert
                           (nreverse
                            (prog1 body
                              (setf leftovers nil
                                    body-length nil
                                    body-received nil
                                    body nil))))
                    (decode-coding-region (point-min)
                                          (point-max)
                                          'utf-8)
                    (goto-char (point-min))
                    (push (eca-api--json-read-buffer) messages))

                (error
                 (eca-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                           (concat leftovers raw-output)
                           err)))))))
      (mapc (lambda (msg)
              (eca--handle-message msg))
            (nreverse messages)))))

(defun eca--start-process ()
  "Start the eca process."
  (unless (process-live-p (eca--session-process eca--session))
    (eca-info "Starting process...")
    (let ((stderr-buffer (get-buffer-create "*eca:stderr*")))
      (with-current-buffer stderr-buffer
        (delete-region (point-min) (point-max)))
      (setf (eca--session-process eca--session)
            (make-process
             :coding 'no-conversion
             :connection-type 'pipe
             :name "eca"
             :command eca-custom-command
             :buffer "*eca*"
             :stderr stderr-buffer
             :filter #'eca--process-filter
             :sentinel (lambda (process exit-str)
                         (unless (process-live-p process)
                           (setq eca--session nil)
                           (eca-info "process has exited (%s)" exit-str)))
             :file-handler t
             :noquery t)))))

(defun eca--initialize ()
  "Sent the initialize request."
  (run-hooks 'eca-before-initialize-hook)
  (eca-api-request-async
   :method "initialize"
   :params (list :processId (emacs-pid)
                 :clientInfo (list :name "emacs"
                                   :version (emacs-version))
                 :capabilities (list :codeAssistant (list :chat t))
                 :workspaceFolders (vconcat (-map (lambda (folder)
                                                    (list :uri (eca--path-to-uri folder)
                                                          :name (file-name-nondirectory (directory-file-name folder))))
                                                  (eca--session-workspace-folders eca--session))))
   :success-callback (-lambda (res)
                       (eca-chat-open)
                       (run-hooks 'eca-after-initialize-hook))))

;;;###autoload
(defun eca ()
  "Start or switch an eca session."
  (interactive)
  (unless eca--session
    (setq eca--session (eca-create-session)))
  (eca--start-process)
  (eca--initialize))

;;;###autoload
(defun eca-stop ()
  "Stop eca if running."
  (interactive)
  (when (and eca--session
             (process-live-p (eca--session-process eca--session)))
    (eca-info "Shutting down...")
    (eca-api-request-sync :method "shutdown")
    (eca-api-notify :method "exit")
    (kill-process (eca--session-process eca--session))
    (eca-chat-exit)
    (setq eca--session nil)))

(provide 'eca)
;;; eca.el ends here
