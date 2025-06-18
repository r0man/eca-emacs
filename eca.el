;;; eca.el --- AI pair programming via ECA (Editor Code Assistant) -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Maintainer: Eric Dallo <ercdll1337@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (dash "2.18.0") (f "0.20.0") (markdown-mode "2.3"))
;; Keywords: ai emacs llm eca ai-pair-programming tools
;; Homepage: https://github.com/ericdallo/eca-emacs
;;
;; SPDX-License-Identifier: Apache-2.0
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

(defcustom eca-custom-command nil
  "The eca server command.
If not provided, download and start eca automatically."
  :group 'eca
  :risky t
  :type '(repeat string))

(defcustom eca-server-download-url
  (format "https://github.com/editor-code-assistant/eca/releases/latest/download/eca-native-static-%s.zip"
          (let ((arch (car (split-string system-configuration "-"))))
            (pcase system-type
              ('gnu/linux (concat "linux-"
                                  (cond
                                   ((string= "x86_64" arch) "amd64")
                                   (t arch))))
              ('darwin (concat "macos-"
                               (cond
                                ((string= "x86_64" arch) "amd64")
                                (t arch))))
              ('windows-nt "windows-amd64"))))
  "The URL to download eca server."
  :group 'eca
  :type 'string)

(defcustom eca-server-install-path
  (f-join (expand-file-name
           (locate-user-emacs-file "eca"))
          (if (eq system-type 'windows-nt)
              "eca.exe"
            "eca"))
  "Directory in which eca will be downloaded."
  :risky t
  :type 'directory
  :group 'eca)

(defconst eca-ext-pwsh-script "pwsh -noprofile -noninteractive \
-nologo -ex bypass -c Expand-Archive -Path '%s' -DestinationPath '%s'"
  "Pwsh script to unzip file.")

(defconst eca-ext-powershell-script "powershell -noprofile -noninteractive \
-nologo -ex bypass -command Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip file.")

(defconst eca-ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"
  "Unzip script to unzip file.")

(defcustom eca-unzip-script (lambda ()
                              (cond ((and (eq system-type 'windows-nt)
                                          (executable-find "pwsh"))
                                     eca-ext-pwsh-script)
                                    ((and (eq system-type 'windows-nt)
                                          (executable-find "powershell"))
                                     eca-ext-powershell-script)
                                    ((executable-find "unzip") eca-ext-unzip-script)
                                    ((executable-find "pwsh") eca-ext-pwsh-script)
                                    (t nil)))
  "The script to unzip downloaded eca server."
  :group 'eca
  :type 'string)

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
               (or (file-remote-p it 'localname t) it))))

(defun eca--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (if (plist-member json-data :id)
      (if (plist-member json-data :error)
          'response-error
        (if (plist-member json-data :method)
            'request
          'response))
    'notification))

(defun eca--download-server ()
  "Download eca server."
  (let* ((url eca-server-download-url)
         (store-path eca-server-install-path)
         (download-path (concat store-path ".zip")))
    (make-thread
     (lambda ()
       (condition-case err
           (progn
             (when (f-exists? download-path) (f-delete download-path))
             (when (f-exists? store-path) (f-delete store-path))
             (eca-info "Starting to download eca to %s..." download-path)
             (mkdir (f-parent download-path) t)
             (url-copy-file url download-path)
             (unless eca-unzip-script
               (error "Unable to find `unzip' or `powershell' on the path, please customize `eca-unzip-script'"))
             (shell-command (format (funcall eca-unzip-script) download-path (f-parent store-path)))
             (eca-info "Downloaded eca successfully"))
         (error "Could not download eca server" err))))))

(defun eca--server-command ()
  "Build the eca server command downloading server if not provided."
  (or eca-custom-command
      (unless (f-exists? eca-server-install-path)
        (eca--download-server)
        nil)
      (list eca-server-install-path "server")))

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

(defun eca--handle-server-notification (notification)
  "Handle NOTIFICATION sent by server."
  (let ((method (plist-get notification :method))
        (params (plist-get notification :params)))
    (pcase method
      ("chat/contentReceived" (eca-chat-content-received params))
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
             :command (eca--server-command)
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
  (pcase (eca--session-status eca--session)
    ('stopped (progn
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
                 :success-callback (-lambda ((&plist :chatWelcomeMessage msg :chatBehavior chat-behavior :models models))
                                     (setf (eca--session-status eca--session) 'started)
                                     (setf (eca--session-chat-welcome-message eca--session) msg)
                                     (setf (eca--session-models eca--session) models)
                                     (setf (eca--session-chat-behavior eca--session) chat-behavior)
                                     (eca-info "Started!")
                                     (eca-chat-open)
                                     (run-hooks 'eca-after-initialize-hook)))))
    ('started (eca-chat-open))))

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
    (kill-buffer "*eca*"))
  (eca-chat-exit)
  (setq eca--session nil))

(provide 'eca)
;;; eca.el ends here
