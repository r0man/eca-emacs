;;; eca-api.el --- ECA (Editor Code Assistant) api -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) api.
;;
;;; Code:

(require 'cl-lib)
(require 'json)

(require 'eca-util)

;; Variables

(defcustom eca-api-response-timeout 10
  "The max time to wait for eca responses"
  :group 'eca
  :type 'number)

;; Internal

(defvar eca--last-id 0
  "Last request id.")

(defmacro eca-api--json-read-buffer ()
  "Read json from the current buffer."
  (if (fboundp 'json-parse-buffer)
      `(json-parse-buffer :object-type 'plist
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'vector)
           (json-object-type 'plist)
           (json-false nil))
       (json-read))))

(defmacro eca-api--json-serialize (params)
  (if (fboundp 'json-serialize)
      `(json-serialize ,params
        :null-object nil
        :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params))))

(defun eca-api--make-message (params)
  "Create a JSONRPC message from PARAMS, after encoding it to a JSON string."
  (let ((body (eca-api--json-serialize params)))
    (concat "Content-Length: "
            (number-to-string (1+ (string-bytes body)))
            "\r\n\r\n"
            body
            "\n")))

(defun eca-api--send! (body)
  "Send to process the BODY."
  (condition-case err
      (process-send-string (eca--session-process eca--session) (eca-api--make-message body))
    (error (eca-error "Sending to process failed with the following error: %s"
                      (error-message-string err)))))

;; Public

(cl-defun eca-api-request-async (&key method params success-callback error-callback)
  "TODO"
  (let* ((id (cl-incf eca--last-id))
         (body `(:jsonrpc "2.0" :method ,method :params ,params :id ,id)))
    (setf (eca--session-response-handlers eca--session)
        (plist-put (eca--session-response-handlers eca--session) id (list success-callback error-callback)))
    (eca-api--send! body)))

(cl-defun eca-api-request-sync (&key method params)
  "TODO"
  (let* ((send-time (float-time))
         (expected-time (and eca-api-response-timeout
                             (+ send-time eca-api-response-timeout)))
         resp-result resp-error)
    (eca-api-request-async
     :method method
     :params params
     :success-callback (lambda (res) (setf resp-result (or res :finished)) (throw 'eca-done '_))
     :error-callback (lambda (err) (setf resp-error err) (throw 'eca-done '_)))
    (while (not (or resp-error resp-result))
      (if (functionp 'json-rpc-connection)
          (catch 'eca-done (sit-for 0.01))
        (catch 'eca-done
          (accept-process-output
           nil
           (if expected-time (- expected-time send-time) 1))))
      (setq send-time (float-time))
      (when (and expected-time (< expected-time send-time))
        (error "Timeout while waiting for response. Method: %s" method)))
    (cond
     ((eq resp-result :finished) nil)
     (resp-result resp-result)
     (resp-error (error resp-error)))))

(cl-defun eca-api-notify (&key method params)
  (let* ((body `(:jsonrpc "2.0" :method ,method :params ,params)))
    (eca-api--send! body)))

(provide 'eca-api)
;;; eca-api.el ends here
