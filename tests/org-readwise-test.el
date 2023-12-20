;;; org-readwise-test.el --- tests for readwise.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 Vincent Demeester
;; Author: Vincent Demeester <vincent@sbr.pm>

;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;
;; Test for readwise.el using ERT
;;
;; The idea behind this tests is the following
;; - A fake server is started and serves "testdata" (json's)
;; - We configure the readwise package to use this fake server
;; - We run query and check the results
;;
;;; Code:

(require 'ert)
(require 'request)
(require 'org-readwise)

;; Globally overwrite the `org-readwise-url' variable.
(setq org-readwise-url "https://foo.bar/baz/v2")

(ert-deftest org-readwise-test--test ()
  "Test the test."
  (request-testing-server)
  ;; (org-readwise--fetch-highlights org-readwise-api-token org-readwise-sync-db-path nil nil 't)
  ;; (setq org-readwise-url (request-testing-url "foo"))
  (should (equal org-readwise-url "https://foo.bar/baz/v2"))
  (should (equal 1 1)))

;; See https://github.com/tkf/emacs-request/blob/master/tests/request-testing.el#L74-L122 for setting up
;; a "fake" api server for this.
(defvar request-testing-server--process nil)
(defvar request-testing-server--port nil)

(defvar request-testing-source-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defun request-testing--wait-process-until (process output-regexp)
  "Wait until PROCESS outputs text which matches to OUTPUT-REGEXP."
  (cl-loop with buffer = (process-buffer process)
           repeat 30
           do (accept-process-output process 0.1 nil t)
           for str = (with-current-buffer buffer (buffer-string))
           do (cond
               ((string-match output-regexp str)
                (cl-return str))
               ((not (eq 'run (process-status process)))
                (error "Server startup error.")))
           finally do (error "Server timeout error.")))

(defun request-testing-server ()
  "Get running test server and return its root URL."
  (interactive)
  (unless request-testing-server--port
    (let ((process (start-process "request-testing" " *request-testing*"
				  (or (executable-find "python2")
				      (executable-find "python"))
                                  (expand-file-name
                                   "testserver.py"
                                   request-testing-source-dir))))
      (setq request-testing-server--process process)
      (setq request-testing-server--port
            (string-to-number
             (request-testing--wait-process-until process "^[0-9]+$")))
      (request-testing--wait-process-until process "Running on")))
  (request-testing-url))

(defun request-testing-stop-server ()
  (interactive)
  (let ((process request-testing-server--process))
    (if (process-live-p process)
        (quit-process process)
      (unless noninteractive
        (message "No server is running!"))))
  (setq request-testing-server--port nil)
  (setq request-testing-server--process nil))
(add-hook 'kill-emacs-hook 'request-testing-stop-server)

(defun request-testing-url (&rest path)
  (cl-loop with url = (format "http://127.0.0.1:%s" request-testing-server--port)
           for p in path
           do (setq url (concat url "/" p))
           finally return url))

(provide 'org-readwise-test)
;;; org-readwise-test.el ends here
