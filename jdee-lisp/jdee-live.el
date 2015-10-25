;;; jdee-live.el --- The JVM backend for JDE. -*- lexical-binding: t -*-

;; Version: 0.1
;; Package-Requires: ((cider "20151020.646")(load-relative "20150224.1722"))

;;; Commentary:
;;  Nrepl backend


;;; Code:
;; The contents of this file are subject to the GPL License, Version 3.0.
                                        ;(require 'cider-client)
(require 'cider)
(require 'load-relative)

(defconst jdee-live-version "0.1-SNAPSHOT")

(defgroup jdee-live nil
  "Parameters for how the connection to the nREPL backend is made"
  :group 'jdee
  :prefix "jdee-live-")

(defcustom jdee-live-timeout 10
  "Time (in seconds) to wait for a response from the nREPL."
  :type 'number
  :group 'jdee-live
  :package-version '("jdee-live" . "0.1"))

(defvar jdee-live-server-alist nil
  "Mapping from pom files to the corresponding server process.")

(defvar jdee-live-launch-script
  (relative-expand-file-name "../jdee-live/clj/jdee-launch-nrepl.clj"))

(defun jdee-live-project-directory-for (dir-name)
  "Find the first directory in DIR-NAME contain a pom.xml.
This is the root of the maven project."
  (when dir-name
    (locate-dominating-file dir-name "pom.xml")))


(defun jdee-live-server ()
  "Get the stored server for this buffer.
The server may be ndead.  Returns nil if the server has not been started."
  (let* ((project-dir (jdee-live-project-directory-for (cider-current-dir)))
         (project-server-cons
          (cl-member-if
           (lambda (el) (string-equal project-dir (car el)))
           jdee-live-server-alist)))
    (message "jdee-live-server: p-dir %s p-s-cons %s"
             project-dir project-server-cons)
    (and project-server-cons
         (cdar project-server-cons))))

(defun jdee-live-stop-server ()
  "Stop the server for this project."
  (let ((server (jdee-live-server)))
    (message "jdee-live-stop-server: server is %s, dir %s, alist %s"
             server
             (jdee-live-project-directory-for (cider-current-dir))
             jdee-live-server-alist)
    (when (jdee-live-connected-p server)
      ;; Kill the process
      (kill-process server)
      ;; Remove it from the lookup table
      (setq jdee-live-server-alist (rassq-delete-all server jdee-live-server-alist)))))

(defun jdee-live-connected-p (&optional server)
  "Test if there is a live server process for this buffer.
If SERVER is given, check if that server is live"
  (let ((server (or server (jdee-live-server))))
    (and server (process-live-p server))))

(defvar jdee-live--response nil
  "Response from the nREPL.")

(defun jdee-live-jeval (statement &optional eval-return no-print-p)
  "Use the jdee-live nREPL to evaluate the Clojure expression EXPR.
If the
nREPL is not running, the JDEE starts an instance.  This function
returns any text output by the nREPL's standard out or
standard error pipes from evaluating STATEMENT.
If EVAL-RETURN is non-nil, this function
returns the result of evaluating the output as a Lisp
expression.

NO-PRINT-P, if non-nil, don't wrap STATEMENT with a `print'
command yeilding the output.  This is going to need to be true
for most things since unless `show()' was invoked and output
prints out, Emacs has nothing to evaluate or report."
  (interactive "sClojure to evaluate ")
  (unless (jdee-live-connected-p)
    (jdee-live-jack-in))
  (let ((server (jdee-live-server)))
    (unwind-protect
        (let ((jdee-live--response nil))
          (set-process-filter server #'jdee-live-eval-filter)
          (if (not (accept-process-output server jdee-live-timeout))
              (error "No response from nREPL")
            jdee-live--response))
      (set-process-filter server #'jdee-live-server-filter))))




;;;###autoload
(defun jdee-live-jack-in ()
  "Start a maven process which connects to the nrepl client.
Check the versions of the middle ware"
  (interactive)
  (if (jdee-live-connected-p)
      (message "nREPL server already running")
    (let* ((project-dir
            (jdee-live-project-directory-for (cider-current-dir))))
      (-when-let (repl-buff (cider-find-reusable-repl-buffer nil project-dir))
        (let* ((nrepl-create-client-buffer-function #'cider-repl-create)
               (nrepl-use-this-as-repl-buffer repl-buff)
               (serv-proc
                (nrepl-start-server-process
                 project-dir
                 ;; TODO: FIXME: generalise!
                 (format
                  (concat "mvn jdee:jdee-maven-nrepl:java "
                          "-Dexec.mainClass=\"clojure.main\""
                          " -Dexec.args=\"%s\""
                          " -Dexec.includePluginsDependencies=true"
                          )
                  jdee-live-launch-script))))
          ;; FIXME: clojure:nrepl drops strange strings out!
          (set-process-filter serv-proc #'jdee-live-server-filter)
          (add-to-list 'jdee-live-server-alist (cons project-dir serv-proc))
          (jdee-live-wait-for-server-ready serv-proc)
          )))))

(defun jdee-live-wait-for-server-ready (serv-proc)
  "Wait for SERV-PROC to be ready to accept input."
  (let ((jdee-live--response nil))
    (while (not jdee-live--response)
      (if (not (accept-process-output serv-proc jdee-live-timeout))
          (error "No response from nREPL")
        (cond
         ;; Successfully connected
         ((eq 'connected jdee-live--response))
         ;; String return means an error occurred
         ((stringp jdee-live--response)
          (error "Unable to connect to nREPL server:\n%s"
                 jdee-live--response))
         (;; Partial output, continue waiting
          (not jdee-live--response))
         (t
          (error "Jdee-live--response set to unexpected value: %s"
                 jdee-live--response)))))))


(defun jdee-live-eval-filter (process output)
  "Process nREPL server output from PROCESS contained in OUTPUT during evaluation."
  (setq jdee-live--response (concat jdee-live--response output)))

(defun jdee-live-server-filter (process output)
  "Process nREPL server output from PROCESS contained in OUTPUT."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert output)))
  (message "checking %s" output)
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (string-to-number (match-string 1 output))))
      (message (format "nREPL server started on %s" port))
      (with-current-buffer (process-buffer process)
        (let ((client-proc (nrepl-start-client-process nil port process)))
          ;; FIXME: Bad connection tracking system. There can be multiple client
          ;; connections per server.  I think this is fixed: there is not one
          ;; connection per pom.xml, which is shared between files in the
          ;; project.   -td 10/24/15.
          (setq nrepl-connection-buffer (buffer-name (process-buffer client-proc)))
          (setq jdee-live--response 'connected)))))
  ;; Check for errors
  (when (string-match "BUILD FAILURE" output)
    (setq jdee-live--response output))
  )



;;;###autoload
(define-minor-mode jdee-live-mode
  "Minor mode for JVM/Clojure interaction from a Java buffer."
  )

(provide 'jdee-live)
;;; jdee-live.el ends here
