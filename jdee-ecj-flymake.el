;;; jdee-ecj-flymake.el --- JDEE flymake support when using Eclipse compiler as a compile server.

;; Copyright (C) 2006-2007 by Suraj Acharya

;; Author: Suraj Acharya <sacharya@cs.indiana.edu>
;; Maintainer: Suraj Acharya <sacharya@cs.indiana.edu>
;; Created: 22 Feb 2006
;; Keywords: flymake jdee-mode java eclipse ecj
;; Version 0.2

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;;
;; * Eclipse java compiler
;;
;; This library adds support for using flymake when the eclipse java
;; compiler is selected as a compile server to `jdee-compiler'.
;;
;; Usage:
;;
;; To use this library, ensure that this file in your load path and
;; add the following code to your .emacs:
;; (require 'jdee-ecj-flymake)

;; * Flymake
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax
;; checks. It is included in Emacs 23 onwards. For earlier versions
;; you can get the latest version from
;; http://flymake.sourceforge.net/. It is setup to use the new
;; compile.el from Emacs 23 by default, so you might have to comment
;; out a line which references `compilation-error-regexp-alist-alist'
;; and uncomment the previous line which uses
;; `compilation-error-regexp-alist' instead.
;;
;; You have two options to hook the eclipse java compiler into flymake
;; to get automatic error/warning information in your source buffers:
;; (To use flymake with ecj you first have to select "eclipse java
;; compiler server" as your jdee compiler.)
;;
;; 1) jdee-ecj-flymake-init : This is the simple, robust and painfully slow
;;    method, in which flymake forks a new jvm process each time it
;;    decides to error check the buffer.
;;
;; To use this funtion set the java line in
;; `flymake-allowed-file-name-masks' to
;; (\"\\.java\\'\" jdee-ecj-flymake-init jdee-ecj-flymake-cleanup)"

;; 2) jdee-ecj-server-flymake-init: This method involves flymake
;;    sending a compile command to the eclipse java compiler server in
;;    the JDEE bsh process, detecting when the compiler is done
;;    printing errors and warnings and then handing control of the bsh
;;    process back to JDEE. This option is much faster than 1) and
;;    compares in speed the jikes flymake integration
;;    (http://www.emacswiki.org/cgi-bin/wiki/jdee-flymake.el) but with
;;    java 1.5 syntax support and a larger set of warn
;;    options. However, it requires some significant changes to
;;    `flymake-process-filter' and
;;    `flymake-start-syntax-check-process' and so is more likely to be
;;    flaky, and might cause problems if you use flymake with non-java
;;    buffers.
;;
;; To use this funtion set the java line in
;; `flymake-allowed-file-name-masks' to
;; (\"\\.java\\'\" jdee-ecj-server-flymake-init jdee-ecj-flymake-cleanup)"
;;
;; The default flymake behaviour is to change the background of error
;; lines to light pink, and for warning lines to light blue. Customize
;; the faces `flymake-errline' and `flymake-warnline' to change this
;; behavior. Red and yellow underlines for errors and warnings work
;; well:
;;
;; (custom-set-faces
;; ...
;;  '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
;;  '(flymake-warnline ((((class color)) (:underline "yellow"))))
;; ...

;;
;; ChangeLog
;;
;; NOW - Integrated directly into jdee
;; 0.3 - bug fixes from James Ahlborn <jahlborn@healthmarketscience.com>
;;       deleted unnecessary \n at the end of a bsh-eval string that was causing bsh-buffer-eval to fail occasionally.
;; 0.2 - Eclipse 3.2 and later support the -Xemacs option which makes
;;       it possible to the hook eclipse compiler into flymake.
;; 0.1 - Initial version

;;(eval-when-compile
;;  (require 'jdee-loader))

(require 'flymake)
(require 'jdee)
(require 'jdee-compile)
(require 'jdee-bsh)

;; pattern for matching eclipse compiler error/warning output
(defconst jdee-ecj-compiler-error-regexps
  '(
    ("\\([a-z0-9_./]+\\):\\([0-9]+\\): \\(\\([eE]rror\\|[wW]arning\\): \\(.+\\)\\)"
      1 2 nil 3)
    ))

(defcustom jdee-ecj-command-line-args '("-d" "none" "-source" "1.6" "-target" "1.6" "-warn:-serial")
  "*Specify Eclipse java complier options as a string of command-line arguments.
The value of this variable should be a list of switches
understood by the compiler, for example, -depend -g. This
variable is used by both `jdee-ecj-flymake-init' and
`jdee-ecj-server-flymake-init'. It defaults to use the java 1.6
syntax, and not generate class files during compilation.

See the section
for `Using the batch compiler' at
http://help.eclipse.org/help32/index.jsp?topic=/org.eclipse.jdt.doc.isv/guide/jdt_api_compile.htm
for a description of the eclipse batch compiler and a list of all
the `warn' options that it can take.
"
  :group 'jdee-compile-options
  :type '(repeat (string :tag "Argument:")))

;;
;; flymake invoking new ecj processes
;;

(defun jdee-ecj-flymake-init ()
  "Run the Eclipse Java compiler to collect flymake errors.
To use this funtion set the java line in `flymake-allowed-file-name-masks' to
  (\"\\.java\\'\" jdee-ecj-flymake-init jdee-ecj-flymake-cleanup)"
  (if (not (object-of-class-p (jdee-compile-get-the-compiler) 'jdee-compile-ejc-server))
      (error "The ecj option for flymake can only be set when the jdee-compiler is also set to ecj")
      (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                           'jdee-ecj-create-temp-file))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list "java" (append  (list "-jar" (oref  (jdee-compile-get-ejc) path) "-noExit" "-Xemacs")
			      (jdee-compile-classpath-arg (jdee-compile-get-the-compiler))
                              jdee-ecj-command-line-args
                              (list local-file))))))

(defun jdee-ecj-create-temp-file (file-name prefix)
  "Create the file FILE-NAME in a unique directory in the temp directory.
This function uses `random' to generate a \"unique\" directory
name. It doesn't just create the file in the temp directory to
prevent another emacs process on this same machine from trying to
use the same file.  PREFIX is ignored in this function as java
compilers want the temporary file to have the same name as the
orginal file."
  (file-truename (expand-file-name (file-name-nondirectory file-name)
                                   ;; jta: only return positive random numbers
                                   (expand-file-name  (int-to-string (random 67000000)) (flymake-get-temp-dir)))))

(defun jdee-ecj-flymake-cleanup ()
  "Cleanup after `flymake-ecj-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (jdee-ecj-flymake-delete-temp-directory
     (file-name-directory flymake-temp-source-file-name))))

(defun jdee-ecj-flymake-delete-temp-directory (dir-name)
  "Attempt to delete temp dir DIR-NAME created by `flymake-create-temp-with-folder-structure', do not fail on error."
  (let* ((true-dir-name (file-truename dir-name))
         (true-tmp-dir (file-truename (flymake-get-temp-dir))))
    (when (equal true-tmp-dir (substring true-dir-name 0 (length true-tmp-dir)))
      (while (not (equal true-tmp-dir true-dir-name))
        (mapc 'jdee-ecj-safe-delete-file (directory-files true-dir-name t))
        (flymake-safe-delete-directory true-dir-name)
        (setq true-dir-name (file-name-directory (directory-file-name true-dir-name)))))))

(defun jdee-ecj-safe-delete-file (file-name)
  (when (and file-name (file-exists-p file-name) (file-regular-p file-name))
    (delete-file file-name)
    (flymake-log 1 "deleted file %s" file-name)))

;;
;; flymake talking to the running ecj server process
;;

(defvar jdee-ecj-server-setup-called nil
  "A value of nil indicates that `jdee-ecj-server-setup' has not
yet been called for the current emacs session.")

(defun jdee-ecj-server-setup ()
  (defalias 'flymake-start-syntax-check-process 'jdee-ecj-flymake-start-syntax-check-process)
  (defalias 'flymake-process-filter 'jdee-ecj-flymake-process-filter)
  (bsh-eval (jdee-ecj-get-bsh)
            (format "addClassPath(\"%s\");\n" (oref (jdee-compile-get-the-compiler) :path)))
  (setq jdee-ecj-server-setup-called t))

(defun jdee-ecj-server-flymake-init ()
  "Run the Eclipse Java compiler to collect flymake errors.
To use this funtion set the java line in `flymake-allowed-file-name-masks' to
  (\"\\.java\\'\" jdee-ecj-server-flymake-init jdee-ecj-flymake-cleanup)"

  (unless jdee-ecj-server-setup-called
    (jdee-ecj-server-setup))

  (if (not (object-of-class-p (jdee-compile-get-the-compiler) 'jdee-compile-ejc-server))
      (error "The ecj option for flymake can only be set when the jdee-compiler is also set to ecj")
    ;; else
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'jdee-ecj-create-temp-file))
           (directory-sep-char ?/)
           (args (append (jdee-compile-classpath-arg (jdee-compile-get-the-compiler))
                         (list "-Xemacs" "-noExit")
                         jdee-ecj-command-line-args))
           (arg-array (concat "new String[] {"))
           (bsh-process (oref (oref  (jdee-ecj-get-bsh) buffer) process)))

      (flymake-log 3 "jdee-ecj-server-flymake-init temp-file=%s" temp-file)
      (flymake-log 3 "jdee-ecj-server-flymake-init flymake-temp-source-file-name=%s" flymake-temp-source-file-name)


      (unless (jdee-ecj-bsh-running-p)
        (bsh-launch (jdee-ecj-get-bsh))
        (bsh-eval (jdee-ecj-get-bsh) (jdee-create-prj-values-str)))

      (if args (setq arg-array (concat arg-array
                                       (mapconcat
                                        (lambda (arg)
                                          (concat "\"" arg "\"")) args ",")
                                       ",")))

      (setq arg-array (concat arg-array "\"" temp-file "\"}"))


      (list (cons bsh-process ;; server process
                  "jdee-eclipse-compiler-server-done")  ;; output end marker
            ;; compile command
            (concat (format
                     "(new org.eclipse.jdt.internal.compiler.batch.Main(new java.io.PrintWriter(System.out),new java.io.PrintWriter(System.err), true)).compile(%s);print (\"jdee-eclipse-compiler-server-done\");"
                     arg-array))))))


(defvar flymake-server-process-saved-buffer nil
"Original process buffer of the flymake server process. This is restored in `jdee-ecj-flymake-server-process-end'")

(defvar flymake-server-process-saved-filter nil
"Original process filter of the flymake server process. This is restored in `jdee-ecj-flymake-server-process-end'")

(defvar flymake-process-server-end-marker nil
  "When using a process server, this string in the process output
marks the end of the current set of compilations/warnings.")


(defun jdee-ecj-flymake-server-process-end (process output)
  "The equivalent of `flymake-process-sentinel' for flymake server processes.
This function is called by `flymake-process-filter' when it sees
the end of output marker `flymake-process-server-end-marker' in
the output stream."
  (let* ((source-buffer     (process-buffer process))
         (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer)))
         (flymake-err-line-patterns jdee-ecj-compiler-error-regexps))

    (flymake-log 2 "server process %d \"exited\" with output %s" (process-id process) output)
    (condition-case err
        (progn
          (flymake-log 3 "cleaning up using %s" cleanup-f)
          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer
              (funcall cleanup-f)))

          (setq flymake-processes (delq process flymake-processes))
          (set-process-buffer process flymake-server-process-saved-buffer)
          (set-process-filter process flymake-server-process-saved-filter)

          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer

              (flymake-parse-residual)
              (flymake-post-syntax-check 0 buffer-file-name)
              (setq flymake-is-running nil))))
      (error
       (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                              source-buffer (error-message-string err))))
         (flymake-log 0 err-str)
         (with-current-buffer source-buffer
           (setq flymake-is-running nil)))))))
;;
;; flymake modifications to allow it to converse with a running
;; process instead of always starting a new "make" process
;;
(defun jdee-ecj-flymake-start-syntax-check-process (cmd args dir)
  "Start syntax check process."
  (let* ((process nil))
    (condition-case err
	(progn
	  (when dir
	    (let ((default-directory dir))
	      (flymake-log 3 "starting process on dir %s" default-directory)))

          (cond
           ((and (listp cmd) (processp (car cmd)))
            (setq process (car cmd))
            (setq flymake-server-process-saved-filter (process-filter process))
            (setq flymake-server-process-saved-buffer (process-buffer process))
            (set-process-buffer process (current-buffer))
            (process-send-string process args)
            (setq flymake-process-server-end-marker (cdr cmd))
            (flymake-log 2 "sent command=%s, to process=%S"
                         args process))
           (t
            (setq process (apply 'start-process "flymake-proc" (current-buffer) cmd args))
            (set-process-sentinel process 'flymake-process-sentinel)
            (flymake-log 2 "started process %d, command=%s, dir=%s"
                         (process-id process) (process-command process)
                         default-directory)))

	  (set-process-filter process 'flymake-process-filter)
          (push process flymake-processes)

          (setq flymake-is-running t)
          (setq flymake-last-change-time nil)
          (setq flymake-check-start-time (flymake-float-time))

	  (flymake-report-status nil "*")

	  process)
      (error
       (let* ((err-str (format "Failed to launch syntax check process  with args : %s"
			       (error-message-string err)))
	      (source-file-name buffer-file-name)
	      (cleanup-f        (flymake-get-cleanup-function source-file-name)))
	 (flymake-log 0 err-str)
	 (funcall cleanup-f)
	 (flymake-report-fatal-status "PROCERR" err-str))))))


(defun jdee-ecj-flymake-process-filter (process output)
  "Parse OUTPUT and highlight error lines.
It is the flymake process filter. It is also responsible for
calling `jdee-ecj-flymake-server-process-end' if the process is a server
process and the output contains the end of output marker `flymake-process-server-end-marker'."
  (let ((source-buffer (process-buffer process))
        (flymake-err-line-patterns jdee-ecj-compiler-error-regexps))
    (flymake-log 3 "received %d byte(s) of output from process %d"
                 (length output) (process-id process))
    (flymake-log 3 "output : %s" output)
    (when source-buffer
      (with-current-buffer source-buffer
        (flymake-parse-output-and-residual output))))
  (if (and flymake-process-server-end-marker (string-match flymake-process-server-end-marker output))
      (jdee-ecj-flymake-server-process-end process output)))

(defclass jdee-ecj-bsh-buffer (bsh-comint-buffer) ()
  "ecj's beanshell buffer")

(defmethod initialize-instance ((this jdee-ecj-bsh-buffer) &rest fields)
  (oset this buffer-name "*ecj bsh*")
  (call-next-method))

(defclass jdee-ecj-bsh (jdee-bsh)
  ((the-ecj-bsh        :type jdee-ecj-bsh
                       :allocation :class
                       :documentation
                   "The single instance of the ecj's BeanShell."))
  "Class of ecj BeanShells. There is only one per Emacs session.")

(defmethod initialize-instance ((this jdee-ecj-bsh) &rest fields)
  "Constructor for the ecj BeanShell instance."
  (let ((the-jdee-bsh (oref-default 'jdee-bsh the-bsh)))
    (call-next-method)
    (oset-default 'jdee-ecj-bsh the-ecj-bsh this)
    ;; the jdee-bsh constructor sets the "the-bsh" static member, which needs
    ;; to be reset so we can keep two separate instances running
    (oset-default 'jdee-bsh the-bsh the-jdee-bsh)))

(defmethod bsh-create-buffer ((this jdee-ecj-bsh))
  "Creates the ecj's beanshell buffer."
  (oset this buffer (jdee-ecj-bsh-buffer "ecj bsh buffer")))

;; Create the ecj BeanShell wrapper object.
(jdee-ecj-bsh "ecj BeanShell")

(defun jdee-ecj-get-bsh ()
  (oref-default 'jdee-ecj-bsh the-ecj-bsh))

(defun jdee-ecj-bsh-running-p ()
  (bsh-running-p (jdee-ecj-get-bsh)))

(defun jdee-ecj-reset-bsh ()
  "Reset things when ecj bsh gets wedged"
  (interactive)
  (setq jdee-ecj-server-setup-called nil)
  (jdee-ecj-bsh-exit))

(defun jdee-ecj-bsh-exit ()
  "Closes the existing ecj beanshell process"
  (interactive)
  (if (jdee-ecj-bsh-running-p)
      (let ((process (bsh-get-process (jdee-ecj-get-bsh))))
        (process-send-string process "exit();\n"))
    (message "The ecj beanshell is not running")))


;; Use this for some extra debugging in flymake
;; (defun flymake-get-full-patched-file-name (file-name-from-err-msg
;; base-dirs files)
;;  (let* ((base-dirs-count  (length base-dirs))
;;         (file-count       (length files))
;;         (real-name        nil))

;;    (while (and (not real-name) (> base-dirs-count 0))
;;      (setq file-count (length files))
;;      (while (and (not real-name) (> file-count 0))
;;        (let* ((this-dir        (nth (1- base-dirs-count) base-dirs))
;;               (this-file       (nth 0 (nth (1- file-count) files)))
;;               (this-real-name  (nth 1 (nth (1- file-count) files))))
;;          (flymake-log 0 "this-dir=%s this-file=%s this-real=%s msg-file=%s"
;; this-dir this-file this-real-name file-name-from-err-msg)
;;          (when (and this-dir this-file (flymake-same-files
;;                                         (expand-file-name file-name-from-err-msg this-dir)
;;                                         this-file))
;;            (setq real-name this-real-name)))
;;        (setq file-count (1- file-count)))
;;      (setq base-dirs-count (1- base-dirs-count)))
;;    real-name))


(provide 'jdee-ecj-flymake)

;;; jdee-ecj-flymake.el ends here
