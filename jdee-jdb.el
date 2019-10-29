;;; jdee-jdb.el -- Debugger mode for jdb.

;; Author: Paul Kinnucan <paulk@mathworks.com>x
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 2000, 2001, 2002, 2003, 2004, 2005 Paul Kinnucan.
;; Copyright (C) 2009 by Paul Landes

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, US
;;; Commentary:

;; This package interfaces emacs to jdb, the debugger
;; distributed as part of JavaSoft's Java
;; Development Kit (JDK).

;;; Code:

(require 'jdee-db)
(require 'semantic/util-modes);; semantic-add-minor-mode

;; FIXME: refactor
(defvar jdee-debugger);; jde
(declare-function jdee-java-major-version "jdee" ())
(declare-function jdee-java-minor-version "jdee" ())
(declare-function jdee-get-jdk-prog "jdee" (progname))
(declare-function jdee-find-jdee-doc-directory "jdee" ())
(declare-function jdee-convert-cygwin-path "jdee" (path &optional separator))

;; Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for this function
;; and its use in jdee-db-marker-filter.
;; Amended by "Patrick J. McNerthney" <pat@mcnerthney.com> to allow
;; package names to begin with underscores.
(defun jdee-jdb-make-qualified-class-name-regexp (class)
  "Constructs a regular expression to extract a qualified class name from a jdb
breakpoint message."
  (concat "\\(\\(\\(\\(\\w\\|[_]\\)*\\.\\)*\\)" class "\\)\\(\\b\\|\\$\\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Debugger Commands                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Launch debuggee command


(defclass jdee-jdb-cmd-launch (jdee-db-cmd-launch) ()
  "Asks jdb to launch a debuggee process.")

(defmethod jdee-db-cmd-launch-buffer-name ((this jdee-jdb-cmd-launch))
  "Return the name of the buffer for this process. Descendant
classes should override this method to create a name that is appropriate
for the process being launched, e.g., an applet or application." nil)

(defmethod jdee-db-cmd-launch-cmd-path ((this jdee-jdb-cmd-launch))
  "Return the path of the command to be used to launch the process. Descendant
classes should override this method to return a path appropriate to
the command to be used to launch the debuggee process, e.g., jdb or
appletviewer." nil)


(defmethod jdee-db-cmd-launch-startup-cmds ((this jdee-jdb-cmd-launch))
  "Add commands to debugger's initial command queue. Derived classes
should override this method to specify commands that should be
executed immediately after the debugger starts, e.g., an initial
step command." nil)


(defmethod jdee-db-cmd-init ((this jdee-jdb-cmd-launch))
  "The debugger invokes this method before executing the launch
command. Launch the debuggee process."
  (let ((debugger (oref this debugger)))
    (if (or
         (not (slot-boundp debugger 'buffer))
         (not (oref debugger :buffer))
         (not (comint-check-proc (oref debugger :buffer))))
        (let* ((debuggee
                (oref debugger debuggee))
               (source-directory default-directory)
               (working-directory
                (jdee-db-debugger-get-working-dir debugger))
               (prog-args (jdee-db-debugger-get-prog-args debugger))
               (cmd-path (jdee-db-cmd-launch-cmd-path this))
               (command-string
                (concat
                 cmd-path " "
                 (jdee-run-make-arg-string prog-args) "\n\n")))

        (oset debugger :buffer-name (jdee-db-cmd-launch-buffer-name this))
        (oset debugger :buffer (get-buffer-create (oref debugger :buffer-name)))

        (jdee-db-cmd-launch-startup-cmds this)

        (oset debugger :path cmd-path)
        (jdee-db-jdb-start debugger prog-args command-string)

        (let ((debuggee-status (oref debuggee status)))
          (oset debuggee-status running-p t)
          (oset debuggee-status stopped-p t)))
    (progn
      (message "An instance of %s is running." (oref this :buffer-name))
      (pop-to-buffer (oref this :buffer-name))))))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-launch))
  "Returns nil because jdb launches the debuggee application automatically
when it is started." nil)


;; Launch application command

(defclass jdee-jdb-cmd-launch-app (jdee-jdb-cmd-launch
                                  jdee-db-cmd-launch-app) ()
  "Asks jdb to launch the debuggee application.")

(defmethod initialize-instance ((this jdee-jdb-cmd-launch-app) &rest fields)
  (call-next-method)
  (oset this name "launch application in jdb debug mode"))

(defmethod jdee-db-cmd-launch-cmd-path ((this jdee-jdb-cmd-launch-app))
  "Return the path of the jdb command."
  (let* ((debugger (oref this :debugger)))
    (oref debugger :path)))

(defmethod jdee-db-cmd-launch-buffer-name ((this jdee-jdb-cmd-launch-app))
  (let* ((debugger (oref this :debugger))
         (debuggee (oref debugger :debuggee))
         (main-class (oref debuggee :main-class)))
    (concat  "*debug"  main-class "*")))

(defmethod jdee-db-cmd-launch-startup-cmds ((this jdee-jdb-cmd-launch-app))
  "If `jdee-db-initial-step-p' is nonnil, add a step command to the
debugger's startup command queue."
  (if jdee-db-initial-step-p
      (let*  ((debugger (oref this debugger))
              (step-cmd (oref (oref debugger cmd-set) step-into)))
        (oset debugger next-cmd
                    (append (oref debugger next-cmd) (list step-cmd))))))


;; Launch applet command

(defclass jdee-jdb-cmd-launch-applet (jdee-jdb-cmd-launch
                                     jdee-db-cmd-launch-applet) ()
  "Asks jdb to launch the debuggee applet.")

(defmethod initialize-instance ((this jdee-jdb-cmd-launch-applet) &rest fields)
  (call-next-method)
  (oset this name "launch applet in jdb debug mode"))

(defmethod jdee-db-cmd-launch-cmd-path ((this jdee-jdb-cmd-launch-applet))
  "Return the path of the command to be used to launch the process. Descendant
classes should override this method to return a path appropriate to
the command to be used to launch the debuggee process, e.g., jdb or
appletviewer."
  (let* ((debugger (oref this :debugger))
         (jdb-path (oref debugger :path))
         (jdb-dir (file-name-directory jdb-path)))
    (expand-file-name "appletviewer" jdb-dir)))

(defmethod jdee-db-cmd-launch-buffer-name ((this jdee-jdb-cmd-launch-applet))
  (let* ((debugger (oref this :debugger))
         (debuggee (oref debugger :debuggee))
         (doc (oref debuggee :doc)))
    (concat  "*debug"  (file-name-nondirectory doc) "*")))

(defmethod jdee-db-cmd-launch-startup-cmds ((this jdee-jdb-cmd-launch-applet))
  "If `jdee-db-initial-step-p' is nonnil, add a run command followed by a
step command to the debugger's startup command queue."
  (if jdee-db-initial-step-p
      (let*  ((debugger (oref this debugger))
              (cmd-set (oref debugger cmd-set))
              (run-cmd (oref cmd-set run))
              (step-cmd (oref cmd-set step-into)))
        (oset debugger next-cmd
                    (append
                     (oref debugger next-cmd)
                     (list run-cmd)
                     (list step-cmd))))))



;; Run command

(defclass jdee-jdb-cmd-run (jdee-db-cmd) ()
  "Asks jdb to start the debuggee application.")

(defmethod initialize-instance ((this jdee-jdb-cmd-run) &rest fields)
  (call-next-method)
  (oset this name "run"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-run))
  "Creates command line for jdb run command."
  "run")

;; Cont command

(defclass jdee-jdb-cmd-cont (jdee-db-cmd) ()
  "Asks jdb to continue the debuggee application from its current
stopping point.")

(defmethod initialize-instance ((this jdee-jdb-cmd-cont) &rest fields)
  (call-next-method)
  (oset this name "cont"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-cont))
  "Creates command line for jdb cont command."
  "cont")

;; Quit command

(defclass jdee-jdb-cmd-quit (jdee-db-cmd) ()
  "Quit debugging the current application.")

(defmethod initialize-instance ((this jdee-jdb-cmd-quit) &rest fields)
  (call-next-method)
  (oset this name "quit"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-quit))
  "Creates command line for jdb quit command."
  "quit")

;; Step-over command

(defclass jdee-jdb-cmd-step-over (jdee-db-cmd) ()
  "Step to the next line in the current frame.")

(defmethod initialize-instance ((this jdee-jdb-cmd-step-over) &rest fields)
  (call-next-method)
  (oset this name "next"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-step-over))
  "Creates command line for jdb step-over command."
  "next")

;; Step-into command

(defclass jdee-jdb-cmd-step-into (jdee-db-cmd) ()
  "Step to the next line in the current program.")

(defmethod initialize-instance ((this jdee-jdb-cmd-step-into) &rest fields)
  (call-next-method)
  (oset this name "step"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-step-into))
  "Creates command line for jdb step-into command."
  "step")

;; Step-out command

(defclass jdee-jdb-cmd-step-out (jdee-db-cmd) ()
  "Continue to the end of the current method.")

(defmethod initialize-instance ((this jdee-jdb-cmd-step-out) &rest fields)
  (call-next-method)
  (oset this name "step up"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-step-out))
  "Creates command line for jdb step-out command."
  "step up")


;; Up stack command

(defclass jdee-jdb-cmd-up (jdee-db-cmd) ()
  "Move up one stack frame.")

(defmethod initialize-instance ((this jdee-jdb-cmd-up) &rest fields)
  (call-next-method)
  (oset this name "up"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-up))
  "Creates command line for jdb up command."
  "up")

(defmethod jdee-db-cmd-notify-response ((this jdee-jdb-cmd-up) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response. This method invokes the jdb where
command in order to position the debug pointer at the
current stack location."
  ;; (jdee-debug-where)
  (let* ((jdb (oref this debugger))
         (cmds (oref jdb cmd-set))
         (cmd (oref cmds where)))
    (jdee-db-exec-cmd jdb cmd)))

;; Down stack command

(defclass jdee-jdb-cmd-down (jdee-db-cmd) ()
  "Move down one stack frame.")

(defmethod initialize-instance ((this jdee-jdb-cmd-down) &rest fields)
  (call-next-method)
  (oset this name "down"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-down))
  "Creates command line for jdb down command."
  "down")

(defmethod jdee-db-cmd-notify-response ((this jdee-jdb-cmd-down) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response. This method invokes the jdb where
command in order to position the debug pointer at the
current stack location."
  ;;(jdee-debug-where)
  (let* ((jdb (oref this debugger))
         (cmds (oref jdb cmd-set))
         (cmd (oref cmds where)))
    (jdee-db-exec-cmd jdb cmd)))


;; Where stack command

(defclass jdee-jdb-cmd-where (jdee-db-cmd) ()
  "Point to current location on the stack.")

(defmethod initialize-instance ((this jdee-jdb-cmd-where) &rest fields)
  (call-next-method)
  (oset this name "where"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-where))
  "Creates command line for jdb where command."
  "where")

(defmethod jdee-db-cmd-notify-response ((this jdee-jdb-cmd-where) output)
"Processes the output of the jdb where
 command, which lists the current stack. An example of the output
 is

      [1] jmath.LinearSystem$InnerClass.print (LinearSystem.java:36)
      [2] jmath.LinearSystem.<init> (LinearSystem.java:52)
      [3] jmath.Test.main (Test.java:38)

 This method positions the source line cursor at the position that
 matches the current location of the debugger in the program's
 stack (set by the jdb up and down stack commands)."
  (let* ((jdb (oref this debugger))
         (debuggee (oref jdb debuggee)))
    ;; if the stack depth is not set default to 1
    (if (string-equal "" (oref debuggee :stack-depth))
        (oset debuggee :stack-depth "1"))
    (if (string-match
         (concat "^  \\["
                 (oref debuggee :stack-depth)
                 "\\] .*(\\([^\$\n]*\\).*:\\([0-9]*[^[:digit:]]?[0-9]+\\))")
         output)
        (let ((marker (match-string 0 output))
              (class (match-string 1 output))
              (line-no (jdee-jdb-string-to-int (match-string 2 output)))
              (package ""))

          (if (equal ".java" (substring class -5))
              (setq class (substring class 0 -5)))

          ;; Extract package path from input.
          (let ((case-fold-search nil))        ;; Make sure search is case-sensitive
            (and (string-match (jdee-jdb-make-qualified-class-name-regexp class) marker)
                 (setq package
                       (substring marker (match-beginning 2) (match-end 2)))))
          (jdee-db-set-debug-cursor
           (if package (concat package class) class)
           (concat class ".java") line-no)))
    output))

;; Set Breakpoint command

(defclass jdee-jdb-cmd-set-breakpoint (jdee-db-cmd-breakpoint) ()
  "Asks jdb to set the breakpoint specified by the
breakpoint field.")

(defmethod initialize-instance ((this jdee-jdb-cmd-set-breakpoint) &rest fields)
  (call-next-method)
  (oset this name "stop at"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-set-breakpoint))
  "Creates command line for jdb set breakpoint command."
  (let* ((bps (oref this breakpoints))
         (bp (car bps)))
    (format "stop at %s:%d"
            (oref bp class)
            (jdee-db-breakpoint-get-line bp))))

(defmethod jdee-db-cmd-notify-response ((this jdee-jdb-cmd-set-breakpoint) output)
  "Called when the debugger responds to the last set-breakpoint
  command. Invokes `jdee-db-mark-breakpoint-requested' on the breakpoint and
updates the breakpoint to `requested' status.  Removes the breakpoint
from the command's breakpoint list. If the list contains more
breakpoints, this method reissues the command on the next breakpoint
on the list."
  ;; (message "set-bp resp <<%s>>" output)
  (if (or
       (string-match "Deferring breakpoint" output)
       (string-match "Set breakpoint" output)
       (string-match "Unable to set" output))
      (let* ((bps (oref this breakpoints))
             (bp (car bps)) file line)
        (if (not (null bp))
            (progn
              (setq file (oref bp file))
              (setq line (jdee-db-breakpoint-get-line bp))
              (if (string-match "Unable to set breakpoint" output)
                  (jdee-db-delete-breakpoint bp)
                (if (string-match "Unable to set deferred breakpoint" output)
                    (if (jdee-db-debuggee-running-p)
                        (let* ((debugger (oref-default 'jdee-db-debugger the-debugger))
                               (bp-cmd
                                (oref (oref debugger cmd-set) clear-bp)))
                          (oset bp-cmd breakpoints (list bp))
                          (jdee-db-exec-cmd debugger bp-cmd))
                      (jdee-db-delete-breakpoint bp))
                  (if (string-match "Deferring breakpoint" output)
                      (progn
                        (oset bp status 'requested)
                        (jdee-db-mark-breakpoint-requested file line))
                    (if (string-match "Set breakpoint" output)
                        (progn
                          (oset bp status 'active)
                          (jdee-db-mark-breakpoint-active file line))))))
              (setq bps (cdr bps))
              (oset this breakpoints bps)
              (if bps
                  (let ((jdb (oref this debugger)))
                    (jdee-db-exec-cmd jdb this))))))))

;; Clear Breakpoint command

(defclass jdee-jdb-cmd-clear-breakpoint (jdee-db-cmd-breakpoint) ()
  "Asks jdb to clear the breakpoint specified by the
breakpoint field.")

(defmethod initialize-instance ((this jdee-jdb-cmd-clear-breakpoint) &rest fields)
  (call-next-method)
  (oset this name "clear"))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-clear-breakpoint))
  "Creates command line for jdb clear breakpoint command."
  (let* ((bps (oref this breakpoints))
         (bp (car bps)))
    (format "clear %s:%d"
            (oref bp class)
            (jdee-db-breakpoint-get-line bp))))

(defmethod jdee-db-cmd-notify-response ((this jdee-jdb-cmd-clear-breakpoint) output)
  "Called when the debugger responds to the last clear-breakpoint command.
Removes the breakpoint from the command's breakpoint list. If the list contains
more breakpoints, this method reissues the clear command on the next breakpoint
on the list."
  (let* ((bps (oref this breakpoints))
         (bp (car bps)))
    (jdee-db-delete-breakpoint bp)
    (setq bps (cdr bps))
    (oset this breakpoints bps)
    (if bps
        (let ((jdb (oref this debugger)))
          (jdee-db-exec-cmd jdb this)))))

;; Print command
(defvar jdee-jdb-cmd-print-history  nil)

(defclass jdee-jdb-cmd-print (jdee-db-cmd)
  ((expr        :initarg :expr
                :type string
                :initform ""
                :documentation
                "Expression passed to jdb"))
  "Asks jdb to print value of expression at point")

(defmethod initialize-instance ((this jdee-jdb-cmd-print) &rest fields)
  (call-next-method)
  (oset this name "print")
  (oset this expr ""))

(defmethod jdee-db-cmd-init ((this jdee-jdb-cmd-print))
  "The debugger invokes this method before executing the
command."
  (oset this expr (read-from-minibuffer "expr: " (thing-at-point 'word)
                                        nil nil 'jdee-jdb-cmd-print-history)))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-print))
  "Creates a command line for jdb print command."
  (format "%s %s"
          (oref this name)
          (oref this expr)))

;; Dump command
(defclass jdee-jdb-cmd-dump (jdee-jdb-cmd-print) ()
  "Asks jdb to print all object information of the expression at point")

(defmethod initialize-instance ((this jdee-jdb-cmd-dump) &rest fields)
  (call-next-method)
  (oset this name "dump"))

;; Eval command
(defclass jdee-jdb-cmd-eval (jdee-jdb-cmd-print) ()
  "Ask jdb to evaluate the expression(Same as the print command)")

(defmethod initialize-instance ((this jdee-jdb-cmd-eval) &rest fields)
  (call-next-method)
  (oset this name "eval"))

;; Set command
(defclass jdee-jdb-cmd-set-var (jdee-jdb-cmd-print)
  ((value :initarg :value
          :type string
          :initform "null"
          :document
          "Value to assign to the variable"))
  "Ask jdb to assign new value to a field/variable/array element")

(defmethod initialize-instance ((this jdee-jdb-cmd-set-var) &rest fields)
  (call-next-method)
  (oset this name "set"))

(defmethod jdee-db-cmd-init ((this jdee-jdb-cmd-set-var))
  "The debugger invokes this method before executing the
command."
  (oset this expr (read-from-minibuffer "variable: " (thing-at-point 'word)
                                        nil nil 'jdee-jdb-cmd-print-history))
  (oset this value (read-from-minibuffer "value: " nil
                                        nil nil '(null))))

(defmethod jdee-db-cmd-make-command-line ((this jdee-jdb-cmd-set-var))
  "Creates a command line for jdb print command."
  (format "%s %s = %s"
          (oref this name)
          (oref this expr)
          (oref this value)))

;; Locals commands
(defclass jdee-jdb-cmd-locals (jdee-jdb-cmd-print) ()
  "Ask jdb to print al local variables in current stack frame")

(defmethod initialize-instance ((this jdee-jdb-cmd-locals) &rest fields)
  (call-next-method)
  (oset this name "locals"))

(defmethod jdee-db-cmd-init ((this jdee-jdb-cmd-locals))
  "The debugger invokes this method before executing the
command."
  (oset this expr ""))

;; jdb Command Set

(defclass jdee-jdb-cmd-set (jdee-db-cmd-set)
  ((print :initarg :print
          :type jdee-jdb-cmd-print
          :documentation
          "Asks jdb to print the value of expression at point")
   (dump :initarg :dump
         :type jdee-jdb-cmd-dump
         :documentation
         "Ask jdb to print all object information from the
expression at poing")
   (eval :initarg :eval
         :type jdee-jdb-cmd-eval
         :documentation
         "Ask jdb to evaluate the expression at point")
   (set-var :initarg :set-var
            :type jdee-jdb-cmd-set-var
            :documentation
            "Ask jdb to assign a new value to the expression at point")
   (locals :initarg :locals
         :type jdee-jdb-cmd-locals
         :documentation
         "Ask jdb to print all local variables in current stack frame")
   )
  "Set of debugger commands implemented by jdb.")

(defmethod initialize-instance ((this jdee-jdb-cmd-set) &rest fields)
  "Construct jdb command set."
  (call-next-method)
  (let ((jdb (oref this debugger)))
    (oset this launch-app
          (jdee-jdb-cmd-launch-app "launch" :debugger jdb))
    (oset this launch-applet
          (jdee-jdb-cmd-launch-applet "launch" :debugger jdb))
    (oset this run
          (jdee-jdb-cmd-run "run" :debugger jdb))
    (oset this cont
          (jdee-jdb-cmd-cont "cont" :debugger jdb))
    (oset this quit
          (jdee-jdb-cmd-quit "jdb quit" :debugger jdb))
    (oset this step-over
          (jdee-jdb-cmd-step-over "jdb step-over cmd" :debugger jdb))
    (oset this step-into
          (jdee-jdb-cmd-step-into "jdb step-into cmd" :debugger jdb))
    (oset this step-out
          (jdee-jdb-cmd-step-out "jdb step-out cmd" :debugger jdb))
    (oset this up
          (jdee-jdb-cmd-up "jdb up cmd" :debugger jdb))
    (oset this down
          (jdee-jdb-cmd-down "jdb down cmd" :debugger jdb))
    (oset this where
          (jdee-jdb-cmd-where "jdb where cmd" :debugger jdb))
    (oset this set-bp
          (jdee-jdb-cmd-set-breakpoint "jdb set breakpoint" :debugger jdb))
    (oset this clear-bp
          (jdee-jdb-cmd-clear-breakpoint "jdb clear breakpoint" :debugger jdb))
    (oset this print
          (jdee-jdb-cmd-print "jdb print cmd" :debugger jdb))
    (oset this dump
          (jdee-jdb-cmd-dump "jdb dump cmd" :debugger jdb))
    (oset this eval
          (jdee-jdb-cmd-eval "jdb eval cmd" :debugger jdb))
    (oset this set-var
          (jdee-jdb-cmd-set-var "jdb set cmd" :debugger jdb))
    (oset this locals
          (jdee-jdb-cmd-locals "jdb locals cmd" :debugger jdb))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Breakpoint Listener                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-jdb-breakpoint-listener (jdee-db-listener)
  ((marker-regexp   :initarg :marker-regexp
                    :type string
                    :documentation
                    "Regular expression for parsing breakpoint messages.")
   (class-index     :initarg :class-index
                    :type integer
                    :initform 3
                    :documentation
                    "Index of class name parsed by marker-regex")
   (line-index      :initarg :line-index
                    :type integer
                    :initform 5
                    :documentation
                    "Index of line number parsed by marker-regex")
   (noline-regexp   :initarg :noline-regexp
                    :type string
                    :documentation
                    "Regular expression for parsing breakpoint messages without line numbers.")
   ;; There's no guarantee that Emacs will hand the filter the entire
   ;; marker at once; it could be broken up across several strings.  We
   ;; might even receive a big chunk with several markers in it.  If we
   ;; receive a chunk of text which looks like it might contain the
   ;; beginning of a marker, we save it here between calls to the
   ;; filter.
   (marker-acc      :initarg :marker-acc
                    :type string
                    :initform ""
                    :documentation
                    "Debug output accumulator")

   )
  "Handles jdb breakpoint events.")


(defmethod initialize-instance ((this jdee-jdb-breakpoint-listener) &rest fields)
  "Construct breakpoint listener."

  (call-next-method)

  ;; Regular expression used to find a jdb breakpoint position marker.
  ;; The regular expression must have two subexpressions. The first matches
  ;; the name of the class in which the breakpoint occurs; the second, the
  ;; line number at which the breakpoint occurs. The default expression
  ;; matches breakpoint messages emitted by jdb. You may need to change
  ;; the expression to accommodate other debuggers."
  (oset
   this
   :marker-regexp
   "^.*: thread=.*, \\(\\(.*[.]\\)*\\)\\([^\$]*\\)\\(\$.*\\)*[.].+(), line=\\([0-9,. ]*\\),")
  ;; Regular expression to match a breakpoint message that lacks a line
  ;; number because the breakpoint occurs in a class compiled without deug
  ;; information.
  (oset
   this
   :noline-regexp
   "^Breakpoint hit: .*(pc \\([0-9]*\\))"))



(defmethod jdee-jdb-fixup-output ((this jdee-jdb-breakpoint-listener))
    ;; This is a hack to accommodate reorder of message chunks
    ;; on Solaris at debugger startup.
    (if (string-match "running ...\n" (oref this :marker-acc))
        (oset this :marker-acc
              (concat "running ...\n"
                      (substring (oref this :marker-acc) 0 (match-beginning 0))
                      (substring (oref this :marker-acc) (match-end 0)))))


    ;; This is a hack to fix reordering of message chunks on Windows 2000
    ;; The problem is the debugger prompt - the thread name with the stack
    ;; depth (eg main[1]) - sometimes shows up in the middle of the output
    ;; from the command sent to the debugger.
    ;; This seems to show up most often with step commands.
    ;;(message "checking string %s" (oref jdb :marker-acc))
    (if (string-match "^.*: \\([-a-zA-Z0-9_$]+\\[[0-9]+\\] \\)thread="
                      (oref this :marker-acc))
        (oset this :marker-acc
              (concat (match-string 1 (oref this :marker-acc))
                      (substring (oref this :marker-acc) 0 (match-beginning 1))
                      (substring (oref this :marker-acc) (match-end 1)))))
    ;; (message "fixed string is %s" jdee-db-marker-acc)
    )


(defmethod jdee-jdb-set-breakpoint-listener ((this jdee-jdb-breakpoint-listener) output)
  "Listens for set breakpoint messages."
  (let ((msgs (split-string output "\n")))
    (loop for msg in msgs do
          (if (and (string-match
                    "^.*Set .*breakpoint \\(.*\\):\\([0-9]+\\)"
                    msg)
                   (not (string-match "Unable to set.*" msg)))
              (let* ((class (substring
                             msg
                             (match-beginning 1)
                             (match-end 1)))
                     (line (string-to-number
                            (substring
                             msg
                             (match-beginning 2)
                             (match-end 2))))
                     (source-buffer (jdee-db-find-class-source class))
                     (path (buffer-file-name source-buffer))
                     (bp (jdee-db-find-breakpoint path line)))
                (oset bp status 'active)
                (jdee-db-mark-breakpoint-active path  line))))))

(defmethod jdee-db-listener-filter-output ((this jdee-jdb-breakpoint-listener) input)
  "Filters the output of the debugger."
  (let ((jdb (oref this debugger))
        (output ""))

    ;; Accumulate next chunk of debugger output.
    (oset this
          :marker-acc (concat
                       (oref this :marker-acc)
                       (string-make-unibyte input)))

    ;; (message (format "<acc-start>%s<acc-end>" (oref this :marker-acc)))

    (jdee-jdb-fixup-output this)

    (let* ((marker-regexp (oref this :marker-regexp))
           (marker-regexp-class-index (oref this :class-index))
           (marker-regexp-line-index (oref this :line-index)))

      ;; (message (concat "jdb output:" input))
      ;; (message (concat "acc = " jdee-db-marker-acc))

      ;; Process all the complete markers in this chunk.
      (if (string-match marker-regexp (oref this :marker-acc))
          ;; Extract the frame position from the marker.
          (let ((premarker (substring
                            (oref this :marker-acc) 0 (match-beginning 0)))
                (marker (substring (oref this :marker-acc)
                                   (match-beginning 0) (match-end 0)))
                (rest (substring (oref this :marker-acc) (match-end 0)))
                (class (substring
                        (oref this :marker-acc)
                        (match-beginning marker-regexp-class-index)
                        (match-end marker-regexp-class-index)))
                (line-no (jdee-jdb-string-to-int
                          (substring
                           (oref this :marker-acc)
                           (match-beginning marker-regexp-line-index)
                           (match-end marker-regexp-line-index))))
                (package ""))
            ;; Extract package path from input.
            (let ((case-fold-search nil)) ;; Make sure search is case-sensitive
              (and (string-match (jdee-jdb-make-qualified-class-name-regexp class) marker)
                   (setq package
                         (substring marker (match-beginning 2) (match-end 2))))

               ;; (message "jdee-db package: %s. marker = %s" jdee-db-last-package marker)
               ;;(message "case-fold-search = %s" (if case-fold-search "true" "false"))
              )

            ;; Insert debugger output into debugger buffer.
            (setq output (concat premarker marker))

            ;; Set the accumulator to the remaining text.
            (oset this :marker-acc rest)

            (jdee-db-set-debug-cursor
             (concat package class) (concat class ".java") line-no)

            (let* ((debuggee (oref jdb debuggee))
                   (status (oref debuggee status)))
              (oset status stopped-p t)))))

   ;; Handle case where there is no line number info in current class.
    (if (string-match (oref this noline-regexp) (oref this marker-acc))
        (let ((premarker (substring
                          (oref this :marker-acc) 0 (match-beginning 0)))
              (marker (substring (oref this :marker-acc)
                                 (match-beginning 0) (match-end 0)))
              (pc (substring (oref this :marker-acc)
                             (match-beginning 1) (match-end 1)))
              (rest (substring (oref this :marker-acc) (match-end 0))))

          (setq output (concat premarker marker))
          (oset this :marker-acc rest)))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\\(^Breakpoint hit:\\)\\|\\(^Step completed:\\)"
                      (oref this :marker-acc))
        (progn
        ;; Everything before the potential marker start can be output.
          (setq output (concat output
                               (substring (oref this :marker-acc)
                                          0 (match-beginning 0))))

          ;; Everything after, we save, to combine with later input.
          (oset this
                :marker-acc
                (substring (oref this :marker-acc) (match-beginning 0))))
      (setq output
            (concat output (oref this :marker-acc)))
      (oset this :marker-acc ""))

    (jdee-jdb-set-breakpoint-listener this output)
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Stack Listener                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-jdb-stack-listener (jdee-db-listener)
  ((stack-depth     :initarg :stack-depth
                    :type string
                    :initform ""
                    :documentation
                    "Stack depth."))
  "Listens for changes in the current stack frame.")


;; Thanks to Michael Ernst <mernst@cs.washington.edu> for the following
;; stack-related code.
;;
;; Extract the index of the current stack frame from the jdb prompt, where
;; the prompt is of the form
;;
;;   thread[stack_index]
;;
;; e.g.,
;;
;;   main[1]
;;
;; The user can move the debugger up and down the stack via the up and
;; down commands. The debugger indicates the current location by the
;; stack index portion of its prompt.
(defmethod jdee-db-listener-filter-output ((this jdee-jdb-stack-listener) output)
  (let* ((jdb (oref this debugger))
         (debuggee (oref jdb debuggee)))
    (if (string-match "^[-a-zA-Z0-9_$ -]+\\[\\([0-9]*,?[0-9]+\\)\\] " output)
        (oset debuggee :stack-depth (match-string 1 output)))
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Application Debuggee                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-jdb-debuggee-app (jdee-db-debuggee-app) ()
  "Application process being debugged with jdb.")

(defmethod initialize-instance ((this jdee-jdb-debuggee-app) &rest fields)
  "Constructs an instance of a jdb debuggee."
  (call-next-method)
  (oset  this  status  (jdee-db-debuggee-status "jdb status")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Applet Debuggee                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-jdb-debuggee-applet (jdee-db-debuggee-applet) ()
  "Application process being debugged with jdb.")

(defmethod initialize-instance ((this jdee-jdb-debuggee-applet) &rest fields)
  "Constructs an instance of a jdb debuggee."
  (call-next-method)
  (oset  this  status  (jdee-db-debuggee-status "jdb status")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Class of JDE Debuggers based on jdb.                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-db-jdb (jdee-db-debugger)
  ((exec-name       :initarg :exec-name
                    :type string
                    :initform "jdb"
                    :documentation
                    "Name of the jdb executable.")
   (path            :initarg :path
                    :type string
                    :initform "jdb"
                    :documentation
                    "Path of the jdb executable.")
   (bp-listener     :initarg :bp-listener
                    :type jdee-jdb-breakpoint-listener
                    :documentation "Breakpoint listener."))
  (:allow-nil-initform t)
"Class of generic jdb debuggers")

(defmethod initialize-instance ((this jdee-db-jdb) &rest fields)
  "Constructor for generic jdb debugger."
  (call-next-method)
  (oset this :name "jdb")

  ;; Install jdb versions of debugger commands.
  (oset this cmd-set (jdee-jdb-cmd-set "jdb commands" :debugger this))

  (oset this bp-listener
   (jdee-jdb-breakpoint-listener
    "jdb breakpoint listener"
    :debugger this))

  (jdee-db-add-listener this (oref this bp-listener))

  (jdee-db-add-listener
   this
   (jdee-jdb-stack-listener
    "jdb stack listener"
    :debugger this)))

(defmethod jdee-db-create-debuggee-app ((this jdee-db-jdb) main-class)
  (oset
   this
   :debuggee (jdee-jdb-debuggee-app
              (concat "Application: " main-class)
              :main-class main-class)))

(defmethod jdee-db-create-debuggee-applet ((this jdee-db-jdb) applet-doc)
  (oset
   this
   :debuggee (jdee-jdb-debuggee-applet
              (concat "Applet: " applet-doc)
              :doc applet-doc)))

(defmethod jdee-db-jdb-start ((this jdee-db-jdb) prog-args cmdstr)
  "Start the debugger."
  (let ((w32-quote-process-args ?\")
        (win32-quote-process-args ?\") ;; XEmacs
        (source-directory default-directory)
        (working-directory
         (jdee-db-debugger-get-working-dir this)))

    (oset this :buffer (get-buffer-create (oref this :buffer-name)))

    (with-current-buffer (oref this :buffer)
      ;; Do not erase the last transcript; user may wish to view it.
      ;; (erase-buffer)
      (goto-char (point-max))
      (cd working-directory)
      (insert (concat "cd " working-directory "\n"))
      (insert cmdstr)
      (comint-mode)
      (make-local-variable 'comint-prompt-regexp)
      (setq comint-prompt-regexp "\\(^> *\\)\\|\\(^.*\\[[0-9]+\\] *\\)")
      (make-local-variable 'paragraph-start)
      (setq paragraph-start comint-prompt-regexp)

      (let ((process-connection-type nil))
       (comint-exec (oref this :buffer)
                   (oref this :buffer-name)
                   (oref this :path)
                   nil
                   prog-args))

      (oset this process
            (get-buffer-process (oref this buffer)))

      (cd source-directory)

      (jdee-db-set-process-filter this)
      (jdee-db-set-process-sentinel this)
      (run-hooks 'jdee-jdb-mode-hook)
      (pop-to-buffer (oref this buffer))

      (oset-default 'jdee-db-debugger the-debugger this)
      (oset this running-p t))))


(defmethod jdee-jdb-connect ((this jdee-db-jdb))
  "Connect the debugger to an existing process."
  (if (or
       (not (slot-boundp this 'buffer))
       (not (oref this :buffer))
       (not (comint-check-proc (oref this :buffer))))
      (let* ((debuggee (oref this debuggee))
             (source-directory default-directory)
             (connector (oref debuggee connector))
             (working-directory
              (jdee-db-debugger-get-working-dir this))
             (prog-args
              (if (typep connector 'jdee-db-listen-connector)
                  (if (typep connector 'jdee-db-socket-connector)
                      (list
                       "-connect"
                       (format
                        "com.sun.jdi.SocketListen:port=%s"
                        (oref connector port)))
                    (if (typep connector 'jdee-db-shared-memory-connector)
                        (list
                         "-connect"
                         (format
                          "com.sun.jdi.SharedMemoryListen:name=%s"
                          (oref connector name)))
                      (error "Invalid connector type.")))
                (if (typep connector 'jdee-db-attach-connector)
                    (if (typep connector 'jdee-db-socket-connector)
                        (let ((host (oref connector host))
                              (port (oref connector port)))
                          (if host
                              (list
                               "-connect"
                               (format
                                "com.sun.jdi.SocketAttach:hostname=%s,port=%s"
                                host port))
                          (list
                           "-connect"
                           (format
                            "com.sun.jdi.SocketAttach:port=%s"
                            port))))
                    (if (typep connector 'jdee-db-shared-memory-connector)
                        (list
                         "-connect"
                         (format
                          "com.sun.jdi.SharedMemoryAttach:name=%s"
                          (oref connector name)))
                      (error "Invalid connector type."))))))
             (command-string
              (format "%s %s\n\n"
               (oref this :path)
               (mapconcat (lambda (x) x) prog-args " "))))

        (oset
         this
         :buffer-name
         (if (typep connector 'jdee-db-shared-memory-connector)
             (format "*debug %s* debugee-shmem-name" (oref connector name))
           (format
            "*debug %s:%s*"
            (if (or (typep connector 'jdee-db-listen-connector)
                    (not (oref connector port)))
                "localhost" (oref connector host))
            (oref connector port))))

        (oset this :buffer (get-buffer-create (oref this :buffer-name)))

        ;; Forward to the debugger any breakpoint requests made
        ;; by the user before launching the application.
        (if jdee-db-breakpoints
            (let ((bp-cmd (oref (oref this cmd-set) set-bp)))
              (oset
               bp-cmd
               breakpoints
               (mapcar (lambda (assoc-x) (cdr assoc-x)) jdee-db-breakpoints))

              (oset this next-cmd
                    (append (oref this next-cmd) (list bp-cmd)))))

        (jdee-db-jdb-start this prog-args command-string)

        (let* ((debuggee (oref this debuggee))
               (debuggee-status (oref debuggee status)))
          (oset debuggee-status running-p t)
          (oset debuggee-status stopped-p t)))
    (progn
      (message "An instance of %s is running." (oref this :buffer-name))
      (pop-to-buffer (oref this :buffer-name)))))


(defmethod jdee-db-notify-process-exit ((this jdee-db-jdb) msg)
  "The default debugger process sentinel invokes this method
when the jdb process terminates."
  (call-next-method)
  (let* ((debuggee (oref this debuggee))
         (debuggee-status (oref debuggee status)))
    (oset this running-p nil)
    (oset debuggee-status running-p nil)
    (oset debuggee-status stopped-p nil)
    (jdee-db-set-all-breakpoints-specified)))

(defmethod jdee-db-launch-arg ((this jdee-db-jdb))
  "Generate the -launch option for jdb."
  (list "-launch"))


(defmethod jdee-db-debugger-get-prog-args ((this jdee-db-jdb))
  (cond
   ((typep (oref this debuggee) 'jdee-db-debuggee-app)
    (append
     (jdee-db-get-vm-args this)
     (jdee-db-get-vm-args-from-user)
     (list (oref (oref this debuggee) main-class))
     jdee-db-option-application-args
     (jdee-db-get-app-args-from-user)))
   ((typep (oref this debuggee) 'jdee-db-debuggee-applet)
    (list "-debug"
          (oref (oref this debuggee) doc)))
   (t
    (error "Unrecognized jdb debuggee type."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.1.x Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jdee-db-jdb-1-1 (jdee-db-jdb)
  ()
  (:allow-nil-initform t)
"Class of jdb shipped with JDK 1.1.x.")


(defmethod initialize-instance ((this jdee-db-jdb-1-1) &rest fields)
  "Constructor for jdb-1.1."
  (call-next-method)
  (oset (oref this bp-listener)
   :marker-regexp
   "^Breakpoint hit: .*(\\([^\$]*\\).*:\\([0-9]*\\))")
  (oset (oref this bp-listener) :class-index 1)
  (oset (oref this bp-listener) :line-index 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.3.1 Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-db-old-jdb (jdee-db-jdb-1-1)
  ()
  (:allow-nil-initform t)
"Class of pre-JPDA jdb shipped with post-JPDA versions of the
JDK.")

(defmethod initialize-instance ((this jdee-db-old-jdb) &rest fields)
  "Constructor for old jdb."

  (call-next-method)
  (oset this :exec-name "oldjdb"))


(defclass jdee-db-jdb-1-3 (jdee-db-jdb)
  ()
  (:allow-nil-initform t)
  "Class of JPDA-based jdb shipped with JDK 1.3.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.4.0 Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-db-jdb-1-4 (jdee-db-jdb)
  ()
  (:allow-nil-initform t)
  "Class of JPDA-based jdb shipped with J2SDK 1.4")

(defmethod initialize-instance ((this jdee-db-jdb-1-4) &rest fields)
  "Constructor for jdb-1.4."
  (call-next-method)
  ;; Regular expression used to find a jdb breakpoint position marker.
  ;; The regular expression has two subexpressions. The first matches
  ;; the name of the class in which the breakpoint occurs; the second, the
  ;; line number at which the breakpoint occurs."
  (oset (oref this bp-listener)
   :marker-regexp
   "^.*: \"thread=.*\", \\(\\(.*[.]\\)*\\)\\([^$]*\\)\\($.*\\)*[.].+(), line=\\([0-9,.]*\\)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.6.0 Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-db-jdb-1-6 (jdee-db-jdb)
  ()
  (:allow-nil-initform t)
  "Class of JPDA-based jdb shipped with J2SDK 1.6")

(defmethod initialize-instance ((this jdee-db-jdb-1-6) &rest fields)
  "Constructor for jdb-1.6."
  (call-next-method)
  ;; Regular expression used to find a jdb breakpoint position marker.
  ;; The regular expression has two subexpressions. The first matches
  ;; the name of the class in which the breakpoint occurs; the second, the
  ;; line number at which the breakpoint occurs."
  (oset (oref this bp-listener)
   :marker-regexp
   "^.*: \"thread=.*\", \\(\\(.*[.]\\)*\\)\\([^$]*\\)\\($.*\\)*[.].+(), line=\\([0-9,.\240]*\\)"))


(defun jdee-jdb-get-jdb ()
  "Gets the version of jdb specified for the
current project."
  (let (jdb)
    (cond
     ((string= (car jdee-debugger) "jdb")
      (cond
       ((< (jdee-java-major-version) 2)
        (cond
         ((< (jdee-java-minor-version) 2)
          (setq jdb (jdee-db-jdb-1-1 "jdb 1.1")))
         ((= (jdee-java-minor-version) 3)
          (setq jdb (jdee-db-jdb-1-3 "jdb 1.3")))
         ((= (jdee-java-minor-version) 4)
          (setq jdb (jdee-db-jdb-1-4 "jdb 1.4")))
         (t
          (setq jdb (jdee-db-jdb-1-6 "jdb 1.6")))))
       (t
        (setq jdb (jdee-db-jdb-1-6 "jdb 1.6")))))
     ((string= (car jdee-debugger) "old jdb")
      (if (and (< (jdee-java-major-version) 2)
               (< (jdee-java-minor-version) 2))
          (setq jdb (jdee-db-jdb-1-1 "jdb 1.1"))
        (setq jdb (jdee-db-old-jdb "old jdb"))))
     (t
      (error "%s is not a valid jdb debugger choice."
             (car jdee-debugger))))
    (oset
     jdb
     :path (jdee-get-jdk-prog (oref jdb :exec-name)))
    jdb))

(defun jdee-jdb-get-socket-address ()
  (if jdee-db-option-connect-socket
      jdee-db-option-connect-socket
    (let ((host
           (read-from-minibuffer  "Debuggee host: " "local"))
          (port
           (read-from-minibuffer "Debuggee port: " "4444")))
      (list
       (if (not (string= host "local"))
           host)
       port))))



(defun jdee-jdb-attach-via-socket ()
  "Launch jdb in attach mode. In this mode, jdb connects itself to an
existing debuggee process via a socket. The debuggee process itself must have been
launched in debugger server mode. The JDK vm accepts command line
arguments that starts the vm in the appropriate mode, e.g.,

java -Xdebug -Xrunjdwp:transport=dt_socket,address=4444,server=y,suspend=n MyClass

starts MyClass in debugger server mode at the socket address
4444. See jdb in the tools section of the JDK documentation for
more information on these arguments.

Selecting the Server mode option of the `jdee-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The attach command connects the debugger to the debuggee at the
address specified by `jdee-db-option-connect-socket'. If this variable
is nil, this command prompts you to enter the address."
  (interactive)
  (let* ((socket (jdee-jdb-get-socket-address))
         (host (nth 0 socket))
         (port (nth 1 socket)))
    (if (string= port "")
        (error "You must specify the port of the debuggee process.")
      (let* ((debugger (jdee-jdb-get-jdb))
             (connector
              (jdee-db-socket-attach-connector
               "connector"
               :host host
               :port port))
             (debuggee
              (jdee-jdb-debuggee-app
              "debuggee"
              :main-class (format
                           "Attached to socket %s:%s"
                           (if host host "localhost")
                           port)
              :connector connector)))
        (oset debugger the-debugger debugger)
        (oset debugger :debuggee debuggee)
        (jdee-jdb-connect debugger)))))


(defun jdee-jdb-get-shared-memory-name ()
  (if jdee-db-option-connect-shared-memory-name
      jdee-db-option-connect-shared-memory-name
    (read-from-minibuffer  "Shared memory name: " "javadebug")))


(defun jdee-jdb-attach-via-shared-memory ()
  "Launch jdb in attach mode. In this mode, jdb connects itself to an
existing debuggee process. This command specifies use of shared memory
as the connection transport. The debuggee process itself must have
been launched in debugger server mode. The JDK vm accepts command line
arguments that starts the vm in the appropriate mode, e.g.,

java -Xdebug -Xrunjdwp:transport=dt_shmem,address=javadebug,server=y,suspend=n MyClass

starts MyClass in debugger server mode, specifying \"javadebug\" as
the name of the shared memory transport. See jdb in the tools section
of the JDK documentation for more information on these arguments.

Selecting the Server mode option of the `jdee-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The attach command connects the debugger to the debuggee at the
address specified by `jdee-db-option-connect-shared-memory', or, if
this variable is nil, this command prompts you to enter a name."
  (interactive)
  (assert
   (eq system-type 'windows-nt)
   "The debugger does not support shared memory connections on this platform.")
  (let ((shmem-name (jdee-jdb-get-shared-memory-name)))
    (if (string= shmem-name "")
        (error "Shared memory name required.")
      (let* ((debugger (jdee-jdb-get-jdb))
             (connector
              (jdee-db-shared-memory-attach-connector
               "connector"
               :name shmem-name))
             (debuggee
              (jdee-jdb-debuggee-app
               "debuggee"
               :main-class (format
                            "Attached via shared memory: %s."
                            shmem-name)
               :connector connector)))
        (oset debugger the-debugger debugger)
        (oset debugger :debuggee debuggee)
        (jdee-jdb-connect debugger)))))


(defun jdee-jdb-get-socket-listen-port ()
  (if jdee-db-option-connect-socket
      (nth 1 jdee-db-option-connect-socket)
    (read-from-minibuffer "Listen port: " "4444")))

(defun jdee-jdb-listen-via-socket ()
  "Launch jdb in listen mode. In this mode, jdb launches itself and
connects itself to a subsequently launched debuggee process. This
command uses a socket as the method for connecting to the debuggee
process. The debuggee process itself must be launched in debugger
client mode The JDK vm accepts command line arguments that starts the
vm in the appropriate mode, e.g.,

java -Xdebug -Xrunjdwp:transport=dt_socket,address=4444,server=n,suspend=n MyClass

starts MyClass in debugger client mode at the socket port 4444. See jdb in
the tools section of the JDK documentation for
more information.

Selecting the Client mode option of the `jdee-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The listen command listens for the debugger at the
address specified by `jdee-db-option-connect-socket'. If this variable
is nil, this command prompts you to enter the address. You can enter
either the address that you plan to start the debuggee process at
(e.g., jdbconn) or a null string.  If you enter a null string, this
command startes the debugger in a mode that accepts connections from
any debuggee process started in debugger client mode, regardless of
address."
  (interactive)
  (let* ((debugger (jdee-jdb-get-jdb))
         (port (jdee-jdb-get-socket-listen-port))
         (connector
           (jdee-db-socket-listen-connector
               "connector"
               :port port))
         (debuggee
          (jdee-jdb-debuggee-app
           "debuggee"
          :main-class (concat "Listening at port " port)
          :connector connector)))
    (oset debugger the-debugger debugger)
    (oset debugger :debuggee debuggee)
    (jdee-jdb-connect debugger)))

(defun jdee-jdb-listen-via-shared-memory ()
  "Launch jdb in listen mode. In this mode, a subsequently
launched debuggee process connects itself to jdb. This command uses
Windows shared memory primitives as the method of communications
between jdb andthe debuggee process. The debuggee process itself must
be launched in debugger client mode. The JDK vm accepts command line
arguments that starts the vm in the appropriate mode, e.g.,

java -Xdebug
-Xrunjdwp:transport=dt_shmem,address=javadebug,server=n,suspend=n
MyClass

starts MyClass in debugger client mode at the shared memory address
javadebug. See jdb in the tools section of the JDK documentation for
more information.

Selecting the Client mode option of the `jdee-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The listen command listens for the debugger at the
address specified by `jdee-db-option-connect-shared-memory'. If this variable
is nil, this command prompts you to enter the address that you plan to start
the debuggee process at (e.g., jdbconn)."
  (interactive)
  (assert
   (eq system-type 'windows-nt)
   "The debugger does not support shared memory connections on this platform.")
  (let* ((debugger (jdee-jdb-get-jdb))
         (name (jdee-jdb-get-shared-memory-name))
         (connector
           (jdee-db-shared-memory-listen-connector
               "connector"
               :name name))
         (debuggee
          (jdee-jdb-debuggee-app
           "debuggee"
          :main-class (concat "Listening to " name)
          :connector connector)))
    (oset debugger the-debugger debugger)
    (oset debugger :debuggee debuggee)
    (jdee-jdb-connect debugger)))

(defun jdee-jdb-display (key)
  "Print expression at point."
  (interactive "sExpression: ")
  (jdee-assert-source-or-debug-buffer)
  (let* ((debugger (oref-default 'jdee-db-debugger the-debugger))
         (debuggee (oref debugger debuggee))
         (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
             (oref debuggee-status stopped-p))
        (let* ((cmd-set (oref debugger cmd-set))
               cmd)
          (cond ((string= "print" key)
                 (setq cmd (oref cmd-set print)))
                ((string= "dump" key)
                 (setq cmd (oref cmd-set dump)))
                ((string= "eval" key)
                 (setq cmd (oref cmd-set eval)))
                ((string= "set" key)
                 (setq cmd (oref cmd-set set-var)))
                ((string= "locals" key)
                 (setq cmd (oref cmd-set locals))))
          (jdee-db-exec-cmd debugger cmd))
      (let ((class (oref debuggee main-class)))
        (error "Application %s is not stopped" class)))))

(defun jdee-jdb-print ()
  (interactive)
  (jdee-jdb-display "print"))

(defun jdee-jdb-dump ()
  (interactive)
  (jdee-jdb-display "dump"))

(defun jdee-jdb-eval ()
  (interactive)
  (jdee-jdb-display "eval"))

(defun jdee-jdb-locals ()
  (interactive)
  (jdee-jdb-display "locals"))

(defun jdee-jdb-set ()
  (interactive)
  (jdee-jdb-display "set"))

(defun jdee-jdb-help ()
  (interactive)
  (let* ((jdee-dir (jdee-find-jdee-doc-directory))
         (jdb-ug-path
          (if jdee-dir
              (expand-file-name "doc/html/jdb-ug/jdb-ug-frame.html" jdee-dir))))
    (if (and
         jdb-ug-path
         (file-exists-p jdb-ug-path))
        (browse-url (concat "file://" (jdee-convert-cygwin-path jdb-ug-path)))
      (signal 'error '("Cannot find jdb user guide.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debug Commands                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar jdee-jdb-emacs-menu-spec
  (list "Jdb"

        ["Step Over"
         jdee-debug-step-over
         :active (jdee-db-debuggee-stopped-p)
         :help "Step over the next method."]


        ["Step Into"
         jdee-debug-step-into
         :active (jdee-db-debuggee-stopped-p)
         :help "Step into the next method."]

        ["Step Out"
         jdee-debug-step-out
         :active (jdee-db-debuggee-stopped-p)
         :help "Step out of the current method."]

        ["Run"
         jdee-debug-run
         :active   (and
                    (slot-boundp 'jdee-db-debugger 'the-debugger)
                    (let* ((debugger (oref-default 'jdee-db-debugger the-debugger))
                           (debuggee (oref debugger debuggee))
                           (debuggee-status (oref debuggee status)))
                      (and (oref debugger running-p)
                           (not (oref debuggee-status running-p)))))

         :included (or
                    (not (slot-boundp 'jdee-db-debugger 'the-debugger))
                    (let* ((debugger (oref-default 'jdee-db-debugger the-debugger))
                           (debuggee (oref debugger debuggee))
                           (debuggee-status (oref debuggee status)))
                      (or (not (oref debugger running-p))
                          (not (oref debuggee-status running-p)))))
         :help "Start the current program."]

        ["Continue"
         jdee-debug-cont
         :active   (or
                    (jdee-db-debuggee-stopped-p)
                    (jdee-db-debuggee-suspended-p))

         :included  (jdee-db-debuggee-running-p)
         :help "Continue the current program."]

        ["Quit"
         jdee-debug-quit
         :active  (jdee-db-debuggee-running-p)
         :help "Terminate the current debugging session."]

        "-"

        ["Toggle Breakpoint"
         jdee-debug-toggle-breakpoint
         :active t
         :help "Set (or remove) a breakpoint at the current line."]

        ["Clear Breakpoints"
         jdee-debug-clear-breakpoints
         :active  jdee-db-breakpoints
         :help "Remove all breakpoints."]

        ["List Breakpoints"
         jdee-debug-list-breakpoints
         :active  jdee-db-breakpoints
         :help "Display a list of breakpoints."]

        "-"

        (list
         "Display"

         ["Expression"
          jdee-jdb-print
          :active  (jdee-db-debuggee-stopped-p)
          :help "Evaluate an expression and display the results."]

         ["Object"
          jdee-jdb-dump
          :active  (jdee-db-debuggee-stopped-p)
          :help "Display the fields of an object referenced by a variable."]


         ["Locals"
          jdee-jdb-locals
          :active  (jdee-db-debuggee-stopped-p)
          :help "Display the variables in scope at the current line."]
         )

        ["Set Variable"
         jdee-jdb-set
         :active  (jdee-db-debuggee-stopped-p)
         :help "Change the value of an in-scope variable."]

        (list
         "Stack"

         ["Up"
          jdee-debug-up
          :active  (jdee-db-debuggee-stopped-p)
          :help "Move the debug cursor up the method call stack."]

         ["Down"
          jdee-debug-down
          :active (and
                   (jdee-db-debuggee-stopped-p)
                   (let* ((debugger (oref-default 'jdee-db-debugger the-debugger))
                          (debuggee (oref debugger debuggee)))
                     (> (jdee-jdb-string-to-int
                         (oref debuggee :stack-depth)) 1)))
          :help "Move the debug cursor down the method call stack." ]

         ["Where"
          jdee-debug-where
          :active (jdee-db-debuggee-stopped-p)
          :help "Display the call stack."]

         )
        "-"
        (list
         "External Process"
         ["Attach Via Socket"
          jdee-jdb-attach-via-socket
          :active (not (jdee-db-debuggee-running-p))
          :help "Attach the debugger to an external process via a socket."]
         ["Attach Via Shared Memory"
          jdee-jdb-attach-via-shared-memory
          :active (and
                   (eq system-type 'windows-nt)
                   (not (jdee-db-debuggee-running-p)))
          :help "Attach the debugger to an external process via a shared memory connection."]
         ["Listen Via Socket"
          jdee-jdb-listen-via-socket
          :active (not (jdee-db-debuggee-running-p))
          :help "Listen at a socket for an external process."]
         ["Listen Via Shared Memory"
          jdee-jdb-listen-via-shared-memory
          :active (and
                   (eq system-type 'windows-nt)
                   (not (jdee-db-debuggee-running-p)))
          :help "Listen in shared memory for an external process."]
         )
        "-"

        ["Preferences"
         jdee-bug-show-preferences
         :active nil
         :help "Not yet implemented."]

        "-"
        ["Help"
         jdee-jdb-help
         :active t
         :help "Display the JDEE's jdb user's guide in an HTML browser."]
        )
  "Defines the Jdb menu for Emacs.")

(defvar jdee-jdb-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define jdee-jdb-menu km "Jdb Minor Mode Menu"
      jdee-jdb-emacs-menu-spec)
    km)
  "Keymap for Jdb minor mode.")

(define-minor-mode jdee-jdb-minor-mode nil
                   :keymap jdee-jdb-mode-map)

(semantic-add-minor-mode 'jdee-jdb-minor-mode " jdb")


;; (fmakunbound 'jdee-jdb-key-bindings)
(defcustom jdee-jdb-key-bindings
  (list (cons "[?\C-c ?\C-a ?\C-s]" 'jdee-debug-step-over)
        (cons "[?\C-c ?\C-a ?\C-n]" 'jdee-debug-step-into)
        (cons "[?\C-c ?\C-a ?\C-o]" 'jdee-debug-step-out)
        (cons "[?\C-c ?\C-a ?\C-c]" 'jdee-debug-cont)
        (cons "[?\C-c ?\C-a ?\C-r]" 'jdee-debug-run)
        (cons "[?\C-c ?\C-a ?\C-b]" 'jdee-debug-toggle-breakpoint)
        (cons "[?\C-c ?\C-a ?\C-u]" 'jdee-debug-up)
        (cons "[?\C-c ?\C-a ?\C-d]" 'jdee-debug-down)
        (cons "[?\C-c ?\C-a ?\C-p]" 'jdee-jdb-print)
        (cons "[?\C-c ?\C-a ?\C-x]" 'jdee-jdb-dump)
        (cons "[?\C-c ?\C-a ?\C-e]" 'jdee-jdb-eval)
        (cons "[?\C-c ?\C-a ?\C-v]" 'jdee-jdb-set)
        (cons "[?\C-c ?\C-a ?\C-l]" 'jdee-jdb-locals))
  "*Specifies key bindings for jdb debug commands.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jdee-project
  :type '(repeat
          (cons :tag "Key binding"
           (string :tag "Key")
           (function :tag "Command")))
  :set '(lambda (sym val)
          ;; Unmap existing key bindings
          (if (and
               (boundp 'jdee-jdb-key-bindings)
               jdee-jdb-key-bindings)
              (mapc
               (lambda (binding)
                 (let ((key (car binding))
                       (fcn (cdr binding)))
                   (if (string-match "\\[.+]" key)
                       (setq key (car (read-from-string key))))
                   (define-key jdee-jdb-mode-map key nil)))
               jdee-jdb-key-bindings))
          ;; Map new key bindings.
          (mapc
           (lambda (binding)
             (let ((key (car binding))
                   (fcn (cdr binding)))
               (if (string-match "\\[.+]" key)
                   (setq key (car (read-from-string key))))
               (define-key jdee-jdb-mode-map key fcn)))
           val)
          (set-default sym val)))

(defun jdee-jdb-string-to-int (number)
  "This method removes punctuation from a string, e.g, 1,200 (1.200 in Danish),
and converts the result to an integer."
  (if (string-match "[^[:digit:]]" number)
      (setq number (replace-match "" nil nil number)))
  (string-to-number number))

(provide 'jdee-jdb)

;;; jdee-jdb.el ends here
