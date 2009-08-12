;;; jde-jdb.el -- Debugger mode for jdb.
;; $Id$

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

(require 'jde-db)


;; Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for this function
;; and its use in jde-db-marker-filter.
;; Amended by "Patrick J. McNerthney" <pat@mcnerthney.com> to allow
;; package names to begin with underscores.
(defun jde-jdb-make-qualified-class-name-regexp (class)
  "Constructs a regular expression to extract a qualified class name from a jdb
breakpoint message."
  (concat "\\(\\(\\(\\(\\w\\|[_]\\)*\\.\\)*\\)" class "\\)\\(\\b\\|\\$\\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Debugger Commands                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Launch debuggee command


(defclass jde-jdb-cmd-launch (jde-db-cmd-launch) ()
  "Asks jdb to launch a debuggee process.")

(defmethod jde-db-cmd-launch-buffer-name ((this jde-jdb-cmd-launch))
  "Return the name of the buffer for this process. Descendant
classes should override this method to create a name that is appropriate
for the process being launched, e.g., an applet or application." nil)

(defmethod jde-db-cmd-launch-cmd-path ((this jde-jdb-cmd-launch))
  "Return the path of the command to be used to launch the process. Descendant
classes should override this method to return a path appropriate to
the command to be used to launch the debuggee process, e.g., jdb or
appletviewer." nil)


(defmethod jde-db-cmd-launch-startup-cmds ((this jde-jdb-cmd-launch))
  "Add commands to debugger's initial command queue. Derived classes
should override this method to specify commands that should be
executed immediately after the debugger starts, e.g., an initial
step command." nil)


(defmethod jde-db-cmd-init ((this jde-jdb-cmd-launch))
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
		(jde-db-debugger-get-working-dir debugger))
	       (prog-args (jde-db-debugger-get-prog-args debugger))
	       (cmd-path (jde-db-cmd-launch-cmd-path this))
	       (command-string
		(concat
		 cmd-path " "
		 (jde-run-make-arg-string prog-args) "\n\n")))

	(oset debugger :buffer-name (jde-db-cmd-launch-buffer-name this))
	(oset debugger :buffer (get-buffer-create (oref debugger :buffer-name)))

	(jde-db-cmd-launch-startup-cmds this)

	(oset debugger :path cmd-path)
	(jde-db-jdb-start debugger prog-args command-string)

	(let ((debuggee-status (oref debuggee status)))
	  (oset debuggee-status running-p t)
	  (oset debuggee-status stopped-p t)))
    (progn
      (message "An instance of %s is running." (oref this :buffer-name))
      (pop-to-buffer (oref this :buffer-name))))))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-launch))
  "Returns nil because jdb launches the debuggee application automatically
when it is started." nil)


;; Launch application command

(defclass jde-jdb-cmd-launch-app (jde-jdb-cmd-launch
				  jde-db-cmd-launch-app) ()
  "Asks jdb to launch the debuggee application.")

(defmethod initialize-instance ((this jde-jdb-cmd-launch-app) &rest fields)
  (call-next-method)
  (oset this name "launch application in jdb debug mode"))

(defmethod jde-db-cmd-launch-cmd-path ((this jde-jdb-cmd-launch-app))
  "Return the path of the jdb command."
  (let* ((debugger (oref this :debugger)))
    (oref debugger :path)))

(defmethod jde-db-cmd-launch-buffer-name ((this jde-jdb-cmd-launch-app))
  (let* ((debugger (oref this :debugger))
	 (debuggee (oref debugger :debuggee))
	 (main-class (oref debuggee :main-class)))
    (concat  "*debug"  main-class "*")))

(defmethod jde-db-cmd-launch-startup-cmds ((this jde-jdb-cmd-launch-app))
  "If `jde-db-initial-step-p' is nonnil, add a step command to the
debugger's startup command queue."
  (if jde-db-initial-step-p
      (let*  ((debugger (oref this debugger))
	      (step-cmd (oref (oref debugger cmd-set) step-into)))
	(oset debugger next-cmd
		    (append (oref debugger next-cmd) (list step-cmd))))))


;; Launch applet command

(defclass jde-jdb-cmd-launch-applet (jde-jdb-cmd-launch
				     jde-db-cmd-launch-applet) ()
  "Asks jdb to launch the debuggee applet.")

(defmethod initialize-instance ((this jde-jdb-cmd-launch-applet) &rest fields)
  (call-next-method)
  (oset this name "launch applet in jdb debug mode"))

(defmethod jde-db-cmd-launch-cmd-path ((this jde-jdb-cmd-launch-applet))
  "Return the path of the command to be used to launch the process. Descendant
classes should override this method to return a path appropriate to
the command to be used to launch the debuggee process, e.g., jdb or
appletviewer."
  (let* ((debugger (oref this :debugger))
	 (jdb-path (oref debugger :path))
	 (jdb-dir (file-name-directory jdb-path)))
    (expand-file-name "appletviewer" jdb-dir)))

(defmethod jde-db-cmd-launch-buffer-name ((this jde-jdb-cmd-launch-applet))
  (let* ((debugger (oref this :debugger))
	 (debuggee (oref debugger :debuggee))
	 (doc (oref debuggee :doc)))
    (concat  "*debug"  (file-name-nondirectory doc) "*")))

(defmethod jde-db-cmd-launch-startup-cmds ((this jde-jdb-cmd-launch-applet))
  "If `jde-db-initial-step-p' is nonnil, add a run command followed by a
step command to the debugger's startup command queue."
  (if jde-db-initial-step-p
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

(defclass jde-jdb-cmd-run (jde-db-cmd) ()
  "Asks jdb to start the debuggee application.")

(defmethod initialize-instance ((this jde-jdb-cmd-run) &rest fields)
  (call-next-method)
  (oset this name "run"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-run))
  "Creates command line for jdb run command."
  "run")

;; Cont command

(defclass jde-jdb-cmd-cont (jde-db-cmd) ()
  "Asks jdb to continue the debuggee application from its current
stopping point.")

(defmethod initialize-instance ((this jde-jdb-cmd-cont) &rest fields)
  (call-next-method)
  (oset this name "cont"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-cont))
  "Creates command line for jdb cont command."
  "cont")

;; Quit command

(defclass jde-jdb-cmd-quit (jde-db-cmd) ()
  "Quit debugging the current application.")

(defmethod initialize-instance ((this jde-jdb-cmd-quit) &rest fields)
  (call-next-method)
  (oset this name "quit"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-quit))
  "Creates command line for jdb quit command."
  "quit")

;; Step-over command

(defclass jde-jdb-cmd-step-over (jde-db-cmd) ()
  "Step to the next line in the current frame.")

(defmethod initialize-instance ((this jde-jdb-cmd-step-over) &rest fields)
  (call-next-method)
  (oset this name "next"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-step-over))
  "Creates command line for jdb step-over command."
  "next")

;; Step-into command

(defclass jde-jdb-cmd-step-into (jde-db-cmd) ()
  "Step to the next line in the current program.")

(defmethod initialize-instance ((this jde-jdb-cmd-step-into) &rest fields)
  (call-next-method)
  (oset this name "step"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-step-into))
  "Creates command line for jdb step-into command."
  "step")

;; Step-out command

(defclass jde-jdb-cmd-step-out (jde-db-cmd) ()
  "Continue to the end of the current method.")

(defmethod initialize-instance ((this jde-jdb-cmd-step-out) &rest fields)
  (call-next-method)
  (oset this name "step up"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-step-out))
  "Creates command line for jdb step-out command."
  "step up")


;; Up stack command

(defclass jde-jdb-cmd-up (jde-db-cmd) ()
  "Move up one stack frame.")

(defmethod initialize-instance ((this jde-jdb-cmd-up) &rest fields)
  (call-next-method)
  (oset this name "up"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-up))
  "Creates command line for jdb up command."
  "up")

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-up) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response. This method invokes the jdb where
command in order to position the debug pointer at the
current stack location."
  ;; (jde-debug-where)
  (let* ((jdb (oref this debugger))
	 (cmds (oref jdb cmd-set))
	 (cmd (oref cmds where)))
    (jde-db-exec-cmd jdb cmd)))

;; Down stack command

(defclass jde-jdb-cmd-down (jde-db-cmd) ()
  "Move down one stack frame.")

(defmethod initialize-instance ((this jde-jdb-cmd-down) &rest fields)
  (call-next-method)
  (oset this name "down"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-down))
  "Creates command line for jdb down command."
  "down")

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-down) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response. This method invokes the jdb where
command in order to position the debug pointer at the
current stack location."
  ;;(jde-debug-where)
  (let* ((jdb (oref this debugger))
	 (cmds (oref jdb cmd-set))
	 (cmd (oref cmds where)))
    (jde-db-exec-cmd jdb cmd)))


;; Where stack command

(defclass jde-jdb-cmd-where (jde-db-cmd) ()
  "Point to current location on the stack.")

(defmethod initialize-instance ((this jde-jdb-cmd-where) &rest fields)
  (call-next-method)
  (oset this name "where"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-where))
  "Creates command line for jdb where command."
  "where")

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-where) output)
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
	      (line-no (jde-jdb-string-to-int (match-string 2 output)))
	      (package ""))

	  (if (equal ".java" (substring class -5))
	      (setq class (substring class 0 -5)))

	  ;; Extract package path from input.
	  (let ((case-fold-search nil))	;; Make sure search is case-sensitive
	    (and (string-match (jde-jdb-make-qualified-class-name-regexp class) marker)
		 (setq package
		       (substring marker (match-beginning 2) (match-end 2)))))
	  (jde-db-set-debug-cursor
	   (if package (concat package class) class)
	   (concat class ".java") line-no)))
    output))

;; Set Breakpoint command

(defclass jde-jdb-cmd-set-breakpoint (jde-db-cmd-breakpoint) ()
  "Asks jdb to set the breakpoint specified by the
breakpoint field.")

(defmethod initialize-instance ((this jde-jdb-cmd-set-breakpoint) &rest fields)
  (call-next-method)
  (oset this name "stop at"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-set-breakpoint))
  "Creates command line for jdb set breakpoint command."
  (let* ((bps (oref this breakpoints))
	 (bp (car bps)))
    (format "stop at %s:%d"
	    (oref bp class)
	    (jde-db-breakpoint-get-line bp))))

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-set-breakpoint) output)
  "Called when the debugger responds to the last set-breakpoint
  command. Invokes `jde-db-mark-breakpoint-requested' on the breakpoint and
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
	      (setq line (jde-db-breakpoint-get-line bp))
	      (if (string-match "Unable to set breakpoint" output)
		  (jde-db-delete-breakpoint bp)
		(if (string-match "Unable to set deferred breakpoint" output)
		    (if (jde-db-debuggee-running-p)
			(let* ((debugger (oref 'jde-db-debugger the-debugger))
			       (bp-cmd
				(oref (oref debugger cmd-set) clear-bp)))
			  (oset bp-cmd breakpoints (list bp))
			  (jde-db-exec-cmd debugger bp-cmd))
		      (jde-db-delete-breakpoint bp))
		  (if (string-match "Deferring breakpoint" output)
		      (progn
			(oset bp status 'requested)
			(jde-db-mark-breakpoint-requested file line))
		    (if (string-match "Set breakpoint" output)
			(progn
			  (oset bp status 'active)
			  (jde-db-mark-breakpoint-active file line))))))
	      (setq bps (cdr bps))
	      (oset this breakpoints bps)
	      (if bps
		  (let ((jdb (oref this debugger)))
		    (jde-db-exec-cmd jdb this))))))))

;; Clear Breakpoint command

(defclass jde-jdb-cmd-clear-breakpoint (jde-db-cmd-breakpoint) ()
  "Asks jdb to clear the breakpoint specified by the
breakpoint field.")

(defmethod initialize-instance ((this jde-jdb-cmd-clear-breakpoint) &rest fields)
  (call-next-method)
  (oset this name "clear"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-clear-breakpoint))
  "Creates command line for jdb clear breakpoint command."
  (let* ((bps (oref this breakpoints))
	 (bp (car bps)))
    (format "clear %s:%d"
	    (oref bp class)
	    (jde-db-breakpoint-get-line bp))))

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-clear-breakpoint) output)
  "Called when the debugger responds to the last clear-breakpoint command.
Removes the breakpoint from the command's breakpoint list. If the list contains
more breakpoints, this method reissues the clear command on the next breakpoint
on the list."
  (let* ((bps (oref this breakpoints))
	 (bp (car bps)))
    (jde-db-delete-breakpoint bp)
    (setq bps (cdr bps))
    (oset this breakpoints bps)
    (if bps
	(let ((jdb (oref this debugger)))
	  (jde-db-exec-cmd jdb this)))))

;; Print command
(defvar jde-jdb-cmd-print-history  nil)

(defclass jde-jdb-cmd-print (jde-db-cmd)
  ((expr	:initarg :expr
		:type string
		:initform ""
		:documentation
		"Expression passed to jdb"))
  "Asks jdb to print value of expression at point")

(defmethod initialize-instance ((this jde-jdb-cmd-print) &rest fields)
  (call-next-method)
  (oset this name "print")
  (oset this expr ""))

(defmethod jde-db-cmd-init ((this jde-jdb-cmd-print))
  "The debugger invokes this method before executing the
command."
  (oset this expr (read-from-minibuffer "expr: " (thing-at-point 'word)
					nil nil 'jde-jdb-cmd-print-history)))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-print))
  "Creates a command line for jdb print command."
  (format "%s %s"
	  (oref this name)
	  (oref this expr)))

;; Dump command
(defclass jde-jdb-cmd-dump (jde-jdb-cmd-print) ()
  "Asks jdb to print all object information of the expression at point")

(defmethod initialize-instance ((this jde-jdb-cmd-dump) &rest fields)
  (call-next-method)
  (oset this name "dump"))

;; Eval command
(defclass jde-jdb-cmd-eval (jde-jdb-cmd-print) ()
  "Ask jdb to evaluate the expression(Same as the print command)")

(defmethod initialize-instance ((this jde-jdb-cmd-eval) &rest fields)
  (call-next-method)
  (oset this name "eval"))

;; Set command
(defclass jde-jdb-cmd-set-var (jde-jdb-cmd-print)
  ((value :initarg :value
	  :type string
	  :initform "null"
	  :document
	  "Value to assign to the variable"))
  "Ask jdb to assign new value to a field/variable/array element")

(defmethod initialize-instance ((this jde-jdb-cmd-set-var) &rest fields)
  (call-next-method)
  (oset this name "set"))

(defmethod jde-db-cmd-init ((this jde-jdb-cmd-set-var))
  "The debugger invokes this method before executing the
command."
  (oset this expr (read-from-minibuffer "variable: " (thing-at-point 'word)
					nil nil 'jde-jdb-cmd-print-history))
  (oset this value (read-from-minibuffer "value: " nil
					nil nil '(null))))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-set-var))
  "Creates a command line for jdb print command."
  (format "%s %s = %s"
	  (oref this name)
	  (oref this expr)
	  (oref this value)))

;; Locals commands
(defclass jde-jdb-cmd-locals (jde-jdb-cmd-print) ()
  "Ask jdb to print al local variables in current stack frame")

(defmethod initialize-instance ((this jde-jdb-cmd-locals) &rest fields)
  (call-next-method)
  (oset this name "locals"))

(defmethod jde-db-cmd-init ((this jde-jdb-cmd-locals))
  "The debugger invokes this method before executing the
command."
  (oset this expr ""))

;; jdb Command Set

(defclass jde-jdb-cmd-set (jde-db-cmd-set)
  ((print :initarg :print
	  :type jde-jdb-cmd-print
	  :documentation
	  "Asks jdb to print the value of expression at point")
   (dump :initarg :dump
	 :type jde-jdb-cmd-dump
	 :documentation
	 "Ask jdb to print all object information from the
expression at poing")
   (eval :initarg :eval
	 :type jde-jdb-cmd-eval
	 :documentation
	 "Ask jdb to evaluate the expression at point")
   (set-var :initarg :set-var
	    :type jde-jdb-cmd-set-var
	    :documentation
	    "Ask jdb to assign a new value to the expression at point")
   (locals :initarg :locals
	 :type jde-jdb-cmd-locals
	 :documentation
	 "Ask jdb to print all local variables in current stack frame")
   )
  "Set of debugger commands implemented by jdb.")

(defmethod initialize-instance ((this jde-jdb-cmd-set) &rest fields)
  "Construct jdb command set."
  (call-next-method)
  (let ((jdb (oref this debugger)))
    (oset this launch-app
	  (jde-jdb-cmd-launch-app "launch" :debugger jdb))
    (oset this launch-applet
	  (jde-jdb-cmd-launch-applet "launch" :debugger jdb))
    (oset this run
	  (jde-jdb-cmd-run "run" :debugger jdb))
    (oset this cont
	  (jde-jdb-cmd-cont "cont" :debugger jdb))
    (oset this quit
	  (jde-jdb-cmd-quit "jdb quit" :debugger jdb))
    (oset this step-over
	  (jde-jdb-cmd-step-over "jdb step-over cmd" :debugger jdb))
    (oset this step-into
	  (jde-jdb-cmd-step-into "jdb step-into cmd" :debugger jdb))
    (oset this step-out
	  (jde-jdb-cmd-step-out "jdb step-out cmd" :debugger jdb))
    (oset this up
	  (jde-jdb-cmd-up "jdb up cmd" :debugger jdb))
    (oset this down
	  (jde-jdb-cmd-down "jdb down cmd" :debugger jdb))
    (oset this where
	  (jde-jdb-cmd-where "jdb where cmd" :debugger jdb))
    (oset this set-bp
	  (jde-jdb-cmd-set-breakpoint "jdb set breakpoint" :debugger jdb))
    (oset this clear-bp
	  (jde-jdb-cmd-clear-breakpoint "jdb clear breakpoint" :debugger jdb))
    (oset this print
	  (jde-jdb-cmd-print "jdb print cmd" :debugger jdb))
    (oset this dump
	  (jde-jdb-cmd-dump "jdb dump cmd" :debugger jdb))
    (oset this eval
	  (jde-jdb-cmd-eval "jdb eval cmd" :debugger jdb))
    (oset this set-var
	  (jde-jdb-cmd-set-var "jdb set cmd" :debugger jdb))
    (oset this locals
	  (jde-jdb-cmd-locals "jdb locals cmd" :debugger jdb))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Breakpoint Listener                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-jdb-breakpoint-listener (jde-db-listener)
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


(defmethod initialize-instance ((this jde-jdb-breakpoint-listener) &rest fields)
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
   "^.*: thread=.*, \\(\\(.*[.]\\)*\\)\\([^\$]*\\)\\(\$.*\\)*[.].+(), line=\\([0-9,.]*\\),")
  ;; Regular expression to match a breakpoint message that lacks a line
  ;; number because the breakpoint occurs in a class compiled without deug
  ;; information.
  (oset
   this
   :noline-regexp
   "^Breakpoint hit: .*(pc \\([0-9]*\\))"))



(defmethod jde-jdb-fixup-output ((this jde-jdb-breakpoint-listener))
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
    ;; (message "fixed string is %s" jde-db-marker-acc)
    )


(defmethod jde-jdb-set-breakpoint-listener ((this jde-jdb-breakpoint-listener) output)
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
		     (line (string-to-int
			    (substring
			     msg
			     (match-beginning 2)
			     (match-end 2))))
		     (source-buffer (jde-db-find-class-source class))
		     (path (buffer-file-name source-buffer))
		     (bp (jde-db-find-breakpoint path line)))
		(oset bp status 'active)
		(jde-db-mark-breakpoint-active path  line))))))

(defmethod jde-db-listener-filter-output ((this jde-jdb-breakpoint-listener) input)
  "Filters the output of the debugger."
  (let ((jdb (oref this debugger))
	(output ""))

    ;; Accumulate next chunk of debugger output.
    (oset this
	  :marker-acc (concat
		       (oref this :marker-acc)
		       input))

    ;; (message (format "<acc-start>%s<acc-end>" (oref this :marker-acc)))

    (jde-jdb-fixup-output this)

    (let* ((marker-regexp (oref this :marker-regexp))
	   (marker-regexp-class-index (oref this :class-index))
	   (marker-regexp-line-index (oref this :line-index)))

      ;; (message (concat "jdb output:" input))
      ;; (message (concat "acc = " jde-db-marker-acc))

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
		(line-no (jde-jdb-string-to-int
			  (substring
			   (oref this :marker-acc)
			   (match-beginning marker-regexp-line-index)
			   (match-end marker-regexp-line-index))))
		(package ""))
	    ;; Extract package path from input.
	    (let ((case-fold-search nil)) ;; Make sure search is case-sensitive
	      (and (string-match (jde-jdb-make-qualified-class-name-regexp class) marker)
		   (setq package
			 (substring marker (match-beginning 2) (match-end 2))))

	       ;; (message "jde-db package: %s. marker = %s" jde-db-last-package marker)
	       ;;(message "case-fold-search = %s" (if case-fold-search "true" "false"))
	      )

	    ;; Insert debugger output into debugger buffer.
	    (setq output (concat premarker marker))

	    ;; Set the accumulator to the remaining text.
	    (oset this :marker-acc rest)

	    (jde-db-set-debug-cursor
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

    (jde-jdb-set-breakpoint-listener this output)
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Stack Listener                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-jdb-stack-listener (jde-db-listener)
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
(defmethod jde-db-listener-filter-output ((this jde-jdb-stack-listener) output)
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
(defclass jde-jdb-debuggee-app (jde-db-debuggee-app) ()
  "Application process being debugged with jdb.")

(defmethod initialize-instance ((this jde-jdb-debuggee-app) &rest fields)
  "Constructs an instance of a jdb debuggee."
  (call-next-method)
  (oset  this  status  (jde-db-debuggee-status "jdb status")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Applet Debuggee                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-jdb-debuggee-applet (jde-db-debuggee-applet) ()
  "Application process being debugged with jdb.")

(defmethod initialize-instance ((this jde-jdb-debuggee-applet) &rest fields)
  "Constructs an instance of a jdb debuggee."
  (call-next-method)
  (oset  this  status  (jde-db-debuggee-status "jdb status")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Class of JDE Debuggers based on jdb.                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-jdb (jde-db-debugger)
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
		    :type jde-jdb-breakpoint-listener
		    :documentation "Breakpoint listener."))
  (:allow-nil-initform t)
"Class of generic jdb debuggers")

(defmethod initialize-instance ((this jde-db-jdb) &rest fields)
  "Constructor for generic jdb debugger."
  (call-next-method)
  (oset this :name "jdb")

  ;; Install jdb versions of debugger commands.
  (oset this cmd-set (jde-jdb-cmd-set "jdb commands" :debugger this))

  (oset this bp-listener
   (jde-jdb-breakpoint-listener
    "jdb breakpoint listener"
    :debugger this))

  (jde-db-add-listener this (oref this bp-listener))

  (jde-db-add-listener
   this
   (jde-jdb-stack-listener
    "jdb stack listener"
    :debugger this)))

(defmethod jde-db-create-debuggee-app ((this jde-db-jdb) main-class)
  (oset
   this
   :debuggee (jde-jdb-debuggee-app
	      (concat "Application: " main-class)
	      :main-class main-class)))

(defmethod jde-db-create-debuggee-applet ((this jde-db-jdb) applet-doc)
  (oset
   this
   :debuggee (jde-jdb-debuggee-applet
	      (concat "Applet: " applet-doc)
	      :doc applet-doc)))

(defmethod jde-db-jdb-start ((this jde-db-jdb) prog-args cmdstr)
  "Start the debugger."
  (let ((w32-quote-process-args ?\")
	(win32-quote-process-args ?\") ;; XEmacs
	(source-directory default-directory)
	(working-directory
	 (jde-db-debugger-get-working-dir this)))

    (oset this :buffer (get-buffer-create (oref this :buffer-name)))

    (save-excursion
      (set-buffer (oref this :buffer))
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

      (jde-db-set-process-filter this)
      (jde-db-set-process-sentinel this)
      (run-hooks 'jde-jdb-mode-hook)
      (pop-to-buffer (oref this buffer))

      (oset-default 'jde-db-debugger the-debugger this)
      (oset this running-p t))))


(defmethod jde-jdb-connect ((this jde-db-jdb))
  "Connect the debugger to an existing process."
  (if (or
       (not (slot-boundp this 'buffer))
       (not (oref this :buffer))
       (not (comint-check-proc (oref this :buffer))))
      (let* ((debuggee (oref this debuggee))
	     (source-directory default-directory)
	     (connector (oref debuggee connector))
	     (working-directory
	      (jde-db-debugger-get-working-dir this))
	     (prog-args
	      (if (typep connector 'jde-db-listen-connector)
		  (if (typep connector 'jde-db-socket-connector)
		      (list
		       "-connect"
		       (format
			"com.sun.jdi.SocketListen:port=%s"
			(oref connector port)))
		    (if (typep connector 'jde-db-shared-memory-connector)
			(list
			 "-connect"
			 (format
			  "com.sun.jdi.SharedMemoryListen:name=%s"
			  (oref connector name)))
		      (error "Invalid connector type.")))
		(if (typep connector 'jde-db-attach-connector)
		    (if (typep connector 'jde-db-socket-connector)
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
		    (if (typep connector 'jde-db-shared-memory-connector)
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
	 (if (typep connector 'jde-db-shared-memory-connector)
	     (format "*debug %s* debugee-shmem-name" (oref connector name))
	   (format
	    "*debug %s:%s*"
	    (if (or (typep connector 'jde-db-listen-connector)
		    (not (oref connector port)))
		"localhost" (oref connector host))
	    (oref connector port))))

	(oset this :buffer (get-buffer-create (oref this :buffer-name)))

	;; Forward to the debugger any breakpoint requests made
	;; by the user before launching the application.
	(if jde-db-breakpoints
	    (let ((bp-cmd (oref (oref this cmd-set) set-bp)))
	      (oset
	       bp-cmd
	       breakpoints
	       (mapcar (lambda (assoc-x) (cdr assoc-x)) jde-db-breakpoints))

	      (oset this next-cmd
		    (append (oref this next-cmd) (list bp-cmd)))))

	(jde-db-jdb-start this prog-args command-string)

	(let* ((debuggee (oref this debuggee))
	       (debuggee-status (oref debuggee status)))
	  (oset debuggee-status running-p t)
	  (oset debuggee-status stopped-p t)))
    (progn
      (message "An instance of %s is running." (oref this :buffer-name))
      (pop-to-buffer (oref this :buffer-name)))))


(defmethod jde-db-notify-process-exit ((this jde-db-jdb) msg)
  "The default debugger process sentinel invokes this method
when the jdb process terminates."
  (call-next-method)
  (let* ((debuggee (oref this debuggee))
	 (debuggee-status (oref debuggee status)))
    (oset this running-p nil)
    (oset debuggee-status running-p nil)
    (oset debuggee-status stopped-p nil)
    (jde-db-set-all-breakpoints-specified)))

(defmethod jde-db-launch-arg ((this jde-db-jdb))
  "Generate the -launch option for jdb."
  (list "-launch"))


(defmethod jde-db-debugger-get-prog-args ((this jde-db-jdb))
  (cond
   ((typep (oref this debuggee) 'jde-db-debuggee-app)
    (append
     (jde-db-get-vm-args this)
     (jde-db-get-vm-args-from-user)
     (list (oref (oref this debuggee) main-class))
     jde-db-option-application-args
     (jde-db-get-app-args-from-user)))
   ((typep (oref this debuggee) 'jde-db-debuggee-applet)
    (list "-debug"
	  (oref (oref this debuggee) doc)))
   (t
    (error "Unrecognized jdb debuggee type."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.1.x Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-db-jdb-1-1 (jde-db-jdb)
  ()
  (:allow-nil-initform t)
"Class of jdb shipped with JDK 1.1.x.")


(defmethod initialize-instance ((this jde-db-jdb-1-1) &rest fields)
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
(defclass jde-db-old-jdb (jde-db-jdb-1-1)
  ()
  (:allow-nil-initform t)
"Class of pre-JPDA jdb shipped with post-JPDA versions of the
JDK.")

(defmethod initialize-instance ((this jde-db-old-jdb) &rest fields)
  "Constructor for old jdb."

  (call-next-method)
  (oset this :exec-name "oldjdb"))


(defclass jde-db-jdb-1-3 (jde-db-jdb)
  ()
  (:allow-nil-initform t)
  "Class of JPDA-based jdb shipped with JDK 1.3.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.4.0 Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-jdb-1-4 (jde-db-jdb)
  ()
  (:allow-nil-initform t)
  "Class of JPDA-based jdb shipped with J2SDK 1.4")

(defmethod initialize-instance ((this jde-db-jdb-1-4) &rest fields)
  "Constructor for jdb-1.4."
  (call-next-method)
  ;; Regular expression used to find a jdb breakpoint position marker.
  ;; The regular expression has two subexpressions. The first matches
  ;; the name of the class in which the breakpoint occurs; the second, the
  ;; line number at which the breakpoint occurs."
  (oset (oref this bp-listener)
   :marker-regexp
   "^.*: \"thread=.*\", \\(\\(.*[.]\\)*\\)\\([^$]*\\)\\($.*\\)*[.].+(), line=\\([0-9,.]*\\)"))

(defun jde-jdb-get-jdb ()
  "Gets the version of jdb specified for the
current project."
  (let (jdb)
    (cond
     ((string= (car jde-debugger) "jdb")
      (cond
       ((and (< (jde-java-major-version) 2)
	     (< (jde-java-minor-version) 2))
	(setq jdb (jde-db-jdb-1-1 "jdb 1.1")))
       ((and (< (jde-java-major-version) 2)
	     (= (jde-java-minor-version) 3))
	(setq jdb (jde-db-jdb-1-3 "jdb 1.3")))
       (t
	(setq jdb (jde-db-jdb-1-4 "jdb 1.4")))))
     ((string= (car jde-debugger) "old jdb")
      (if (and (< (jde-java-major-version) 2)
	       (< (jde-java-minor-version) 2))
	  (setq jdb (jde-db-jdb-1-1 "jdb 1.1"))
	(setq jdb (jde-db-old-jdb "old jdb"))))
     (t
      (error "%s is not a valid jdb debugger choice."
	     (car jde-debugger))))
    (oset
     jdb
     :path (jde-get-jdk-prog (oref jdb :exec-name)))
    jdb))

(defun jde-jdb-get-socket-address ()
  (if jde-db-option-connect-socket
      jde-db-option-connect-socket
    (let ((host
	   (read-from-minibuffer  "Debuggee host: " "local"))
	  (port
	   (read-from-minibuffer "Debuggee port: " "4444")))
      (list
       (if (not (string= host "local"))
	   host)
       port))))



(defun jde-jdb-attach-via-socket ()
  "Launch jdb in attach mode. In this mode, jdb connects itself to an
existing debuggee process via a socket. The debuggee process itself must have been
launched in debugger server mode. The JDK vm accepts command line
arguments that starts the vm in the appropriate mode, e.g.,

java -Xdebug -Xrunjdwp:transport=dt_socket,address=4444,server=y,suspend=n MyClass

starts MyClass in debugger server mode at the socket address
4444. See jdb in the tools section of the JDK documentation for
more information on these arguments.

Selecting the Server mode option of the `jde-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The attach command connects the debugger to the debuggee at the
address specified by `jde-db-option-connect-socket'. If this variable
is nil, this command prompts you to enter the address."
  (interactive)
  (let* ((socket (jde-jdb-get-socket-address))
	 (host (nth 0 socket))
	 (port (nth 1 socket)))
    (if (string= port "")
	(error "You must specify the port of the debuggee process.")
      (let* ((debugger (jde-jdb-get-jdb))
	     (connector
	      (jde-db-socket-attach-connector
	       "connector"
	       :host host
	       :port port))
	     (debuggee
	      (jde-jdb-debuggee-app
	      "debuggee"
	      :main-class (format
			   "Attached to socket %s:%s"
			   (if host host "localhost")
			   port)
	      :connector connector)))
	(oset debugger the-debugger debugger)
	(oset debugger :debuggee debuggee)
	(jde-jdb-connect debugger)))))


(defun jde-jdb-get-shared-memory-name ()
  (if jde-db-option-connect-shared-memory-name
      jde-db-option-connect-shared-memory-name
    (read-from-minibuffer  "Shared memory name: " "javadebug")))


(defun jde-jdb-attach-via-shared-memory ()
  "Launch jdb in attach mode. In this mode, jdb connects itself to an
existing debuggee process. This command specifies use of shared memory
as the connection transport. The debuggee process itself must have
been launched in debugger server mode. The JDK vm accepts command line
arguments that starts the vm in the appropriate mode, e.g.,

java -Xdebug -Xrunjdwp:transport=dt_shmem,address=javadebug,server=y,suspend=n MyClass

starts MyClass in debugger server mode, specifying \"javadebug\" as
the name of the shared memory transport. See jdb in the tools section
of the JDK documentation for more information on these arguments.

Selecting the Server mode option of the `jde-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The attach command connects the debugger to the debuggee at the
address specified by `jde-db-option-connect-shared-memory', or, if
this variable is nil, this command prompts you to enter a name."
  (interactive)
  (assert
   (eq system-type 'windows-nt)
   "The debugger does not support shared memory connections on this platform.")
  (let ((shmem-name (jde-jdb-get-shared-memory-name)))
    (if (string= shmem-name "")
	(error "Shared memory name required.")
      (let* ((debugger (jde-jdb-get-jdb))
	     (connector
	      (jde-db-shared-memory-attach-connector
	       "connector"
	       :name shmem-name))
	     (debuggee
	      (jde-jdb-debuggee-app
	       "debuggee"
	       :main-class (format
			    "Attached via shared memory: %s."
			    shmem-name)
	       :connector connector)))
	(oset debugger the-debugger debugger)
	(oset debugger :debuggee debuggee)
	(jde-jdb-connect debugger)))))


(defun jde-jdb-get-socket-listen-port ()
  (if jde-db-option-connect-socket
      (nth 1 jde-db-option-connect-socket)
    (read-from-minibuffer "Listen port: " "4444")))

(defun jde-jdb-listen-via-socket ()
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

Selecting the Client mode option of the `jde-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The listen command listens for the debugger at the
address specified by `jde-db-option-connect-socket'. If this variable
is nil, this command prompts you to enter the address. You can enter
either the address that you plan to start the debuggee process at
(e.g., jdbconn) or a null string.  If you enter a null string, this
command startes the debugger in a mode that accepts connections from
any debuggee process started in debugger client mode, regardless of
address."
  (interactive)
  (let* ((debugger (jde-jdb-get-jdb))
	 (port (jde-jdb-get-socket-listen-port))
	 (connector
	   (jde-db-socket-listen-connector
	       "connector"
	       :port port))
	 (debuggee
	  (jde-jdb-debuggee-app
	   "debuggee"
	  :main-class (concat "Listening at port " port)
	  :connector connector)))
    (oset debugger the-debugger debugger)
    (oset debugger :debuggee debuggee)
    (jde-jdb-connect debugger)))

(defun jde-jdb-listen-via-shared-memory ()
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

Selecting the Client mode option of the `jde-run-option-debug' customization
variable causes the JDEE to specify the appropriate command-line
arguments when launching the debuggee process.

The listen command listens for the debugger at the
address specified by `jde-db-option-connect-shared-memory'. If this variable
is nil, this command prompts you to enter the address that you plan to start
the debuggee process at (e.g., jdbconn)."
  (interactive)
  (assert
   (eq system-type 'windows-nt)
   "The debugger does not support shared memory connections on this platform.")
  (let* ((debugger (jde-jdb-get-jdb))
	 (name (jde-jdb-get-shared-memory-name))
	 (connector
	   (jde-db-shared-memory-listen-connector
	       "connector"
	       :name name))
	 (debuggee
	  (jde-jdb-debuggee-app
	   "debuggee"
	  :main-class (concat "Listening to " name)
	  :connector connector)))
    (oset debugger the-debugger debugger)
    (oset debugger :debuggee debuggee)
    (jde-jdb-connect debugger)))

(defun jde-jdb-display (key)
  "Print expression at point."
  (interactive "sExpression: ")
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       cmd)
	  (if (string= "print" key)
	      (setq cmd (oref cmd-set print))
	    (if (string= "dump" key)
		(setq cmd (oref cmd-set dump))
	      (if (string= "eval" key)
		  (setq cmd (oref cmd-set eval))
		(if (string= "set" key)
		    (setq cmd (oref cmd-set set-var))
		  (if (string= "locals" key)
		      (setq cmd (oref cmd-set locals)))))))
	  (jde-db-exec-cmd debugger cmd))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-jdb-print ()
  (interactive)
  (jde-jdb-display "print"))

(defun jde-jdb-dump ()
  (interactive)
  (jde-jdb-display "dump"))

(defun jde-jdb-eval ()
  (interactive)
  (jde-jdb-display "eval"))

(defun jde-jdb-locals ()
  (interactive)
  (jde-jdb-display "locals"))

(defun jde-jdb-set ()
  (interactive)
  (jde-jdb-display "set"))

(defun jde-jdb-help ()
  (interactive)
  (let* ((jde-dir (jde-find-jde-doc-directory))
	 (jdb-ug-path
	  (if jde-dir
	      (expand-file-name "doc/html/jdb-ug/jdb-ug-frame.html" jde-dir))))
    (if (and
	 jdb-ug-path
	 (file-exists-p jdb-ug-path))
	(browse-url (concat "file://" (jde-convert-cygwin-path jdb-ug-path))
		    (if (boundp 'browse-url-new-window-flag)
			'browse-url-new-window-flag
		      browse-url-new-window-p))
      (signal 'error '("Cannot find jdb user guide.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debug Commands                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar jde-jdb-emacs-menu-spec
  (list "Jdb"

	["Step Over"
	     jde-debug-step-over
	     :active (jde-db-debuggee-stopped-p)
	     :help "Step over the next method."]


	["Step Into"
	 jde-debug-step-into
	 :active (jde-db-debuggee-stopped-p)
	 :help "Step into the next method."]

	["Step Out"
	 jde-debug-step-out
	 :active (jde-db-debuggee-stopped-p)
	 :help "Step out of the current method."]

	["Run"
	 jde-debug-run
	 :active   (and
		    (slot-boundp 'jde-db-debugger 'the-debugger)
		    (let* ((debugger (oref 'jde-db-debugger the-debugger))
			   (debuggee (oref debugger debuggee))
			   (debuggee-status (oref debuggee status)))
		      (and (oref debugger running-p)
			   (not (oref debuggee-status running-p)))))

	 :included (or
		    (not (slot-boundp 'jde-db-debugger 'the-debugger))
		    (let* ((debugger (oref 'jde-db-debugger the-debugger))
			   (debuggee (oref debugger debuggee))
			   (debuggee-status (oref debuggee status)))
		      (or (not (oref debugger running-p))
			  (not (oref debuggee-status running-p)))))
	 :help "Start the current program."]

	["Continue"
	 jde-debug-cont
	 :active   (or
		    (jde-db-debuggee-stopped-p)
		    (jde-db-debuggee-suspended-p))

	 :included  (jde-db-debuggee-running-p)
	 :help "Continue the current program."]

	["Quit"
	 jde-debug-quit
	 :active  (jde-db-debuggee-running-p)
	 :help "Terminate the current debugging session."]

	"-"

	["Toggle Breakpoint"
	 jde-debug-toggle-breakpoint
	 :active t
	 :help "Set (or remove) a breakpoint at the current line."]

	["Clear Breakpoints"
	 jde-debug-clear-breakpoints
	 :active  jde-db-breakpoints
	 :help "Remove all breakpoints."]

	["List Breakpoints"
	 jde-debug-list-breakpoints
	 :active  jde-db-breakpoints
	 :help "Display a list of breakpoints."]

	"-"

	(list
	 "Display"

	 ["Expression"
	  jde-jdb-print
	  :active  (jde-db-debuggee-stopped-p)
	  :help "Evaluate an expression and display the results."]

	 ["Object"
	  jde-jdb-dump
	  :active  (jde-db-debuggee-stopped-p)
	  :help "Display the fields of an object referenced by a variable."]


	 ["Locals"
	  jde-jdb-locals
	  :active  (jde-db-debuggee-stopped-p)
	  :help "Display the variables in scope at the current line."]
	 )

	 ["Set Variable"
	  jde-jdb-set
	  :active  (jde-db-debuggee-stopped-p)
	  :help "Change the value of an in-scope variable."]

	(list
	 "Stack"

	 ["Up"
	  jde-debug-up
	  :active  (jde-db-debuggee-stopped-p)
	  :help "Move the debug cursor up the method call stack."]

	 ["Down"
	  jde-debug-down
	  :active (and
		   (jde-db-debuggee-stopped-p)
		   (let* ((debugger (oref 'jde-db-debugger the-debugger))
			  (debuggee (oref debugger debuggee)))
		     (> (jde-jdb-string-to-int
			 (oref debuggee :stack-depth)) 1)))
	  :help "Move the debug cursor down the method call stack." ]

	 ["Where"
	  jde-debug-where
	  :active (jde-db-debuggee-stopped-p)
	  :help "Display the call stack."]

	 )
	"-"
	(list
	 "External Process"
	 ["Attach Via Socket"
	  jde-jdb-attach-via-socket
	  :active (not (jde-db-debuggee-running-p))
	  :help "Attach the debugger to an external process via a socket."]
	 ["Attach Via Shared Memory"
	  jde-jdb-attach-via-shared-memory
	  :active (and
		   (eq system-type 'windows-nt)
		   (not (jde-db-debuggee-running-p)))
	  :help "Attach the debugger to an external process via a shared memory connection."]
	 ["Listen Via Socket"
	  jde-jdb-listen-via-socket
	  :active (not (jde-db-debuggee-running-p))
	  :help "Listen at a socket for an external process."]
	 ["Listen Via Shared Memory"
	  jde-jdb-listen-via-shared-memory
	  :active (and
		   (eq system-type 'windows-nt)
		   (not (jde-db-debuggee-running-p)))
	  :help "Listen in shared memory for an external process."]
	 )
	"-"

	["Preferences"
	 jde-bug-show-preferences
	 :active nil
	 :help "Not yet implemented."]

	"-"
	["Help"
	 jde-jdb-help
	 :active t
	 :help "Display the JDEE's jdb user's guide in an HTML browser."]
	)
  "Defines the Jdb menu for Emacs.")

(defvar jde-jdb-xemacs-menu-spec
  (list "Jdb"

	["Step Over"
	     jde-debug-step-over
	     :active (jde-db-debuggee-stopped-p)]


	["Step Into"
	 jde-debug-step-into
	 :active (jde-db-debuggee-stopped-p)
	 ]

	["Step Out"
	 jde-debug-step-out
	 :active (jde-db-debuggee-stopped-p)]

	["Run"
	 jde-debug-run
	 :active   (and
		    (slot-boundp 'jde-db-debugger 'the-debugger)
		    (let* ((debugger (oref 'jde-db-debugger the-debugger))
			   (debuggee (oref debugger debuggee))
			   (debuggee-status (oref debuggee status)))
		      (and (oref debugger running-p)
			   (not (oref debuggee-status running-p)))))

	 :included (or
		    (not (slot-boundp 'jde-db-debugger 'the-debugger))
		    (let* ((debugger (oref 'jde-db-debugger the-debugger))
			   (debuggee (oref debugger debuggee))
			   (debuggee-status (oref debuggee status)))
		      (or (not (oref debugger running-p))
			  (not (oref debuggee-status running-p)))))]

	["Continue"
	 jde-debug-cont
	 :active   (or
		    (jde-db-debuggee-stopped-p)
		    (jde-db-debuggee-suspended-p))

	 :included  (jde-db-debuggee-running-p)]

	["Quit"
	 jde-debug-quit
	 :active  (jde-db-debuggee-running-p)]

	"-"

	["Toggle Breakpoint"
	 jde-debug-toggle-breakpoint
	 t]

	["Clear Breakpoints"
	 jde-debug-clear-breakpoints
	 jde-db-breakpoints]

	["List Breakpoints"
	 jde-debug-list-breakpoints
	 jde-db-breakpoints]
	"-"

	(list
	 "Display"

	 ["Expression"
	  jde-jdb-print
	  :active  (jde-db-debuggee-stopped-p)]

	 ["Object"
	  jde-jdb-dump
	  :active  (jde-db-debuggee-stopped-p)]


	 ["Locals"
	  jde-jdb-locals
	  :active  (jde-db-debuggee-stopped-p)]
	 )

	 ["Set Variable"
	  jde-jdb-set
	  :active  (jde-db-debuggee-stopped-p)]

	(list
	 "Stack"

	 ["Up"
	  jde-debug-up
	  :active  (jde-db-debuggee-stopped-p)]

	 ["Down"
	  jde-debug-down
	  :active (and
		   (jde-db-debuggee-stopped-p)
		   (let* ((debugger (oref 'jde-db-debugger the-debugger))
			  (debuggee (oref debugger debuggee)))
		     (> (jde-jdb-string-to-int
			 (oref debuggee :stack-depth)) 1)))]

	 ["Where"
	  jde-debug-where
	  :active (jde-db-debuggee-stopped-p)]

	 )
	"-"
	(list
	 "External Process"
	 ["Attach Via Socket"
	  jde-jdb-attach-via-socket
	  :active (not (jde-db-debuggee-running-p))]
	 ["Attach Via Shared Memory"
	  jde-jdb-attach-via-shared-memory
	  :active (and
		   (eq system-type 'windows-nt)
		   (not (jde-db-debuggee-running-p)))]
	 ["Listen Via Socket"
	  jde-jdb-listen-via-socket
	  :active (not (jde-db-debuggee-running-p))]
	 ["Listen Via Shared Memory"
	  jde-jdb-listen-via-shared-memory
	  :active (and
		   (eq system-type 'windows-nt)
		   (not (jde-db-debuggee-running-p)))]

	 )
	"-"
	["Preferences"
	 jde-bug-show-preferences nil]
	"-"
	["Help"
	 jde-jdb-help t]
	)
  "Defines the JDE's menu of jdb commands.")


(defvar jde-jdb-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define jde-jdb-menu km "Jdb Minor Mode Menu"
		      jde-jdb-emacs-menu-spec)
    km)
  "Keymap for Jdb minor mode.")

(defvar jde-jdb-minor-mode nil
  "Non-nil if jdb minor mode is enabled.")
(make-variable-buffer-local 'jde-jdb-minor-mode)

(defun jde-jdb-minor-mode (&optional arg)
  "Toggle jdb minor mode.
With prefix argument ARG, turn on if positive, otherwise off..

\\{jde-jdb-mode-map}"
  (interactive
   (list (or current-prefix-arg
	     (if jde-jdb-minor-mode 0 1))))

  (setq jde-jdb-minor-mode
	(if arg
	    (>
	     (prefix-numeric-value arg)
	     0)
	  (not jde-jdb-minor-mode)))

  (if jde-jdb-minor-mode
      (if (featurep 'xemacs)
	    (easy-menu-add jde-jdb-xemacs-menu-spec jde-jdb-mode-map))
    (if (featurep 'xemacs)
      (easy-menu-remove jde-jdb-xemacs-menu-spec))))

(semantic-add-minor-mode 'jde-jdb-minor-mode " jdb" jde-jdb-mode-map)


;; (fmakunbound 'jde-jdb-key-bindings)
(defcustom jde-jdb-key-bindings
  (list (cons "[?\C-c ?\C-a ?\C-s]" 'jde-debug-step-over)
	(cons "[?\C-c ?\C-a ?\C-n]" 'jde-debug-step-into)
	(cons "[?\C-c ?\C-a ?\C-o]" 'jde-debug-step-out)
	(cons "[?\C-c ?\C-a ?\C-c]" 'jde-debug-cont)
	(cons "[?\C-c ?\C-a ?\C-r]" 'jde-debug-run)
	(cons "[?\C-c ?\C-a ?\C-b]" 'jde-debug-toggle-breakpoint)
	(cons "[?\C-c ?\C-a ?\C-u]" 'jde-debug-up)
	(cons "[?\C-c ?\C-a ?\C-d]" 'jde-debug-down)
	(cons "[?\C-c ?\C-a ?\C-p]" 'jde-jdb-print)
	(cons "[?\C-c ?\C-a ?\C-d]" 'jde-jdb-dump)
	(cons "[?\C-c ?\C-a ?\C-e]" 'jde-jdb-eval)
	(cons "[?\C-c ?\C-a ?\C-v]" 'jde-jdb-set)
	(cons "[?\C-c ?\C-a ?\C-l]" 'jde-jdb-locals))
  "*Specifies key bindings for jdb debug commands.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  ;; Unmap existing key bindings
	  (if (and
	       (boundp 'jde-jdb-key-bindings)
	       jde-jdb-key-bindings)
	      (mapc
	       (lambda (binding)
		 (let ((key (car binding))
		       (fcn (cdr binding)))
		   (if (string-match "\\[.+]" key)
		       (setq key (car (read-from-string key))))
		   (define-key jde-jdb-mode-map key nil)))
	       jde-jdb-key-bindings))
	  ;; Map new key bindings.
	  (mapc
	   (lambda (binding)
	     (let ((key (car binding))
		   (fcn (cdr binding)))
	       (if (string-match "\\[.+]" key)
		   (setq key (car (read-from-string key))))
	       (define-key jde-jdb-mode-map key fcn)))
	   val)
	  (set-default sym val)))

(defun jde-jdb-string-to-int(number)
  "This method removes punctuation from a string, e.g, 1,200 (1.200 in Danish),
and converts the result to an integer."
  (if (string-match "[^[:digit:]]" number)
      (setq number (replace-match "" nil nil number)))
  (string-to-int number))

(provide 'jde-jdb)

;; Change History
;; $Log: jde-jdb.el,v $
;; Revision 1.48  2005/01/18 05:23:30  paulk
;; Change variables named assoc to assoc-x. This is intended to fix a "Symbol's value as variable is void: old-assoc" problem when debugging with the compiled version of JDE in xemacs/cygwin. Thanks to Henry S. Thompson.
;;
;; Revision 1.47  2004/12/17 05:22:25  paulk
;; Fix regression that caused DOS window to appear when running the debugger.
;;
;; Revision 1.46  2004/12/08 12:49:18  paulk
;; Updated regular expressions to accommodate non-English punctuation styles for numeric
;; expressions in jdb messages, e.g., 1.200 for the English 1,200. Thanks to Morten B. Isaksen.
;;
;; Revision 1.45  2004/10/18 03:37:34  paulk
;; Wrap call to comint-exec in save-w32-show-window macro.
;;
;; Revision 1.44  2004/10/03 03:33:12  paulk
;; Fix regression caused by renaming jde-jdb-debuggee as jde-jdb-debuggee-app.
;;
;; Revision 1.43  2004/09/20 05:29:34  paulk
;; Changes needed to make restructured debugging code work for applets.
;;
;; Revision 1.42  2004/09/02 04:49:01  paulk
;; More infrastructure work to support debugging of applets.
;;
;; Revision 1.41  2004/09/01 06:21:29  paulk
;; Restructured to accommodate debugging applets.
;;
;; Revision 1.40  2004/06/22 01:56:44  jslopez
;; Removes extra point from the class being passed to jde-db-set-debug-cursor.
;; It has an extra dot i.e. com.company..MyClass.
;;
;; Revision 1.39  2004/05/12 02:43:40  jslopez
;; Adds safety check to set the stack depth to 1 if it is not set. This can
;; happens if the regexpression fails, i.e A thread name with a character not in
;; the regexpression. Enhances the regexpression to parse the line number to allow
;; number larger than 999. Enhances the regexpression that parser the where
;; command to allow spaces and '-' since those are valid thread names. In
;; additions, adds support for line number larger than 999. Fixes unbound variable
;; "port" in jde-jdb-connect method.
;;
;; Revision 1.38  2004/02/18 13:48:21  jslopez
;; When a breakpoint was requested on a line that does not contain code and jdb
;; responded with "Unable to set breakpoint" it was being handle as a set
;; breakpoint,i.e. the breakpoint was being activated. Modifies
;; jde-jdb-set-breakpoint-listener to ignore this message. Modifies
;; jde-db-cmd-notify-response to delete the breakpoint in this situation. In
;; addition, adds a check for a null bp, in some instances it was null causing an
;; error.
;;
;; Revision 1.37  2003/09/18 05:30:41  paulk
;; Replace the attach and listen commands with separate commands for
;; attaching/listening via sockets and shared memory. This is necessary to support attaching via sockets on Windows platforms.
;;
;; Revision 1.36  2003/06/30 03:58:11  paulk
;; Provided help strings for Jdb menu items.
;;
;; Revision 1.35  2003/06/15 04:35:29  paulk
;; Fix docstrings for jde-jdb-attach and jde-jdb-listen.
;;
;; Revision 1.34  2003/06/12 04:06:19  paulk
;; Cosmetic fix to set-var command.
;;
;; Revision 1.33  2003/06/02 05:08:51  paulk
;; Reorganized Jdb->Display menu. Changed names of commands to reflect purpose, e.g., Display->Print
;; becomes Display->Expression. Deleted Display-Eval because it is the same as Display->Expression.
;; Moved Set (variable) command from the DIsplay submenu to the toplevel Jdb menu. Changed
;; first Jdb->Set Variable prompt from "Expression: " to Variable: ".
;;
;; Revision 1.32  2003/05/27 04:05:15  paulk
;; Adds jde-jdb-help command. This command displays the JDB user's guide.
;;
;; Revision 1.31  2003/05/14 11:09:30  paulk
;; Fix regression caused by renaming jdb classes.
;;
;; Revision 1.30  2003/05/14 06:38:48  paulk
;; Fix jde-jdb-attach and jde-jdb-listen to reflect replacement of
;; jde-db-option-connect-address with jde-db-option-listen-address and
;; jde-db-option-attach-address.
;;
;; Revision 1.29  2003/05/13 05:06:04  paulk
;; The attach and listen commands now prompt for a connect address if jde-db-connect-address
;; is nil. Also provided extensive doc strings for these commands.
;;
;; Revision 1.28  2003/02/25 17:19:59  jslopez
;; Fixes bug setting breakpoints that will leave breakpoints mark as
;; deferred even though they are not.
;;
;; Revision 1.27  2003/02/25 17:09:11  jslopez
;; Fixes regression bug. Updates the remaining jde-db-debuggee to jde-jdb-debuggee.
;;
;; Revision 1.26  2003/02/25 06:53:29  paulk
;; Created a generalized jde-debug command and wired it up to jdb.
;; Next step is to wire it up to JDEbug.
;;
;; Revision 1.25  2003/02/25 04:37:15  jslopez
;; Sets a main-class when attaching or listening through jdb to avoid
;; an unbound error.
;;
;; Revision 1.24  2003/02/24 18:03:28  jslopez
;; Fixes bug in jde-db-listener-filter-output.
;;
;; Revision 1.23  2003/01/12 19:17:17  jslopez
;; Adds command List Breakpoints to the JDB menu.
;;
;; Revision 1.22  2003/01/09 12:10:13  jslopez
;; Exposes the jdb methods: print, dump, eval, set, and locals.
;;
;; Revision 1.21  2003/01/08 21:49:13  jslopez
;; Fixes typo in menu.
;;
;; Revision 1.20  2003/01/07 00:05:32  jslopez
;; Fixes bug that would parse line number higher than 999.
;; JDB prints out the number as 1,999.
;;
;; Revision 1.19  2002/11/11 05:24:26  paulk
;; No need to add .exe to jdb path thanks to Mac compatibility fix.
;;
;; Revision 1.18  2002/11/05 07:56:20  paulk
;; Mac OS X (darwin) compatibility fix: find paths of jdb and appletviewer on the Mac. Thanks to Andrew Hyatt.
;;
;; Revision 1.17  2002/10/16 04:59:57  paulk
;; Debug cursor now works in files that do not belong to a package. Thanks to Andy Piper.
;;
;; Revision 1.16  2002/06/17 07:24:08  paulk
;; Updated the JDEE's applet debugging command to
;; work with its new jdb interface.
;;
;; Revision 1.15  2002/05/21 06:35:20  paulk
;; Updated to support J2SDK 1.4.0 version of jdb.
;;
;; Revision 1.14  2002/03/06 13:00:18  paulk
;; * Removed references to obsolete jde-db-option-attach variable.
;; * The jdb launch, attach, and listen commands now update the
;;   the-debugger field in the jde-db-debugger class.
;;
;; Revision 1.13  2002/03/04 06:43:41  paulk
;; Adds support for connecting debugger to an independently started
;; process, using either attach or listen mode.
;;
;; Revision 1.12  2002/02/08 12:04:00  paulk
;; Completed implementation of step-into and step-out commands.
;;
;; Revision 1.11  2002/02/04 05:47:17  paulk
;; Added code to rehighlight breakpoints if the user kills a
;; buffer for a source file that contains breakpoints and
;; then reopens the file.
;;
;; Revision 1.10  2002/01/15 13:34:24  paulk
;; Adds a Clear Breakpoints command for jdb.
;;
;; Revision 1.9  2002/01/14 13:33:57  paulk
;; - Now defines three breakpoint marker colors: green for a specified breakpoint,
;;   yellow for a requested breakpoint, and red for an enabled breakpoint.
;;
;; - The debug application command now requests all specified
;;   breakpoints at the beginning of a debug session.
;;
;; - The debug application command now changes the color of all breakpoints
;;   to green (specified) at the end of a debug session.
;;
;; Revision 1.8  2002/01/11 05:45:23  paulk
;; - Use overlays/extents to record location of breakpoints in a buffer.
;; - Use different colors to indicate requested and enabled breakpoints.
;;
;; Revision 1.7  2002/01/02 05:34:31  paulk
;; * Fixed some bugs in jdb stack navigation commands.
;; * Moved the where command out processing from the jdb stack listener
;;   to the whre cmd response method where it belongs.
;; * Added key bindings for jdb commands.* Fixed some bugs in jdb stack navigation commands.
;;
;; Revision 1.6  2001/12/31 07:54:39  paulk
;; Implemented jdb versions of generalized
;; quit, step-over, step-into, stack up, stack down,
;; and stack where commands.
;;
;; Revision 1.5  2001/12/28 05:35:45  paulk
;; * Implemented jdb versions of generalized stop and continue process commands.
;;
;; * Implemented breakpoint and stack message listeners.
;;
;; Revision 1.4  2001/12/17 08:07:47  paulk
;; jdb implementation of generalized clear breakpoint command.
;;
;; Revision 1.3  2001/12/10 04:29:54  paulk
;; Created generalized breakpoint framework. Provided initial
;; implementation for jdb. A lot of work remains.
;;
;; Revision 1.2  2001/12/04 06:05:36  paulk
;; Removed carriage returns.
;;
;; Revision 1.1  2001/12/04 05:25:40  paulk
;; Initial revision.
;;

;;; end of jde-jdb.el
