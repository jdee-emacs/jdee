;; jde-ant.el --- Use Apache Ant to build your JDE projects
;; $Id$

;; Copyright (C) 2009 by Paul Landes
;; Author: Jason Stell | jason.stell@globalone.net
;; Author: Kevin A. Burton ( burton@openprivacy.org )
;; Created: 19 Oct 2000
;; Version 1.4.4

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

;; Commentary:
;; This file defines jde-ant-build and some helper functions.
;; jde-ant-build uses the specified ant program/shell script to
;; execute a specified build file (in the project root).
;;
;;
;; TODO:
;;
;; Notes:
;; -- The JDE (Java Development Environment for Emacs) can be
;;    downloaded at http://jdee.sunsite.dk
;;
;; -- Apache Ant is a Java & XML build system that can be downloaded
;;    at http://jakarta.apache.org/ant/
;;
;;
;;; History:
;;
;; - Version 1.4.4 Thu Jan 17 2002 03:51 PM (burton@openprivacy.org):
;;
;;    - We now have `jde-ant-build-hook' that runs hooks after a build is
;;      started.
;;
;; - Version 1.4.3 Thu Jan 17 2002 03:37 PM (burton@openprivacy.org):
;;
;;    - fixed a bug with target completion.  We were not using initial-input
;;      correctly.
;;
;;    - fixed a bug with jde-ant-build-classpath which wasn't using ant-home
;;      correctly.  (Thanks to Javier S. Lopez)
;;
;; - Version 1.4.2 Wed Jan 16 2002 05:46 PM (burton@openprivacy.org): added
;; `jde-ant-use-global-classpath' (which is disabled by default) so that the
;; `jde-global-classpath' can be used.
;;
;; -- Version 1.3 (19 June 2001)
;;    : Addition of jde-ant-projecthelp to display list of targets for
;;      the current buildfile.
;; -- Version 1.2 (4 June 2001)
;;    : Addition of jde-ant-read-buildfile option to prompt for the buildfile
;;      name -- contributed by Rob Shaw <shaw@servidium.com>
;;    : Various Bug fixes contributed by Rob Shaw <shaw@servidium.com>
;;        - The setting of the system property in a format other
;;          than -Dname=value had the side effect of negating the -emacs
;;          command line argument.
;;        - The setting of the current directory to the location of the
;;          JDE project file is now taking place when
;;          jde-ant-enable-find is nil.
;;        - The ant target is now the last thing to be appended to
;;          ant command to avoid any possible confusion for ANT
;;          as to what is the desired target.
;; -- Version 1.2b2 (25 May 2001)
;;    : Fix to properly use the -find <buildfile> Ant switch--contributed
;;      by Rob Shaw <shaw@servidium.com>.
;; -- Version 1.2b1 (23 May 2001)
;;    : Added jde-ant-enable-find custom flag to use the -find switch
;;      available in Ant. This overrides the requirement for a JDE
;;      project file
;;    : Fixed minor bug missing whitespace before -buildfile switch
;;      when building the ant compile command
;; -- Version 1.1 (20 October 2000)
;;    : Added interactive prompts (optional, based on customizable
;;      toggles) for Ant target and additional args. Removed the
;;      jde-ant-target custom variable, since this is really
;;      represented by the default target in the build file.
;;    : The -f switch seems to be causing problems. Removed it from
;;      the default jde-ant-args.
;;    : Basic changes to the way the ant command is assembled.
;;
;; -- Version 1.0 (19 October 2000)
;;    Initial Version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup jde-ant nil
  "JDE Ant"
  :group 'jde
  :prefix "jde-ant-")

;;; start of Douglas WF Acheson mod
;;; The next three custom variables and defun added by Douglas WF Acheson to
;;; allow a user to select how ant is invoked, either by script or via Java

(defcustom jde-ant-invocation-method (list "Script")
  "*Specifies how to invoke ant. Ant can be invoked in one of three
ways. The first is via the ant script/program that comes with ant.
The second is via java and the third is via the Ant Server."
  :group 'jde-ant
  :type '(list
	   (radio-button-choice
	     (const "Script")
	     (const "Java")
	     (const "Ant Server"))))

(defcustom jde-ant-home ""
  "*Directory where ant is installed."
  :group 'jde-ant
  :type  'file)

(defcustom jde-ant-user-jar-files nil
  "*Specifies jar files that hold user-defined tasks."
  :group 'jde-ant
  :type '(repeat (file :tag "Path")))

(defcustom jde-ant-program "ant"
  "*Specifies name of ant program/script."
 :group 'jde-ant
 :type 'string)

(defcustom jde-ant-args "-emacs"
  "*Specifies arguments to be passed to make program."
  :group 'jde-ant
  :type 'string)

(defcustom jde-ant-buildfile "build.xml"
  "*Specifies the default buildfile to use."
  :group 'jde-ant
  :type 'string)

(defcustom jde-ant-read-buildfile nil
"*Specify whether to prompt for a buildfile. If non-nil, the jde-ant-build
command prompts you for an ant buildfile.  Note that when `jde-ant-enable-find'
is enable the value entered for `jde-ant-read-buildfile' is used as a
default. If no value is entered, or the file is non-existent, or is a
directory, the code tries to find the buildfile."

  :group 'jde-ant
  :type 'boolean)

(defcustom jde-ant-read-target nil
"*Specify whether to prompt for a build target. If non-nil, the
jde-ant-build command prompts you for an ant target."
  :group 'jde-ant
  :type 'boolean)

(defvar jde-ant-interactive-buildfile nil
  "Defauilt buildfile to use when prompting interactively.")

(defvar jde-ant-interactive-target-history nil
  "History of targets entered in the minibuffer.")

(defcustom jde-ant-read-args nil
"*Specify whether to prompt for additional arguments to pass to ant. If
non-nil, the jde-ant-build command prompts you for the additional arguments."
  :group 'jde-ant
  :type 'boolean)

(defvar jde-ant-interactive-args-history nil
"History of targets entered in the minibuffer.")

(defvar jde-ant-buildfile-history nil
 "History of targets entered in the minibuffer.")

(defvar jde-ant-passed-security-exception nil
  "This variable is used to indicate that we have passed the
java.lang.SecurityException in the output. The JDESecurityManager throws an
exception when ANT tries exiting the JVM using System.exit(0). This exception
causes an stack trace in the compilation buffer. This variable is used to
indicate the start of the exception, therefore no more output should be
inserted into the buffer")

(defvar jde-ant-build-status nil
  "Used to indicated the status of build, success or failure")


(defcustom jde-ant-enable-find nil
"*Specify whether jde-ant find the build.xml file based on your current
directory. If non-nil, we will search up the directory hierarchy from the
current directory for the build definition file. Also note that, if non-nil,
this will relax the requirement for an explicit jde project file.  If
`jde-ant-read-buildfile' is enable that value is used as a default if valid."
   :group 'jde-ant
   :type 'boolean)

(defcustom jde-ant-complete-target t
  "*Specify whether to enable completion of build target names in the
minibuffer.

If non-nil, the jde-ant-build command allows you to use tab completion
in the minibuffer to specify the build target name.  This list of
valid build targets is determined by parsing the Ant build file.  This
option has no effect if jde-ant-read-target is nil."
  :group 'jde-ant
  :type 'boolean)

(defcustom jde-ant-use-global-classpath nil
  "*Specify whether to enable use of `jde-global-classpath' when running jde-ant."
  :group 'jde-ant
  :type 'boolean)

(defcustom jde-ant-target-regexp "<\\s-*target\\s-[^...]*?name\\s-*=\\s-*\"\\s-*\\([^\"]+\\)"
  "*Regular expression used to match target names in Ant build files."
  :group 'jde-ant
  :type 'string)

(defcustom jde-ant-build-hook '(jde-compile-finish-kill-buffer
				jde-compile-finish-refresh-speedbar
				jde-compile-finish-update-class-info)
  "*List of hook functions run by `jde-ant-build' (see `run-hooks'). Each
function should accept two arguments: the compilation buffer and a string
describing how the compilation finished"
  :group 'jde-ant
  :type 'hook)

(defcustom jde-ant-working-directory ""
  "*Path of the working directory to use in 'ant' build mode. This string must
end in a slash, for example, c:/foo/bar/ or ./ . If this string is empty, the
'ant' build mode uses the current file location as its working directory."
  :group 'jde-ant
  :type 'string)

(defun jde-build-ant-command (target more-args &optional buildfile)
  "Constructs the java ant command. The variable `jde-ant-home' is used
if it is set, otherwise it gets the ant home from the environment
variable ANT_HOME."

  ;;provide a default buildfile.
  (when (null buildfile)
    (setq buildfile jde-ant-buildfile))

  (let* ((ant-home (jde-ant-get-ant-home))
	 (delimiter (if (or
			 (string= (car jde-ant-invocation-method) "Java")
			 (and (string= (car jde-ant-invocation-method)
				       "Script")
			      (not (featurep 'xemacs))))
			"'"
		      "\""))
	 (classpath-delimiter  (if (and (or (eq system-type 'windows-nt)
			 (eq system-type 'cygwin32))
		     (string-match "sh$" shell-file-name))
		delimiter))
	 (buildfile-delimiter  (if (eq system-type 'windows-nt)
				   "\"" delimiter))
	 (ant-program (if (or (string-match "\\\\" jde-ant-program)
			      (string-match "/" jde-ant-program))
			  (jde-normalize-path jde-ant-program)
			jde-ant-program))
	 (ant-command
	  (concat
	   (if (string= (car jde-ant-invocation-method) "Script") ant-program)
	   (if (string= (car jde-ant-invocation-method) "Java")
	       (concat
		(jde-get-jdk-prog 'java)
		" -classpath "
		classpath-delimiter
		(jde-ant-build-classpath)
		classpath-delimiter))
	   (if ant-home
	       (concat
		" -Dant.home="
		(if (or
		     (string-match " " ant-home) ;; Quote path if it
		     (string-match "." ant-home));; contains a space
		    (concat delimiter ant-home delimiter)  ;; or period.
		  ant-home)))
	  (if (string= (car jde-ant-invocation-method) "Java")
	      (concat
	       " "
	       "org.apache.tools.ant.Main")))))

    (if (not (string= buildfile ""))
	(setq ant-command
	      (concat ant-command
		      " -buildfile " buildfile-delimiter
		      (jde-normalize-path buildfile)
		      buildfile-delimiter)))

    (if (not (string= jde-ant-args ""))
	(setq ant-command (concat ant-command " " jde-ant-args)))

    (if (and (not (null more-args))
	     (not (string= more-args "")))
	(setq ant-command (concat ant-command " " more-args)))

    (if (not (string= target ""))
	(setq ant-command (concat ant-command " " target " ")))

    ant-command))

(defun jde-ant-build-classpath()
  "Build the classpath we should use when running ant.  This returns a
classpath normalized with `jde-build-classpath'."

  (let* ((ant-home (jde-ant-get-ant-home))
	 classpath)

    (setq classpath (append (list (expand-file-name "lib" ant-home)
				  (jde-get-tools-jar))
			    jde-ant-user-jar-files))

    ;; silence the compiler
    ;; TODO: remove this boundp and require 'jde after resolving jde
    ;; compilation warnings
    (with-no-warnings
      (when jde-ant-use-global-classpath
	(setq classpath (append classpath jde-global-classpath))))

    (jde-build-classpath classpath)))

(defun jde-ant-get-ant-home ()
  "Calculate an appropriate ant home."
  (let ((ant-home
	 (if (string= jde-ant-home "")
	     (getenv "ANT_HOME")
	   jde-ant-home)))
    (if ant-home
	(jde-normalize-path ant-home))))

(defun jde-ant-interactive-get-buildfile ()
  "Get a buildfile interactively.  This is used so that code that needs to read
  a buildfile from interactive can share the same type of behavior.  This will
  return a new filename which points to the build.xml file to use."

  (let (buildfile)

    (if jde-ant-read-buildfile
	;;read the buildfile from the user.

	;;figure out which directory to execute from.
	(let (prompt-directory prompt-filename)

	  (if jde-ant-interactive-buildfile
	      (progn
		(setq prompt-directory
		      (file-name-directory jde-ant-interactive-buildfile))
		(setq prompt-filename
		      (file-name-nondirectory jde-ant-interactive-buildfile)))

	    (setq prompt-directory (jde-ant-get-default-directory))
	    (setq prompt-filename ""))

	  (setq buildfile
		(read-file-name "Buildfile: " prompt-directory nil t
				prompt-filename))))
    (if (or (and jde-ant-enable-find (not jde-ant-read-buildfile)) ;enable only
	    (and jde-ant-enable-find jde-ant-read-buildfile
		 (or (null buildfile)   ;no buildfile
		     (string= "" buildfile)
		     (and (file-exists-p buildfile) ;buildfile is a directory
			  (file-directory-p buildfile)))))
	(progn
	  (setq buildfile (jde-ant-find-build-file
			   (jde-ant-get-default-directory)))

	  (when (null buildfile)
	    (error "Could not find Ant build file"))

	  (when (not (file-exists-p buildfile))
	    (error "File does not exist %s " buildfile))))

    (if (and (not jde-ant-enable-find)
	     (not jde-ant-read-buildfile))
	;;use the default buildfile.
	(setq buildfile (jde-normalize-path jde-ant-buildfile)))
    buildfile))

;;;###autoload
(defun jde-ant-build (buildfile target &optional interactive-args)
  "Build the current project using Ant.  If interactive, we try to prompt the
  user for certain variables.."
  (interactive
   (let* ((buildfile (jde-ant-interactive-get-buildfile))
	  (build-history (jde-ant-get-from-history buildfile))
	  (targets
	   (if jde-ant-read-target
	       (if jde-ant-complete-target
		   (if (fboundp 'completing-read-multiple)
		       (completing-read-multiple
			"Target to build: "
			(jde-ant-get-target-alist buildfile)
			nil
			nil
			(car build-history)
			'build-history)
		     (list (completing-read
			    "Target to build: "
			    (jde-ant-get-target-alist buildfile)
			    nil
			    t
			    (car build-history)
			    'build-history)))
		 (list (read-from-minibuffer
			"Target to build: "
			(car build-history)
			nil
			nil
			'build-history)))))
	  (target
	   (jde-ant-escape (mapconcat 'identity targets " ")))
	  (interactive-args
	   (if jde-ant-read-args
	       (read-from-minibuffer
		"Additional build args: "
		(nth 0 jde-ant-interactive-args-history)
		nil nil
		'(jde-ant-interactive-args-history . 1)))))


     ;; Setting the history for future use
     (jde-ant-add-to-history buildfile build-history)


     ;;some of these global variables are defaults.  We should restore then for
     ;;every request.  IE jde-ant-interactive-target and
     ;;jde-ant-interactive-args

     (setq jde-ant-interactive-buildfile buildfile)

     ;;return our new arguments.
     ;;This should be a list of buildfile, target and optional-args.
     (list buildfile target interactive-args)))

  (let ((compile-command
	 (jde-build-ant-command target interactive-args buildfile))
	process-connection-type)

    (when compile-command
      ;; Force save-some-buffers to use the minibuffer
      ;; to query user about whether to save modified buffers.
      ;; Otherwise, when user invokes the command from
      ;; menu, save-some-buffers tries to popup a menu
      ;; which seems not to be supported--at least on
      ;; the PC.
      (if (and (eq system-type 'windows-nt)
	       (not jde-xemacsp))
	  (let ((temp last-nonmenu-event))
	    ;; The next line makes emacs think that the command
	    ;; was invoked from the minibuffer, even when it
	    ;; is actually invoked from the menu-bar.
	    (setq last-nonmenu-event t)
	    (save-some-buffers (not compilation-ask-about-save) nil)
	    (setq last-nonmenu-event temp))
	(save-some-buffers (not compilation-ask-about-save) nil))

      (setq compilation-finish-functions
	    (lambda (buf msg)
	      (run-hook-with-args 'jde-ant-build-hook buf msg)
	      (setq compilation-finish-functions nil)))

      (if (string= (car jde-ant-invocation-method) "Ant Server")
	  (progn
	    (while (string-match "\"" compile-command)
	      (setq compile-command (replace-match "" nil nil
						   compile-command)))
	    (jde-ant-compile-internal compile-command
				      "No more errors"))
	(let ((default-directory (jde-ant-get-default-directory)))
	  (compilation-start compile-command))))))

(defvar jde-ant-comint-filter nil)

(defun jde-ant-escape (target)
  "Looks for \ characters and escape them, i.e. \\"
  (if (not (null target))
      (let (temp c)
	(while (not (string= target ""))
	  (setq c (substring target 0 1))
	  (if (string= c "\\")
	      (setq temp (concat temp c)))
	  (setq temp (concat temp c))
	  (setq target (substring target 1)))
	temp)))

(defun jde-ant-compile-internal (command error-message)
  "This method displays ant output in a compilation buffer.
error-message is a string to print if the user asks to see another error
and there are no more errors. "
  (let* (error-regexp-alist
	 enter-regexp-alist
	 leave-regexp-alist
	 file-regexp-alist
	 nomessage-regexp-alist
	 (parser compilation-parse-errors-function)
	  outbuf)

    (save-excursion				       ;;getting or creating
      (setq outbuf (get-buffer-create "*compilation*"));;the compilation buffer
      (set-buffer outbuf) ;;setting the compilation buffer

      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables))
	(setq error-regexp-alist compilation-error-regexp-alist)
	(setq enter-regexp-alist
	      (if (boundp 'compilation-enter-directory-regexp-alist)
		  compilation-enter-directory-regexp-alist))
	(setq leave-regexp-alist
	      (if (boundp 'compilation-leave-directory-regexp-alist)
		  compilation-leave-directory-regexp-alist))
	(setq file-regexp-alist
	      (if (boundp 'compilation-file-regexp-alist)
		  compilation-file-regexp-alist))
	(setq nomessage-regexp-alist
	      (if (boundp 'compilation-nomessage-regexp-alist)
		  compilation-nomessage-regexp-alist))

	(let* (proc (thisdir (jde-ant-get-default-directory)) outwin)
	  (save-excursion
	    ;; Clear out the compilation buffer and make it writable.
	    (if (not (jde-bsh-running-p))
		(progn
		  (bsh-launch (oref 'jde-bsh the-bsh))
		  (bsh-eval (oref 'jde-bsh the-bsh) (jde-create-prj-values-str))))
	    (setq proc (bsh-get-process (oref 'jde-bsh the-bsh)))
	    (set-buffer outbuf)
	    (compilation-mode)
	    (setq buffer-read-only nil)
	    (buffer-disable-undo (current-buffer))
	    (erase-buffer)
	    (buffer-enable-undo (current-buffer))
	    (display-buffer outbuf)
	    (insert "AntServer output:\n")
	    (insert command "\n")
	    (set-buffer-modified-p nil)
	    (setq jde-ant-comint-filter (process-filter proc))
	    (set-process-filter proc 'jde-ant-filter)
	    ;;resets the jde-ant-passed-security-exception flag
	    (setq jde-ant-passed-security-exception nil)
	    (process-send-string proc (concat "jde.util.AntServer.start(\""
					      command "\");" "\n")))
	  (setq outwin (display-buffer outbuf))
	  (save-excursion
	    ;; (setq buffer-read-only t)  ;;; Non-ergonomic.
	    (set (make-local-variable 'compilation-parse-errors-function)
		 parser)
	    (if (boundp 'compilation-error-message)
		(set (make-local-variable 'compilation-error-message)
		     error-message))
	    (set (make-local-variable 'compilation-error-regexp-alist)
		 error-regexp-alist)

	    (when (not (featurep 'xemacs))
	      (dolist (elt `((compilation-enter-directory-regexp-alist
			      ,enter-regexp-alist)
			     (compilation-leave-directory-regexp-alist
			      ,leave-regexp-alist)
			     (compilation-file-regexp-alist
			      ,file-regexp-alist)
			     (compilation-nomessage-regexp-alist
			      ,nomessage-regexp-alist)))
		(if (boundp (car elt))
		    (set (make-local-variable (car elt)) (second elt)))))

	    (if (boundp 'compilation-directory-stack)
		(setq default-directory thisdir
		      compilation-directory-stack (list default-directory)))
	    (compilation-set-window-height outwin)

	    (if (not jde-xemacsp)
		(if compilation-process-setup-function
		    (funcall compilation-process-setup-function)))))
	;; Make it so the next C-x ` will use this buffer.
	(setq compilation-last-buffer outbuf)))

(defun jde-ant-filter (proc string)
  "This filter prints out the result of the process without buffering.
The result is inserted as it comes in the compilation buffer."
  (let ((compilation-buffer (get-buffer "*compilation*")))
    (if (not (null compilation-buffer))
	(with-current-buffer compilation-buffer
	  (let ((stack-trace
		 (string-match "java.lang.SecurityException" string))
		(end-of-result (string-match ".*bsh % " string))
		(win (get-buffer-window "*compilation*")))

	    (save-excursion
	      ;;Insert the text, advancing the process marker
	      (goto-char (point-max))

	      ;;if the security exception has been thrown set the
	      ;;jde-ant-passed-security-exception flag and filter the stack
	      ;;trace out of the ouput
	      (if stack-trace
		  (progn
		    (setq jde-ant-passed-security-exception t)
		    (insert (substring string 0 stack-trace))
		    (set-buffer-modified-p nil)
		    (compilation-mode)
		    (jde-ant-set-build-status (buffer-string))
		    (jde-ant-handle-exit)))

	      (if end-of-result
		  (progn
		    (if (not jde-ant-passed-security-exception)
			(progn
			  (insert (substring string 0 end-of-result))
			  (set-buffer-modified-p nil)
			  (compilation-mode)
			  (jde-ant-set-build-status (buffer-string))
			  (jde-ant-handle-exit)))
		    (set-process-filter proc jde-ant-comint-filter)))
	      (if (and (not end-of-result)
		       (not jde-ant-passed-security-exception))
		  (insert string)))
	    (if (not jde-xemacsp)
		(if compilation-scroll-output
		    (save-selected-window
		      (if win
			  (progn
			    (select-window win)
			    (goto-char (point-max))))))))))))

(defun jde-ant-handle-exit ()
  "Handles the compilation exit"
  (compilation-handle-exit
   'exit jde-ant-build-status
   (if (string= "0" jde-ant-build-status)
       "finished\n"
     "exited abnormally with code 1\n")))

(defun jde-ant-set-build-status (buffer-contents)
  "Sets the build status based on the BUFFER-CONTENTS"
  (if (string-match ".*BUILD SUCCESSFUL.*" buffer-contents)
      (setq jde-ant-build-status "0"))
  (if (string-match ".*BUILD FAILED.*" buffer-contents)
      (setq jde-ant-build-status "1")))

;;;###autoload
(defun jde-ant-projecthelp(buildfile)
  "Display Ant project help for the current project.
This will execute the Ant program with the `-projecthelp' switch to output
available targets with their descriptions for the current buildfile. This
function uses the same rules as `jde-ant-build' for finding the buildfile."
  (interactive
   (list
    (jde-ant-interactive-get-buildfile)))

  (jde-ant-build buildfile nil "-projecthelp"))

(defun jde-ant-find-build-file (dir)
  "Find the next Ant build file upwards in the directory tree from DIR.
Returns nil if it cannot find a project file in DIR or an ascendant directory."
  (let ((file (find (cond ((string= jde-ant-buildfile "") "build.xml")
			  (t jde-ant-buildfile))
		    (directory-files dir) :test 'string=)))

    (if file
	(setq file (expand-file-name file dir))
      (if (not (jde-root-dir-p dir))
	  (setq file (jde-ant-find-build-file (concat dir "../")))))

    file))

(defun jde-ant-get-target-alist (buildfile)
  "Returns asociation list of valid Ant project targets."

  (let ((targets nil )
	(temp-buf (get-buffer-create "*jde-ant-get-target-list-temp-buffer*")))
    (unwind-protect
	(save-excursion
	  (set-buffer temp-buf)
	  (erase-buffer)
	  (insert-file-contents buildfile)
	  (goto-char (point-min))
	  (while (re-search-forward jde-ant-target-regexp (point-max) t)
	    (setq targets (append targets (list (list (match-string 1)))))))
      (kill-buffer temp-buf))

    targets))

;;;###autoload
(defun jde-ant-show-options ()
  "Show the JDE Ant Options panel."
  (interactive)
  (customize-apropos "jde-ant" 'groups))

(defun jde-ant-get-default-directory ()
  "Gets the default-directory according to the value of
'jde-ant-working-directory."
  (if (string= jde-ant-working-directory "")
      default-directory
    jde-ant-working-directory))

(defun jde-ant-add-to-history (buildfile build-history)
  (let ((temp (nth 0 jde-ant-interactive-target-history))
	(index -1) (found nil))
    (while (and temp (not found))
      (setq index (1+ index))
      (setq temp (nth index jde-ant-interactive-target-history))
      (if (string= (car temp) buildfile)
	  (setq found t)))
    (if found
	(setcdr temp build-history)
      (setq jde-ant-interactive-target-history
	    (append
	     jde-ant-interactive-target-history
	     (list (list buildfile (car build-history))))))))

(defun jde-ant-get-from-history (buildfile)
  (let ((temp (nth 0 jde-ant-interactive-target-history))
	(index -1) (found nil))
    (while (and temp (not found))
      (setq index (1+ index))
      (setq temp (nth index jde-ant-interactive-target-history))
      (if (string= (car temp) buildfile)
	  (setq found t)))
    (if found
	(cdr temp)
      nil)))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-ant)

;; End of jde-ant.el
