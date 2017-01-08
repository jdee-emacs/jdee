;; jdee-run.el --- runs the Java app in the current buffer.

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: tools, processes

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2008 Paul Kinnucan
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
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'jdee-classpath)
(require 'jdee-files)
(require 'jdee-jdk-manager)
(require 'jdee-open-source);; jdee-find-class-source-file
(require 'jdee-parse)

(defcustom jdee-run-mode-hook nil
  "*List of hook functions run by `jdee-run-mode' (see `run-hooks')."
  :group 'jdee-project
  :type 'hook)

(defcustom jdee-run-application-class ""
  "*Name of the Java class to run.
This is the class that is run if you select JDE->Run App from the JDE
menu or type C-c C-v C-r. If this option is the empty string, the JDE
runs the class corresponding to the source file in the current
buffer. Note that the specified class must have a static public main
method."
  :group 'jdee-project
  :type 'string)

(defcustom jdee-run-working-directory ""
  "*Path of the working directory for this application.
If you specify a path, the JDE launches the application from the
directory specified by the path."
  :group 'jdee-project
  :type 'file)


(defcustom jdee-vm-path ""
  "*Path of the Java virtual machine executable. The path
can include environment variables, e.g.,
$JDK_HOME/bin/java. If the value of this variable is the
empty string, the JDE uses the vm that comes with the
version of the JDK specified by jdee-jdk or, if jdee-jdk
is not set, the vm on the system command path."
  :group 'jdee-project
  :type 'file)


(defcustom jdee-run-classic-mode-vm nil
"Runs applications in the classic (i.e., not HotSpot) mode."
  :group 'jdee-project
  :type 'boolean)


(defcustom jdee-run-read-vm-args nil
"*Read vm arguments from the minibuffer.
If this variable is non-nil, the jdee-run command reads vm arguments
from the minibuffer and appends them to those specified by
the `jdee-run-option' variable group."
  :group 'jdee-project
  :type 'boolean)

(defvar jdee-run-interactive-vm-arg-history nil
"History of vm arguments read from the minibuffer")

(defcustom jdee-run-read-app-args nil
"*Read arguments to be passed to application from the minibuffer."
  :group 'jdee-project
  :type 'boolean)

(defvar jdee-run-interactive-app-arg-history nil
"History of application arguments read from the minibuffer")

(defgroup jdee-run-options nil
  "JDE Interpreter Options"
  :group 'jdee
  :prefix "jdee-run-option-")

;; (makunbound 'jdee-run-option-classpath)
(defcustom jdee-run-option-classpath "global"
"*Specify paths of classes required to run this application.
Choose Global from the customization buffer value menu to use
the paths specified by `jdee-global-classpath'.
Choose Local from the menu to override the
`jdee-global-classpath' option. Choose None to specify
no classpath."
  :group 'jdee-run-options
  :type '(choice
	  (const :menu-tag "Global" "global")
	  (repeat :menu-tag "Local" (file :tag "Path"))
	  (const :menu-tag "None" "none")))

(defcustom jdee-run-option-verbose (list nil nil nil)
  "*Print messages about the running process.
The messages are printed in the run buffer."
  :group 'jdee-run-options
  :type '(list :indent 2
	       (checkbox :format "\n  %[%v%] %h \n"
			 :doc "Print classes loaded.
Prints a message in the run buffer each time a class is loaded.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print memory freed.
Prints a message in the run buffer each time the garbage collector
frees memory.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print JNI info.
Prints JNI-related messages including information about which native
methods have been linked and warnings about excessive creation of
local references.")))

;;(makunbound 'jdee-run-option-properties)
(defcustom jdee-run-option-properties nil
  "*Specify property values.
Enter the name of the property, for example, awt.button.color, in the
Property Name field; enter its value, for example, green, in the
Property Value field. You can specify as many properties as you like."
  :group 'jdee-run-options
  :type '(repeat (cons :tag "Property"
		  (string :tag "Name")
		  (string :tag "Value"))))

(defcustom jdee-run-option-heap-size (list
				     (cons 1 "megabytes")
				     (cons 16 "megabytes"))
"*Specify the initial and maximum size of the interpreter heap."
:group 'jdee-run-options
:type '(list
	(cons (integer :tag "Start")
	     (radio-button-choice (const "bytes")
				  (const "kilobytes")
				  (const "megabytes")
				  (const "gigabytes")))
	(cons (integer :tag "Max")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")
				    (const "gigabytes")))))

(defcustom jdee-run-option-stack-size (list
				      (cons 128 "kilobytes")
				      (cons 400 "kilobytes"))
  "*Specify size of the C and Java stacks."
  :group 'jdee-run-options
  :type '(list
	  (cons (integer :tag "C Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")
				    (const "gigabytes")))
	  (cons (integer :tag "Java Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")
				    (const "gigabytes")))))

(defcustom jdee-run-option-garbage-collection (list t t)
  "*Specify garbage collection options."
  :group 'jdee-run-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect garbage asynchronously.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect unused classes.")))

(defcustom jdee-run-option-java-profile (cons nil "./java.prof")
  "*Enable Java profiling."
  :group 'jdee-run-options
  :type '(cons boolean
	       (file :tag "File"
		     :help-echo
"Specify where to put profile results here.")))

(defcustom jdee-run-option-heap-profile (cons nil
						   (list "./java.hprof"
							 5
							 20
							 "Allocation objects"))
"*Output heap profiling data."
  :group 'jdee-run-options
  :type '(cons boolean
	       (list
		(string :tag "Output File Path")
		(integer :tag "Stack Trace Depth")
		(integer :tag "Allocation Sites")
		(radio-button-choice :format "%t \n%v"
				     :tag "Sort output based on:"
		 (const "Allocation objects")
		 (const "Live objects")))))

;; (makunbound 'jdee-run-option-verify)
(defcustom jdee-run-option-verify (list nil t)
  "*Verify classes."
  :group 'jdee-run-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Executed code in all classes.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Classes loaded by a classloader.")))

;; (makunbound 'jdee-run-option-boot-classpath)
(defcustom jdee-run-option-boot-classpath nil
  "Specify a list of directories, JAR archives, and ZIP archives to
search for boot class files.

To specify the standard boot classpath, select \"standard\" from the
Value Menu. To specify a custom boot classpath, select \"custom\" from
the Value Menu. Emacs displays a \"Custom Mode\" Value Menu and an
\"INS\" button for creating a list of directories and/or JAR and ZIP
archives. To append the custom list to the standard list, select
\"append\" from the Value Menu.  To prepend the custom list to the
standard list, select \"prepend\" from the Value Menu. To replace the
standard list with the custom list, select \"replace\" from the Value
Menu.

Note that if `jdee-jdk' specifies the 1.2.x version of the JDK, the
JDEE replaces the standard list with the custom list regardless of the
setting of the \"Custom Mode\" Value Menu. The JDEE ignores this
option if `jdee-jdk' specifies the 1.1.x version of the JDK.

Note also that the value of this variable is either nil or a cons
whose cdr is a list of strings, each of which specifies a path. The
JDE converts this list to a colon- or semicolon-separated list before
inserting it in the compiler or vm command line. The paths may start
with a tilde (~) and may include environment variables. The JDE
replaces the ~ with your home directory and replaces each instance of
an environment variable with its value before inserting it into a
command line. The paths may also start with a . (period) to indicate
that the path is a relative path. If `jdee-resolve-relative-paths-p' is
nonnil, the JDEE treats the paths as relative to the location of the
project file for the current project and replaces the period (.) with
the path of the directory containing the project file."
  :group 'jdee-run-options
  :type '(choice
	  :tag "Classpath Options"
	  (const :tag "standard" nil)
	  (cons :tag "custom" :inline nil
		(choice
		 :tag "Custom Mode"
		 (const "append")
		 (const "prepend")
		 (const "replace"))
		(repeat
		 :tag "Classpath"
		 (file :tag "Path")))))


;; (makunbound 'jdee-run-option-debug)
(defcustom jdee-run-option-debug nil
  "*If \"Connect\" is selected, this option allows the application to
  start and then connect to a debugger, for example, jdb or JDEbug.

\"Mode\" specifies the method for establishing the connection. The
options are \"Server\" (allow a debugger to attach to the application
after the application starts) or \"Client\" (allows the application to connect
to a listening debugger).

\"Data Transport\" specifies the method that the debuggee process uses to
communicate with the debugger. The \"Shared Memory\" option is valid
only when the the debuggee process and the debugger are both running
on the same Microsoft Windows system. (It is also the best choice for
such debugging sessions.) You must choose \"Socket\" if the debugger
and the debuggee process run on different systems or if they both
run on the same Unix system.

The \"Shared Memory Name\" option specifies the name of the shared
memory connection used to connect the debugger and the application.
This option applies only when you specify a shared memory connection
to the debugger.

The \"Socket Host\" specifes the host on which a remote debugger
resides. This option applies only when you run the process in
client mode, using a socket transport.

The \"Socket Port\" option specifies the socket port used to
connect this process to the debugger. This option applies only
when you select socket as the transport method.

The \"Suspend\" option specifies whether the vm should suspend
this process on startup."
  :group 'jdee-run-options
  :type  '(choice
	   :tag "Debug Connection Options"
	   (const :tag "No connect" nil)
	   (list
	    :tag "Connect"
	    :inline nil
	    (choice
	     :tag "Mode"
	     (const "Server")
	     (const "Client"))
	    (choice
	     :tag "Data Transport"
	     (const "Shared Memory")
	     (const "Socket"))
	    (choice
	     :tag "Shared Memory Name"
	     (const :menu-tag "Default" "javadebug")
	     (string :menu-tag "Custom" :tag "Name"))
	    (choice
	     :tag "Socket Host"
	     (const :menu-tag "Local" nil)
	     (string :menu-tag "Remote" :tag "Name"))
	    (choice
	     :tag "Socket Port"
	     (const :menu-tag "Default" "4444")
	     (string :menu-tag "Custom" :tag "Address"))
	    (choice
	     :tag "Suspend?"
	     (const :tag "No" nil)
	     (const :tag "Yes" t)))))

(defcustom jdee-run-option-interpret-mode nil
  "Causes the vm to interpret all byte codes. By default VMs
predating JDK 1.3 use a JIT compiler to execute bytecodes. A JIT
compiler translates the class bytecodes into native machine code when
a class is loaded. Beginning with JDK 1.3, VMs use a Hotspot
compiler. A Hotspot compiler compiles sections of code that execute
frequently."
   :group 'jdee-run-options
   :type 'boolean)

(defcustom jdee-run-option-jar nil
  "Execute a program encapsulated in a JAR file.
Set `jdee-run-application-class' to the name of a JAR file instead of a
startup class name. In order for this option to work, the manifest of
the JAR file must contain a line of the form Main-Class:
classname. Here, classname identifies the class having the public
static void main(String[] args) method that serves as your
application's starting point. See the Jar tool reference page and the
Jar trail of the Java Tutorial for information about working with Jar
files and Jar-file manifests.  When you use this option, the JAR file
is the source of all user classes, and other user class path settings
are ignored."
  :group 'jdee-run-options
  :type 'boolean)


(defcustom jdee-run-option-hotspot-type 'client
  "Specify whether to use the Hotspot client or server vm."
  :group 'jdee-run-options
  :type  '(choice :tag "vm type"
		  (const :tag "client"    client)
		  (const :tag "server"    server)))


;;(makunbound 'jdee-run-option-enable-assertions)
(defcustom jdee-run-option-enable-assertions "Nowhere"
  "Enable assertions for the current project.

To disable assertions everywhere (the default), select \"Nowhere\"
from the Value Menu. To enable assertions everywhere except in system
classes, select \"Everywhere\" from the Value Menu. To enable
assertions in specific locations, select \"Somewhere\" from the Value
Menu. The customization buffer expands, allowing you to enable
assertions in the package in the current directory (the directory of
the current Java source buffer), and in specific packages and classes.

To enable assertions everwhere except in specified locations, set this
option to \"Everywhere\" and use `jdee-run-option-disable-assertions'
to specify the exceptions. To enable assertions in system classes, set
`jdee-run-option-enable-system-assertions' on.

The JDEE  ignores this option if `jdee-jdk' specifies a version
of the JDK that precedes version 1.4, which
introduced assertions into Java."
  :group 'jdee-run-options
  :type '(choice :tag "Enable assertions"
	      (const "Nowhere")
	      (const "Everywhere")
	      (cons :tag "Somewhere" :inline "Everywhere"
		    (boolean :tag "In current directory")
		    (repeat :tag "In the following locations"
			    (cons :tag "Location"
				  (choice :tag "Type"
					  (const "package")
					  (const "class"))
				  (string :tag "Name"))))))

;;(makunbound 'jdee-run-option-disable-assertions)
(defcustom jdee-run-option-disable-assertions "Nowhere"
  "Enable assertions.

Use this option to specify exceptions to packages and classes enabled
by `jdee-run-option-enable-assertions'. To not disable assertions
anywhere (the default), select \"Nowhere\" from the Value Menu. To
disable assertions everywhere except in system classes, select
\"Everywhere except system\" from the Value Menu or set
`jdee-run-option-enable-system-assertions' on.  To disable assertions
in specific locations, select \"Somewhere\" from the Value Menu. The
customization buffer expands, allowing you to enable assertions in the
package in the current directory (the directory of the current Java
source buffer), and in specific packages and classes.

The JDEE  ignores this option if `jdee-jdk' specifies a version
of the JDK that precedes version 1.4, which
introduced assertions into Java."
  :group 'jdee-run-options
  :type '(choice :tag "Disable assertions"
	      (const "Nowhere")
	      (const "Everywhere")
	      (cons :tag "Somewhere" :inline "Everywhere"
		    (boolean :tag "In current directory")
		    (repeat :tag "In the following locations"
			    (cons :tag "Location"
				  (choice :tag "Type"
					  (const "package")
					  (const "class"))
				  (string :tag "Name"))))))

(defcustom jdee-run-option-enable-system-assertions nil
  "Enable assertions in system classes."
  :group 'jdee-run-options
  :type 'boolean)

(defcustom jdee-run-option-disable-system-assertions nil
  "Disable assertions in system classes."
  :group 'jdee-run-options
  :type 'boolean)

(defcustom jdee-run-option-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java interpreter. It is an alternative to using JDE Run Option
variables, such as `jdee-run-option-stack-size', to specify Java
interpreter options. Also, it makes it possible to use the JDE with
interpreters that accept command line arguments not supported by
the JDE Run Option variable set."
  :group 'jdee-run-options
  :type '(repeat (string :tag "Argument")))

(defcustom jdee-run-option-application-args nil
  "*Specify command-line arguments to pass to the application.
The JDE passes the specified arguments to the application on
the command line."
  :group 'jdee-run-options
  :type '(repeat (string :tag "Argument")))

(defcustom jdee-run-applet-viewer ""
  "*Specify name of viewer to use to display page containing the applet."
  :group 'jdee-project
  :type 'file)

(defcustom jdee-run-applet-doc ""
  "*Specify name of document containing applet to be viewed.
If no document is specified, JDE assumes that the document name is
APPLET.html, where APPLET is the name of the applet to be viewed."
  :group 'jdee-project
  :type 'file)


(defcustom jdee-appletviewer-option-encoding ""
"*Specify encoding of the HTML file displayed by the appletviewer."
  :group 'jdee-run-options
  :type 'string)


(defcustom jdee-appletviewer-option-vm-args nil
  "*Specify arguments (e.g., -Xmx16m) to the vm that runs appletviewer.
This option allows you to set the environment of the
virtual machine that runs appletviewer."
  :group 'jdee-run-options
  :type '(repeat (string :tag "Argument")))


(defcustom jdee-run-executable ""
  "*Specifies the executable to be run by the JDE's run command.
If you do not specify an executable, the JDE runs the vm specified
by `jdee-run-get-vm'."
  :group 'jdee-project
  :type 'file)

(defcustom jdee-run-executable-args nil
  "*Specify arguments to be passed to the application executable.
This option allows you to specify one or more arguments to be passed
to the executable specified by `jdee-run-executable'."
  :group 'jdee-run-options
  :type '(repeat (string :tag "Argument")))


(defmacro save-w32-show-window (&rest body)
  "Saves the value of the w32-start-process-show-window variable
before evaluating body and restores the value afterwards."
  `(let ((win32-start-process-show-window t)
	 (w32-start-process-show-window t)
	 (w32-quote-process-args ?\")
	 (win32-quote-process-args ?\") ;; XEmacs
	 (windowed-process-io t)
	 (process-connection-type nil))
     ,@body))

(defun jdee-run-parse-args (s)
 "Converts a string of command-line arguments to a list of arguments.
Any substring that is enclosed in single or double quotes or does not include
whitespace is considered a parameter."
   (let ((n (string-match "[^\"' ][^ ]*\\|\"[^\"]*\"\\|'[^']*'" s))
	(tok)
	(tokens '()))
     (while n
       (setq n (match-end 0))
       (setq tok (match-string 0 s))
       (if (string-match "[\"']\\([^\"']*\\)[\"']" tok)
	   (setq tok (match-string 1 tok)))
       (setq tokens (append tokens (list tok)))
       (setq n (string-match "[^\"' ][^ ]*\\|\"[^\"]*\"\\|'[^']*'" s n)))
     tokens))

(defun jdee-run-make-arg-string (args)
"Converts a list of command-line arguments to a string of arguments."
  (let ((str "")
	(n (length args))
	(i 0))
    (while (< i n)
      (if (not (string= str ""))
	  (setq str (concat str " ")))
      (setq str (concat str (nth i args)))
      (setq i (+ i 1)))
    str))

;;;###autoload
(defun jdee-run-set-app (app)
  "Specify the name of the application class to run."
  (interactive
   "sEnter application class: ")
  (setq jdee-run-application-class app))

;;;###autoload
(defun jdee-run-set-args (args)
  "Specify arguments to be passed to the Java vm.
This command serves as an alternative to using the JDE Run Options
panel to specify command-line arguments for the Java interpreter."
  (interactive
   "sEnter arguments: ")
  (setq jdee-run-option-vm-args (jdee-run-parse-args args)))


;;;###autoload
(defun jdee-run-set-app-args (args)
  "Specify the arguments to be passed to the Java application class.
This command provides an alternative to using the JDE Run Options panel
to specify command-line arguments to pass to the application when starting
the application."
  (interactive
   "sEnter arguments: ")
  (setq jdee-run-option-application-args (jdee-run-parse-args args)))

;;;###autoload
(defun jdee-run-set-applet-viewer (viewer)
  "Sets the viewer to be used to view an applet. The default is
appletviewer."
  (interactive
   "sEnter viewer name: ")
  (setq jdee-run-applet-viewer viewer))

;;;###autoload
(defun jdee-run-set-applet-doc (doc)
  "Specify the doc to be used to view an applet.
This command provides an alternative to using the JDE Options
panel to specifying the applet document."
  (interactive
   "sEnter applet doc name: ")
  (if (string= doc "")
      (setq jdee-run-applet-doc nil)
    (setq jdee-run-applet-doc doc)))


(defclass jdee-run-vm ()
  ((version          :initarg :version
		     :type string
		     :initform ""
		     :documentation
		     "Java virtual machine version number.")
   (path             :initarg :path
		     :type string
		     :documentation
		     "Path of the compiler executable.")
   (buffer           :initarg :buffer
		     :type buffer
		     :documentation
		     "Compilation buffer")
   (main-class        :initarg :main-class
		     :type string
		     :documentation
		     "Name of main class."))
    "Class of Java virtual machines.")

(defmethod jdee-run-classpath-arg ((this jdee-run-vm))
  "Returns the classpath argument for this vm."
  (let ((classpath
	 (if jdee-run-option-classpath
	     (if (and (stringp jdee-run-option-classpath)
		      (string= jdee-run-option-classpath "global"))
		 jdee-global-classpath
	       (unless (and (stringp jdee-run-option-classpath)
			    (string= jdee-run-option-classpath "none"))
		 jdee-run-option-classpath))))
	(symbol
	 (if (and jdee-run-option-classpath
		  (stringp jdee-run-option-classpath)
		  (string= jdee-run-option-classpath "global"))
	     'jdee-global-classpath
	   'jdee-run-option-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jdee-build-classpath
	  classpath symbol)))))

(defmethod jdee-run-classic-mode-arg ((this jdee-run-vm))
  "Get classic-mode option>"
    (if jdee-run-classic-mode-vm
	(list "-classic")))


(defmethod jdee-run-property-args ((this jdee-run-vm))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jdee-run-option-properties))

(defmethod jdee-run-heap-size-args ((this jdee-run-vm))
   "Get heap size arguments."
    (let* ((memory-unit-abbrevs
	    (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")
	       (cons "gigabytes" "g")))
	   (start-cons (nth 0 jdee-run-option-heap-size))
	   (start-size (format "%d%s" (car start-cons)
			       (cdr (assoc (cdr start-cons)
				      memory-unit-abbrevs))))
	   (max-cons (nth 1 jdee-run-option-heap-size))
	   (max-size (format "%d%s" (car max-cons)
			     (cdr (assoc (cdr max-cons)
				    memory-unit-abbrevs)))))
      (append
       (if (not (string= start-size "1m"))
	   (list (concat "-Xms" start-size)))
       (if (not (string= max-size "16m"))
	  (list (concat "-Xmx" max-size))))))

(defmethod jdee-run-stack-size-args ((this jdee-run-vm))
  "Get stack size arguments."
    (let* ((memory-unit-abbrevs
	    (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")
	       (cons "gigabytes" "g")))
	   (c-cons (nth 0 jdee-run-option-stack-size))
	   (c-size (format "%d%s" (car c-cons)
			   (cdr (assoc (cdr c-cons)
				       memory-unit-abbrevs))))
	   (java-cons (nth 1 jdee-run-option-stack-size))
	   (java-size (format "%d%s" (car java-cons)
			     (cdr (assoc (cdr java-cons)
				    memory-unit-abbrevs)))))
      (append
       (if (not (string= c-size "128k"))
	   (list (concat "-Xss" c-size)))
       (if (not (string= java-size "400k"))
	   (list (concat "-Xoss" java-size))))))


(defmethod jdee-run-java-profile-arg ((this jdee-run-vm))
   "Get Java profile option."
    (let ((profilep (car jdee-run-option-java-profile))
	  (file (cdr jdee-run-option-java-profile)))
      (if profilep
	  (if (string= file "./java.prof")
	      '("-Xprof")
	    (list (concat "-Xprof:" file))))))

(defmethod jdee-run-heap-profile-arg ((this jdee-run-vm))
  "Get heap profile argument."
    (let* ((profilep (car jdee-run-option-heap-profile))
	   (prof-options (cdr jdee-run-option-heap-profile))
	   (file (nth 0 prof-options))
	   (depth (nth 1 prof-options))
	   (top (nth 2 prof-options))
	   (sort
	    (downcase (substring (nth 3 prof-options) 0 1))))
      (if profilep
	  (if (and (string= file "./java.hprof")
		   (equal depth 5)
		   (equal top 20)
		   (string= sort "a"))
	      '("-Xhprof")
	    (list
	     (format
	      "-Xhprof:file=%s,depth=%d,top=%d,sort=%s"
	      file depth top sort))))))


(defmethod jdee-run-vm-args ((this jdee-run-vm))
  "Get command line args."
  jdee-run-option-vm-args)


(defmethod jdee-run-vm-launch ((this jdee-run-vm))
  (let ((run-buf-name (concat "*" (oref this :main-class) "*"))
	(source-directory default-directory)
	(working-directory (if (string= jdee-run-working-directory "")
			       default-directory
			     (jdee-normalize-path 'jdee-run-working-directory))))
    (if (not (comint-check-proc run-buf-name))
	(let* ((run-buffer (get-buffer-create run-buf-name))
	       (win32-p (eq system-type 'windows-nt))
	       (prog (oref this :path))
	       (prog-args (append
			   (jdee-run-get-vm-args this)
			   (if jdee-run-read-vm-args
			       (jdee-run-parse-args
				(read-from-minibuffer
				 "Vm args: "
				 (car jdee-run-interactive-vm-arg-history)
				 nil nil
				 'jdee-run-interactive-vm-arg-history)))
			   (list (oref this :main-class))
			   jdee-run-option-application-args
			   (if jdee-run-read-app-args
			       (jdee-run-parse-args
				(read-from-minibuffer
				 "Application args: "
				 (car jdee-run-interactive-app-arg-history)
				 nil nil
				 'jdee-run-interactive-app-arg-history)))
			   ))
	       (command-string (concat prog " "
				       (jdee-run-make-arg-string
					prog-args)
				       "\n\n")))
	  (with-current-buffer run-buffer
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (jdee-run-mode))
	  (save-w32-show-window
	    (comint-exec run-buffer (oref this :main-class) prog nil prog-args))
	  (pop-to-buffer run-buffer)
	  (save-excursion
	    (goto-char (point-min))
	  (jdee-run-etrace-update-current-marker))
	  (cd source-directory))
      (message "An instance of %s is running." (oref this :main-class))
      (pop-to-buffer run-buf-name))))


(defclass jdee-run-vm-1-1 (jdee-run-vm) ()
  "Represents the JDK 1.1.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-1) &rest fields)
  "Constructor for the class representing the JDK 1.1 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.1"))

(defmethod jdee-run-verbose-arg ((this jdee-run-vm-1-1))
  "Set the verbose options."
    (let ((print-classes-loaded
	   (nth 0 jdee-run-option-verbose))
	  (print-memory-freed
	   (nth 1 jdee-run-option-verbose))
	  (print-jni-info
	   (nth 2 jdee-run-option-verbose)))
      (append
       (if print-classes-loaded (list "-verbose"))
       (if print-memory-freed (list "-verbosegc"))
       (if print-jni-info (list "-verbose")))))

(defmethod jdee-run-gc-args ((this jdee-run-vm-1-1))
  "Get garbage collection arguments."
    (let ((no-gc-asynch (not
			 (nth 0 jdee-run-option-garbage-collection)))
	  (no-gc-classes (not
			  (nth 1 jdee-run-option-garbage-collection))))
      (append
       (if no-gc-asynch
	  '("-noasyncgc"))
       (if no-gc-classes
	   '("-noclassgc")))))

(defmethod jdee-run-verify-args ((this jdee-run-vm-1-1))
  "Get verify arguments."
    (let ((verify-all (nth 0 jdee-run-option-verify))
	  (verify-remote (nth 1 jdee-run-option-verify)))
      (append
       (if verify-all
	  '("-verify"))
       (if (and
	   (not verify-all)
	   (not verify-remote))
	   '("-noverify")))))

(defmethod jdee-run-debug-args ((this jdee-run-vm-1-1))
  "Get arguments required to allow process to connect
to a debugger."
  (if jdee-run-option-debug
      '("-debug")))

(defmethod jdee-run-interpret-mode-arg ((this jdee-run-vm-1-1))
  "Get argument required to enable interpret mode."
  (if jdee-run-option-interpret-mode
      '("-nojit")))


(defmethod jdee-run-get-vm-args ((this jdee-run-vm-1-1))
  (append
   (jdee-run-classpath-arg this)
   (jdee-run-verbose-arg this)
   (jdee-run-property-args this)
   (jdee-run-heap-size-args this)
   (jdee-run-stack-size-args this)
   (jdee-run-gc-args this)
   (jdee-run-java-profile-arg this)
   (jdee-run-heap-profile-arg this)
   (jdee-run-verify-args this)
   (jdee-run-interpret-mode-arg this)
   (jdee-run-debug-args this)
   (jdee-run-vm-args this)))


(defclass jdee-run-vm-1-2 (jdee-run-vm-1-1) ()
  "Represents the JDK 1.2.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-2) &rest fields)
  "Constructor for the class representing the JDK 1.2 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.2"))

(defmethod jdee-run-boot-classpath-arg ((this jdee-run-vm-1-2))
  "Returns the boot classpath argument for this vm."
  (if jdee-run-option-boot-classpath
	(list
	 (concat
	  "-Xbootclasspath:"
	  (jdee-build-classpath
	   (cdr jdee-run-option-boot-classpath)
	   'jdee-run-option-boot-classpath)))))

(defmethod jdee-run-verbose-arg ((this jdee-run-vm-1-2))
  "Set the verbose options."
    (let ((print-classes-loaded
	   (nth 0 jdee-run-option-verbose))
	  (print-memory-freed
	   (nth 1 jdee-run-option-verbose))
	  (print-jni-info
	   (nth 2 jdee-run-option-verbose)))
      (append
       (if print-classes-loaded (list "-verbose:class"))
       (if print-memory-freed (list "-verbose:gc"))
       (if print-jni-info (list "-verbose:jni")))))

(defmethod jdee-run-gc-args ((this jdee-run-vm-1-2))
  "Get garbage collection arguments."
    (let ((no-gc-asynch (not
			 (nth 0 jdee-run-option-garbage-collection)))
	  (no-gc-classes (not
			  (nth 1 jdee-run-option-garbage-collection))))
      (append
       (if no-gc-asynch
	  '("-Xnoasyncgc"))
       (if no-gc-classes
	   '("-Xnoclassgc")))))

(defmethod jdee-run-debug-args ((this jdee-run-vm-1-2))
  "Get arguments required to allow process to connect
to a debugger."
  (if jdee-run-option-debug
      '("-Xdebug")))

(defmethod jdee-run-interpret-mode-arg ((this jdee-run-vm-1-2))
  "Get argument required to disable use of JIT compiler."
  (if jdee-run-option-interpret-mode
      '("-Djava.compiler=NONE")))

(defmethod jdee-run-jar-arg ((this jdee-run-vm-1-2))
  "Get argument that specifies use of a jar file to run an app."
  (if jdee-run-option-jar
      '("-jar")))


(defmethod jdee-run-get-vm-args ((this jdee-run-vm-1-2))
  (append
   ;; Classic mode argument must come first.
   (jdee-run-classic-mode-arg this)
   (jdee-run-boot-classpath-arg this)
   (jdee-run-classpath-arg this)
   (jdee-run-verbose-arg this)
   (jdee-run-property-args this)
   (jdee-run-heap-size-args this)
   (jdee-run-stack-size-args this)
   (jdee-run-gc-args this)
   (jdee-run-java-profile-arg this)
   (jdee-run-heap-profile-arg this)
   (jdee-run-debug-args this)
   (jdee-run-interpret-mode-arg this)
   (jdee-run-vm-args this)
   (jdee-run-jar-arg this) ;; must be last
   ))

(defclass jdee-run-vm-1-3 (jdee-run-vm-1-2) ()
  "Represents the JDK 1.3.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-3) &rest fields)
  "Constructor for the class representing the JDK 1.3 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.3"))


(defmethod jdee-run-boot-classpath-arg ((this jdee-run-vm-1-3))
  "Returns the boot classpath argument for this vm."
  (if jdee-run-option-boot-classpath
      (list
       (concat
	"-Xbootclasspath"
	(let ((mode (car jdee-run-option-boot-classpath)))
	  (cond
	   ((string= mode "append")
	    "/a:")
	   ((string= mode "prepend")
	    "/p:")
	   ((string= mode "replace")
	    ":")
	   (t
	    (error "Illegal custom classpath mode: %s"  mode))))
	(jdee-build-classpath
	 (cdr jdee-run-option-boot-classpath)
	 'jdee-run-option-boot-classpath)))))


(defmethod jdee-run-debug-args ((this jdee-run-vm-1-3))
  "Get arguments required to allow process to connect
to a debugger."
  (if jdee-run-option-debug
      (let ((mode (nth 0 jdee-run-option-debug))
	    (transport (nth 1 jdee-run-option-debug))
	    (shared-mem-name (nth 2 jdee-run-option-debug))
	    (socket-host (nth 3 jdee-run-option-debug))
	    (socket-port (nth 4 jdee-run-option-debug))
	    (suspend (nth 5 jdee-run-option-debug))
	    (ms-windows (eq system-type 'windows-nt)))
	(list "-Xdebug"
	      (format
	       "-Xrunjdwp:transport=%s,address=%s,server=%s,suspend=%s"
	       (if (string= transport "Shared Memory")
		   (if ms-windows
		       "dt_shmem"
		     (error "Shared memory transport is valid only on Windows."))
		 "dt_socket")
	       (if  (string= transport "Shared Memory")
		   shared-mem-name
		 (if (string= mode "Client")
		     (if socket-host
			 (concat socket-host ":" socket-port)
		       socket-port)
		   socket-port))
	       (if (string= mode "Server") "y" "n")
	       (if suspend "y" "n"))))))

(defmethod jdee-run-get-vm-args ((this jdee-run-vm-1-3))
  (append
   ;; Classic mode argument must come first.
   (jdee-run-classic-mode-arg this)
   (jdee-run-boot-classpath-arg this)
   (jdee-run-classpath-arg this)
   (jdee-run-verbose-arg this)
   (jdee-run-property-args this)
   (jdee-run-heap-size-args this)
   (jdee-run-stack-size-args this)
   (jdee-run-gc-args this)
   (jdee-run-java-profile-arg this)
   (jdee-run-heap-profile-arg this)
   (jdee-run-debug-args this)
   (jdee-run-interpret-mode-arg this)
   (jdee-run-vm-args this)
   (jdee-run-jar-arg this) ;; must be last
   ))


(defclass jdee-run-vm-1-4 (jdee-run-vm-1-3) ()
  "Represents the JDK 1.4.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-4) &rest fields)
  "Constructor for the class representing the JDK 1.4 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.4"))


(defmethod jdee-run-hotspot-vm-type-arg ((this jdee-run-vm-1-4))
  "Get the vm argument that specifes the Hotspot vm type."
  (if (eq jdee-run-option-hotspot-type 'server)
      '("-server")))

(defmethod jdee-run-interpret-mode-arg ((this jdee-run-vm-1-4))
  "Get argument required to enable interpret mode."
  (if jdee-run-option-interpret-mode
      '("-Xint")))

(defmethod jdee-run-enable-assertions-args ((this jdee-run-vm-1-4))
  "Get argument(s) required to enable assertions."
  (let (args)
    (cond
     ((stringp jdee-run-option-enable-assertions)
      (cond
       ((string= jdee-run-option-enable-assertions "Nowhere"))
       ((string= jdee-run-option-enable-assertions "Everywhere")
	(setq args '("-ea")))
       (t
	(error "Illegal enable assertions option: \"%s\"."
	       jdee-run-option-enable-assertions))))
     ((listp jdee-run-option-enable-assertions)
      (if (car jdee-run-option-enable-assertions)
	  (setq args '("-ea:...")))
      (loop for location in (cdr jdee-run-option-enable-assertions) do
	    (let ((type (car location))
		  (name (cdr location)))
	      (if (string= type "package")
		  (setq name (concat name "...")))
	      (setq args  (append args (list (concat "-ea:" name)))))))
     (t
      (error "Illegal enable assertions option: \"%s\"."
		jdee-run-option-enable-assertions)))
    args))


(defmethod jdee-run-disable-assertions-args ((this jdee-run-vm-1-4))
  "Get argument(s) required to enable assertions."
  (let (args)
    (cond
     ((stringp jdee-run-option-disable-assertions)
      (cond
       ((string= jdee-run-option-disable-assertions "Nowhere"))
       ((string= jdee-run-option-disable-assertions "Everywhere")
	(setq args '("-da")))
       (t
	(error "Illegal disable assertions option: \"%s\"."
	       jdee-run-option-disable-assertions))))
     ((listp jdee-run-option-disable-assertions)
      (if (car jdee-run-option-disable-assertions)
	  (setq args '("-da:...")))
      (loop for location in (cdr jdee-run-option-disable-assertions) do
	    (let ((type (car location))
		  (name (cdr location)))
	      (if (string= type "package")
		  (setq name (concat name "...")))
	      (setq args  (append args (list (concat "-da:" name)))))))
     (t
      (error "Illegal disable assertions option: \"%s\"."
		jdee-run-option-disable-assertions)))
    args))


(defmethod jdee-run-enable-system-assertions-arg ((this jdee-run-vm-1-4))
  "Get argument required to enable system assertions."
  (if jdee-run-option-enable-system-assertions
      '("-esa")))

(defmethod jdee-run-disable-system-assertions-arg ((this jdee-run-vm-1-4))
  "Get argument required to disable system assertions."
  (if jdee-run-option-disable-system-assertions
      '("-dsa")))

(defmethod jdee-run-get-vm-args ((this jdee-run-vm-1-4))
  (append
   ;; Classic mode argument must come first.
   (jdee-run-classic-mode-arg this)
   (jdee-run-boot-classpath-arg this)
   (jdee-run-classpath-arg this)
   (jdee-run-verbose-arg this)
   (jdee-run-property-args this)
   (jdee-run-heap-size-args this)
   (jdee-run-stack-size-args this)
   (jdee-run-gc-args this)
   (jdee-run-java-profile-arg this)
   (jdee-run-heap-profile-arg this)
   (jdee-run-debug-args this)
   (jdee-run-interpret-mode-arg this)
   (jdee-run-enable-assertions-args this)
   (jdee-run-disable-assertions-args this)
   (jdee-run-enable-system-assertions-arg this)
   (jdee-run-disable-system-assertions-arg this)
   (jdee-run-vm-args this)
   (jdee-run-jar-arg this) ;; must be last
   ))

(defclass jdee-run-vm-1-5 (jdee-run-vm-1-4) ()
  "Represents the JDK 1.5.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-5) &rest fields)
  "Constructor for the class representing the JDK 1.5 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.5"))

(defclass jdee-run-vm-1-6 (jdee-run-vm-1-5) ()
  "Represents the JDK 1.6.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-6) &rest fields)
  "Constructor for the class representing the JDK 1.6 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.6"))

(defclass jdee-run-vm-1-7 (jdee-run-vm-1-6) ()
  "Represents the JDK 1.7.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-7) &rest fields)
  "Constructor for the class representing the JDK 1.7 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.7"))

(defclass jdee-run-vm-1-8 (jdee-run-vm-1-7) ()
  "Represents the JDK 1.8.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-8) &rest fields)
  "Constructor for the class representing the JDK 1.8 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.8"))

(defclass jdee-run-vm-1-9 (jdee-run-vm-1-8) ()
  "Represents the JDK 1.9.x vm")

(defmethod initialize-instance ((this jdee-run-vm-1-9) &rest fields)
  "Constructor for the class representing the JDK 1.9 vm."

  ;; Call parent initializer.
  (call-next-method)

  (oset this :version "1.9"))



(defvar jdee-run-virtual-machines
  (list
   (jdee-run-vm-1-1 "JDK 1.1 vm")
   (jdee-run-vm-1-2 "JDK 1.2 vm")
   (jdee-run-vm-1-3 "JDK 1.3 vm")
   (jdee-run-vm-1-4 "JDK 1.4 vm")
   (jdee-run-vm-1-5 "JDK 1.5 vm")
   (jdee-run-vm-1-6 "JDK 1.6 vm")
   (jdee-run-vm-1-7 "JDK 1.7 vm")
   (jdee-run-vm-1-8 "JDK 1.8 vm")
   (jdee-run-vm-1-9 "JDK 1.9 vm"))
  "*List of supported virtual machines.")

(defun jdee-run-get-vm ()
  "Gets the vm for the current JDK."
  (let* ((jdk-version (jdee-java-version))
	 (vm
	  (cl-find-if
	   (lambda (vm-x)
	     (string-match
	      (oref vm-x :version)
	      (if jdk-version jdk-version "")))
	   jdee-run-virtual-machines)))
    (if (not vm)
	(setq vm (car jdee-run-virtual-machines)))
    (oset vm
	  :path
	  (let ((vm-path
		 (substitute-in-file-name jdee-vm-path)))
	    (if (string= vm-path "")
		(jdee-get-jdk-prog (if (eq system-type 'windows-nt)
				      'javaw 'java))
	      (if (file-exists-p
		   (if (and
			(eq system-type 'windows-nt)
			(not (string-match "[.]exe$" vm-path)))
		       (concat vm-path ".exe")
		     vm-path))
		  vm-path
		(if (executable-find vm-path)
		    vm-path
		  (error "Invalid vm path: %s"
			 vm-path))))))
    vm))

;;;###autoload
(defun jdee-run (prefix)
  "Run the Java application specified by `jdee-run-executable', if
not the null string. Otherwise run the class specified by
`jdee-run-application-class', if non-null; otherwise the class in the
current buffer. Specifying a prefix argument, e.g., C-u C-c C-v C-r,
causes this command to prompt you to enter arguments to be passed to
the application's main method.  Specifying a minus prefix argument,
i.e., C-u - C-c C-v C-r, causes this command to prompt you to also
enter the name of the application's main class.  Note that you can use
`jdee-run-read-app-args' to cause the command to prompt you for
application arguments by default (i.e., without having to specify a
prefix argument). This command creates a comint buffer to allow you to
interact with the program."
  (interactive "p")
  (if (equal major-mode 'jdee-mode)
      (if (string= jdee-run-executable "")
	  (let ((vm (jdee-run-get-vm))
		(read-app-args
		 (or jdee-run-read-app-args
		     (not (= prefix 1))))
		(read-main-class
                 (= prefix -1)))
	    (oset
	     vm
	     :main-class
	     (if read-main-class
		 (read-from-minibuffer
		  "Main class: "
		  (concat (jdee-parse-get-package)
                          (file-name-sans-extension
                           (file-name-nondirectory (buffer-file-name)))))
	       (jdee-run-get-main-class)))
	    (let ((jdee-run-read-app-args read-app-args))
	      (jdee-run-vm-launch vm)))
	(jdee-run-executable))
    (error "The jdee-run command works only in a Java source buffer.")))

(defun jdee-run-get-main-class ()
  "Gets the main class for the application to which the current
source buffer belongs."
  (let ((main-class
	 (if jdee-run-option-jar
	     (jdee-normalize-path 'jdee-run-application-class)
           jdee-run-application-class)))
    (if (or
	 (not main-class)
	 (string= main-class ""))
	(setq main-class
	      (concat (jdee-parse-get-package)
		      (file-name-sans-extension
		       (file-name-nondirectory (buffer-file-name))))))
    main-class))

(defun jdee-run-main-class()
  "Runs the Java program named by `jdee-run-application-class' in
a buffer, piping output from the program to the buffer and
input from the buffer to the program."
  (interactive)
  (let ((vm (jdee-run-get-vm)))
    (oset vm :main-class (jdee-run-get-main-class))
    (jdee-run-vm-launch vm)))

(defun jdee-run-unquote (string)
  (if (eq (aref string 0) ?\")
      (substring string 1 (- (length string) 1))
    string))

(defun jdee-run-application-running-p ()
  "*Returns t if the application to which the current
buffer belongs is running."
  (let ((run-buf-name (concat "*" (jdee-run-get-main-class) "*")))
    (comint-check-proc run-buf-name)))


(defun jdee-run-executable()
  (let* ((prog-path
	  (jdee-normalize-path
	   jdee-run-executable
	   'jdee-run-executable))
	 (prog-name (file-name-sans-extension
		     (file-name-nondirectory
		      prog-path)))
	 (run-buf-name (concat "*" prog-name "*"))
	 (source-directory default-directory)
	 (working-directory (if (string= jdee-run-working-directory "")
				default-directory
			      (jdee-normalize-path 'jdee-run-working-directory))))
    (if (not (comint-check-proc run-buf-name))
	(let* ((w32-quote-process-args ?\")
	       (win32-quote-process-args ?\") ;; XEmacs
	       (run-buffer (get-buffer-create run-buf-name))
	       (prog-args (append
			   jdee-run-executable-args
			   (if jdee-run-read-app-args
			       (jdee-run-parse-args
				(read-from-minibuffer
				 "Application args: "
				 nil
				 nil nil
				 '(jdee-run-interactive-app-arg-history . 1))))
			   ))
	       (command-string (concat jdee-run-executable " "
				       (mapconcat (lambda (arg) arg)
						  prog-args " ")
				       "\n\n")))
	  (with-current-buffer run-buffer
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (jdee-run-mode))
	  (save-w32-show-window
	   (comint-exec run-buffer prog-name prog-path nil prog-args))
	  (pop-to-buffer run-buffer)
	  (save-excursion
	    (goto-char (point-min))
	    (jdee-run-etrace-update-current-marker))
	  (cd source-directory))
      (message "An instance of %s is running." prog-name)
      (pop-to-buffer run-buf-name))))

(defun jdee-run-mode-internal()
  "Mode for running Java programs."
  (define-key (current-local-map) "\C-c\C-v\C-[" 'jdee-run-etrace-prev)
  (define-key (current-local-map) "\C-c\C-v\C-]" 'jdee-run-etrace-next)
  (define-key (current-local-map) [mouse-2] 'jdee-run-etrace-show-at-mouse)
  (font-lock-mode 1)
  (jdee-run-etrace-setup-font-lock))

(define-derived-mode
  jdee-run-mode comint-mode "JDE Run Mode"
  "Major mode for running Java applications and applets.
  \\{jdee-run-mode-map}"
  (jdee-run-mode-internal))

(defun jdee-get-appletviewer-options ()
  (let (options)
    (if (not (string= jdee-appletviewer-option-encoding ""))
	(setq options (list
			"-encoding"
			jdee-appletviewer-option-encoding)))
    (if jdee-appletviewer-option-vm-args
	(let ((len (length jdee-appletviewer-option-vm-args))
	      (n 0))
	  (while (< n len)
	    (setq options
		  (nconc
		   options
		   (list
		    (concat "-J"
			    (nth n jdee-appletviewer-option-vm-args)))))
	    (setq n (1+ n)))))
    options))

(defun jdee-run-applet-exec (buffer name command startfile switches)
  "A version of comint-exec patched to start an applet viewer as
a command shell subprocess rather than as a subprocess of Emacs. This
is necessary to avoid displaying a DOS window when starting a viewer
under Windows."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc (jdee-run-applet-exec-1 name buffer command switches)))
      (set-process-filter proc 'comint-output-filter)
      (make-local-variable 'comint-ptyp)
      (setq comint-ptyp process-connection-type) ; T if pty, NIL if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
    (run-hooks 'comint-exec-hook)
    buffer)))

;; This auxiliary function cranks up the process for jdee-run-applet-exec in
;; the appropriate environment.

(defun jdee-run-applet-exec-1 (name buffer command switches)
  (let ((w32-quote-process-args ?\")
	(win32-quote-process-args ?\") ;; XEmacs
	(process-environment
	 (nconc
	  ;; If using termcap, we specify `emacs' as the terminal type
	  ;; because that lets us specify a width.
	  ;; If using terminfo, we specify `dumb' because that is
	  ;; a defined terminal type.  `emacs' is not a defined terminal type
	  ;; and there is no way for us to define it here.
	  ;; Some programs that use terminfo get very confused
	  ;; if TERM is not a valid terminal type.
	  (if (and (boundp 'system-uses-terminfo) system-uses-terminfo)
	      (list "TERM=dumb"
		    (format "COLUMNS=%d" (frame-width)))
	    (list "TERM=emacs"
		  (format "TERMCAP=emacs:co#%d:tc=unknown:" (frame-width))))
	  (if (getenv "EMACS") nil (list "EMACS=t"))
	  process-environment))
	(default-directory
	  (if (file-directory-p default-directory)
	      default-directory
	    "/")))
    (apply 'start-process-shell-command name buffer command switches)))

(defun jdee-run-applet-internal (doc)
  (let* ((doc-file-name (file-name-nondirectory doc))
	 (doc-directory (file-name-directory doc))
	 (doc-name (file-name-sans-extension doc-file-name))
	 (run-buf-name (concat "*" doc-name "*")))

    (if (not (comint-check-proc run-buf-name))
	(let* ((run-buffer (get-buffer-create run-buf-name))
	       (win32-p (eq system-type 'windows-nt))
	       (prog jdee-run-applet-viewer)
	       (prog-args
		(append (jdee-get-appletviewer-options)
			(list doc-file-name)))
	       (command-string (concat prog " "
				       (jdee-run-make-arg-string
					prog-args)
				       "\n\n")))
	  (with-current-buffer run-buffer
	    (erase-buffer)
	    (cd doc-directory)
	    (insert (concat "cd " doc-directory "\n"))
	    (insert command-string)
	    (jdee-run-mode))
	  (jdee-run-applet-exec run-buffer doc-name prog nil prog-args)
	  (pop-to-buffer run-buffer))
      (message "An instance of the applet in %s is running." doc-name)
      (pop-to-buffer run-buf-name))))


(defun jdee-run-find-html-files ()
  "If (buffer-file-name) is /a/b/c.xxx (where xxx can be anything),
return (\"/a/b/c.html\") if it exists, else return (\"/a/b/c.htm\")
if it exists, else return a list of all *.html files in /a/b/
directory."
  (let ((basename (file-name-sans-extension (buffer-file-name)))
	f)
    (cond
     ((file-exists-p (setq f (concat basename ".html")))
      (list f))
     ((file-exists-p (setq f (concat basename ".htm"))) ;; for poor winXX souls
      (list f))
     (t
      (cl-mapcan (lambda (file)
		   (if (or
			(string-match "[.]html$" file)
			(string-match "[.]htm$" file))
		       (list file)))
		 (directory-files
		  (file-name-directory (buffer-file-name)) t))))))



(setq jdee-run-applet-last-doc nil)

;;;###autoload
(defun jdee-run-applet (&optional doc)
  "Runs an applet. This function prompts you to enter the path of an
html document that displays the applet. If you enter return without
specifying a document, this function next checks whether
`jdee-run-applet-doc' specifies a document. If so, it displays that
specified document. Next, it checks whether the current directory
contains any html files. If the current directory contains an html
file with the same root name as the Java file in the current buffer,
it displays the file. If not, it displays the first html file that it
finds in the current directory. If if cannot find an html file, it
signals an error.  This function uses the viewer specified by
`jdee-run-applet-viewer' to display the specified document. Note that
if you run two html applet files successively with the same name, you
must kill the buffer created to run the first file before running the
second file. Otherwise, this command will simply redisplay the first
file."
  (interactive
   (let ((insert-default-directory nil))
     (list (read-file-name "Applet doc: " nil nil nil jdee-run-applet-last-doc))))
  (setq jdee-run-applet-last-doc doc)
  (let ((applet-doc (if (and jdee-run-applet-last-doc
			     (not (string= jdee-run-applet-last-doc "")))
			jdee-run-applet-last-doc
		      (if (and jdee-run-applet-doc
			       (not (string= jdee-run-applet-doc "")))
			    jdee-run-applet-doc
			  (car (jdee-run-find-html-files))))))
    (if applet-doc
	(if (string-match "appletviewer" jdee-run-applet-viewer)
	    (jdee-run-applet-internal applet-doc)
	  (if (or
	       (string= jdee-run-applet-viewer "")
	       (string-match "browse-url" jdee-run-applet-viewer))
	      (browse-url applet-doc)
	    (jdee-run-applet-internal (concat default-directory applet-doc))))
      (signal 'error "Could not find html document to display applet."))))


(defun jdee-run-menu-run-applet ()
  (interactive)
  (jdee-run-applet))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  Exception Trace Navigation                                          ;;
;;                                                                      ;;
;;  Copyright (C) 1999 Phillip Lord <p.lord@hgmp.mrc.ac.uk>             ;;
;;  Copyright (C) 2001 Sam Steingold <sds@gnu.org>                      ;;
;;  Copyright (C) 2001 Kevin A. Burton <burton@apache.org>              ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar jdee-run-etrace-current-marker (cons (make-marker) (make-marker))
  "The location of the last stack shown.
A cons of two markers, location of the error and the location in the code.")

(defvar jdee-run-font-lock-keywords
  '(("\\(^[_a-z.]+[_a-zA-Z0-9]+Exception\\)\\(: \\)?\\(.*\\)?"
     (1 'font-lock-keyword-face append)
     (3 'font-lock-string-face append))
    ("\\(at [_a-z.]+[_a-zA-Z0-9]+\\.[_a-zA-Z<>]*\\)(\\([_a-zA-Z0-9]+.java\\):\\([0-9]+\\))$"
     (1 'font-lock-constant-face append)
     (2 'font-lock-variable-name-face append)
     (3 'font-lock-type-face append)))
  "Defines font-lock keywords for highlighting exception stack traces.")

(defun jdee-run-etrace-update-current-marker ()
  "Updates the `car' of `jdee-run-etrace-current-marker' to be at the current point"
  (set-marker (car jdee-run-etrace-current-marker) (point) (current-buffer)))

(defun jdee-run-etrace-current-marker (&optional next)
  "Update the `cdr' and the `car' of `jdee-run-etrace-current-marker' from its `car'.
Here goes all the error message parsing."
  (let ((here (car jdee-run-etrace-current-marker))
	(there (cdr jdee-run-etrace-current-marker))
	(n (or next 0))
	(re (concat " \\([a-zA-Z0-9_.]+\\.\\)?" ; package
		     "\\([a-zA-Z0-9_$]+\\)" ; class
		     "\\.<?[a-zA-Z0-9_]+>?" ; method
		     "(\\([a-zA-Z0-9_]+\\)\\.java:" ; java file = public class
		     "\\([0-9]+\\))"))); line number
    (with-current-buffer (marker-buffer here)
      (goto-char here)
      ;; In order to display the current match at the top of
      ;; the error screen the previous match was set to be
      ;; at the beggining of the line. If we do not place the point
      ;; at the end of the line again, you will match the same error.
      ;; If n is 0 ensure that point is at the beggining
      (if (> n 0)
	  (end-of-line)
	(beginning-of-line))

      (if (>= n 0)
	  (if (re-search-forward re nil t)
	      (progn
		;;setting the line at the beggining
		;;so that it is the first line of the errors
		(beginning-of-line)
		(jdee-run-etrace-update-current-marker))
	    (error "End of stack trace"))
	(if (re-search-backward re nil t)
	    (progn
	      (jdee-run-etrace-update-current-marker))
	  (error "Start of stack trace")))
;;       (message "1: [%s]; 2: [%s]; 3: [%s]; 4: [%s]" (match-string 1)
;;                (match-string 2) (match-string 3) (match-string 4))
      (let* ((package (or (match-string 1) ""))
	    (class (match-string 2))
	    (file-name (match-string 3))
	    (line (car (read-from-string (match-string 4))))
	    (file (jdee-find-class-source-file (concat package file-name)))
	    buf)
	(condition-case err
	    (progn
	      (setq buf (if file (find-file-noselect file)))
	      (set-buffer buf)
	      (goto-char (point-min))
	      (forward-line (1- line))
	      (set-marker there (point) buf))
	  (error err))))
    jdee-run-etrace-current-marker))

(defun jdee-run-etrace-goto (&optional next)
  "Display the current stack using `compilation-goto-locus'."
  (jdee-run-etrace-current-marker next)
  (compilation-goto-locus (car jdee-run-etrace-current-marker)
			  (cdr jdee-run-etrace-current-marker)
			  nil))

(defun jdee-run-etrace-show-at-mouse (event)
  "Jump to the stack position at the mouse click.
Click anywhere on the line with the stack reference."
  (interactive "e")
  (let ((pos (event-start event)))
    (set-marker (car jdee-run-etrace-current-marker)
                (posn-point pos)
                (window-buffer (posn-window pos))))
  (jdee-run-etrace-goto))


(defun jdee-run-etrace-show-at-point ()
  "Jump to the stack position on this current line.
The point should be anywhere on the line with the stack reference."
  (interactive)
  (set-marker (car jdee-run-etrace-current-marker) (point) (current-buffer))
  (jdee-run-etrace-goto))


(defun jdee-run-etrace-next ()
  "Jump to the next stack position (next line)."
  (interactive)
  (jdee-run-etrace-goto 1))


(defun jdee-run-etrace-prev ()
  "Jump to the previous stack position (previous line)."
  (interactive)
  (jdee-run-etrace-goto -1))

(defun jdee-run-etrace-setup-font-lock ()
  ;;setup the correct font-lock stuff


  ;;font lock setup notes
  ;;
  ;; the actual exception class -> font-lock-keyword-face
  ;; exception message -> font-lock-string-face
  ;; stack entry class and method -> font-lock-constant-face
  ;; stack entry file -> font-lock-variable-name-face
  ;; stack entry line number -> font-lock-type-face

  (font-lock-add-keywords
   nil
   '(("\\(^[_a-z.]+[_a-zA-Z0-9]+Exception\\)\\(: \\)?\\(.*\\)?"
      (1 'font-lock-keyword-face append)
      (3 'font-lock-string-face append))))

  (font-lock-add-keywords
   nil
   '(("\\(at [_a-z.]+[_a-zA-Z0-9]+\\.[_a-zA-Z<>]*\\)(\\([_a-zA-Z0-9]+.java\\):\\([0-9]+\\))$"
      (1 'font-lock-constant-face append)
      (2 'font-lock-variable-name-face append)
      (3 'font-lock-type-face append))))
  (font-lock-fontify-buffer))

(provide 'jdee-run)

;;; jdee-run.el ends here
