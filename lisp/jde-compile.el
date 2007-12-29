;;; jde-compile.el -- Integrated Development Environment for Java.
;; $Revision: 1.65 $ $Date: 2005/04/06 03:03:07 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004, 2005 Paul Kinnucan.

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

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

(require 'eieio)
(require 'cl)
(require 'compile)

(defgroup jde-compile-options nil
  "JDE Compiler Options"
  :group 'jde
  :prefix "jde-compile-option-")

;; (makunbound 'jde-compiler)
(defcustom jde-compiler '("javac server" "")
  "Specify the type, and if necessary, the location of the compiler to
be used to compile source files for the current project. The JDE
supports three compilers: javac server, javac executable, and
jikes. The javac server runs the com.sun.tools.javac package included
with the JDK in the Beanshell. The javac executable shipped with the
JDK also uses this package. The advantage of the javac server is that
it avoids the vm startup time that accounts for most of the
compilation time consumed by the javac executable. The javac server
uses the version of com.sun.tools.javac included in the JDK for the
current project. See `jde-jdk' for more information. If you want to
use the javac executable to compile your project's source files,
select \"javac\" as the compiler type and, optionally, specify
the path to the executable in the \"Path\" field. If you do
not specify a path, the JDE uses the javac executable included in the
JDK for the current project. Similarly, to use jikes, select \"jikes\"
and, if jikes is not on the command path of the Emacs
environment, specify the path of the jikes executable."
  :group 'jde-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Compiler type"
	   (item "javac server")
	   (item "javac")
	   (item "jikes"))
	  (file 
	   :tag "Path")))
	   
	   
(defcustom jde-read-compile-args nil
"*Specify whether to prompt for additional compiler arguments.
If this variable is non-nil, the jde-compile command prompts
you to enter additional compiler arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments 
entered in the minibuffer."
  :group 'jde-project
  :type 'boolean)

(defvar jde-interactive-compile-args ""
"String of compiler arguments entered in the minibuffer.")

(defvar jde-interactive-compile-arg-history nil
"History of compiler arguments entered in the minibuffer.")

;; (makunbound 'jde-compile-finish-hook)
(defcustom jde-compile-finish-hook 
  '(jde-compile-finish-kill-buffer 
    jde-compile-finish-refresh-speedbar
    jde-compile-finish-update-class-info)
  "List of functions to be invoked when compilation of a 
Java source file terminates. Each function should accept
two arguments: the compilation buffer and a string 
describing how the compilation finished."
  :group 'jde
  :type 'hook)

(defcustom jde-compile-option-hide-classpath nil 
  "Substitute the classpath in the compilation window for
..."
  :group 'jde-compile-options
  :type 'boolean)

(defun jde-compile-update-class-list () 
  (let ((class-dir
	 (if (string= jde-compile-option-directory "")
	     (expand-file-name ".")
	   (jde-normalize-path 
	    jde-compile-option-directory 
	    'jde-compile-option-directory))))
    (message (concat "Updating class list for " class-dir))
    (jde-jeval (concat
		"jde.util.JdeUtilities.updateClassList(\"" 
		class-dir
	       "\");"))
    (message "Updating class list...done.")))

(defun jde-compile-finish-update-class-info (buf msg) 
  "Flush the classinfo cache and update the class list used by
JDEE wizards at the end of compilation.  Flush the entire cache as we
don't know which classes were recompiled."
  ;;Setting the last java buffer as the current buffer
  (condition-case nil
      (progn
	(if jde-xemacsp
	    (set-buffer (cadr (buffer-list)))
	  (set-buffer (car (buffer-list))))
	(if (eq major-mode 'jde-mode)
	    (progn
	      (setq jde-complete-last-compiled-class (jde-parse-get-buffer-class))
	      (jde-complete-flush-classes-in-cache (list jde-complete-last-compiled-class))
	      (message "Flushed completion cache.")
	      (setq jde-complete-last-compiled-class nil)
	      (jde-compile-update-class-list))))
    (error nil)))

(defun jde-compile-finish-refresh-speedbar (buf msg) 
  "Refresh speedbar at the end of a compilation."
  (if (and (frame-live-p speedbar-frame)
	    (frame-visible-p speedbar-frame))
       (speedbar-refresh)))


(defcustom jde-compile-jump-to-first-error t
  "*Automatically jump to the first error when a compilation process completes."
  :group 'jde-compile-options
  :type 'boolean)

(defun jde-compile-kill-buffer (buf) 
  (if jde-compile-enable-kill-buffer
      (progn
	(delete-windows-on buf)
	(kill-buffer buf))))


;; Thanks to Jack Donohue <donohuej@synovation.com>.
(defun jde-compile-finish-kill-buffer (buf msg)
  "Removes the jde-compile window after a few seconds if no errors."
  (save-excursion
    (set-buffer buf)
    (if (null (or (string-match ".*exited abnormally.*" msg) 
                  (string-match ".*BUILD FAILED.*" (buffer-string))))
        ;;no errors, make the compilation window go away in a few seconds
        (lexical-let ((compile-buffer buf))
          (run-at-time
           "2 sec" nil 'jde-compile-kill-buffer
           compile-buffer)
          (message "No compilation errors"))
      ;;there were errors, so jump to the first error
      (if jde-compile-jump-to-first-error (next-error 1)))))
  

(defcustom jde-compile-option-command-line-args nil
  "*Specify options as a string of command-line arguments.
The value of this variable should be a list of switches understood
by the compiler, for example, -depend -g. This variable is intended to
be used to set compile options not otherwise defined by the JDE, in
particular, options not defined by javac but used by another compiler
that you might want to use with the JDE."
  :group 'jde-compile-options
  :type '(repeat (string :tag "Argument:")))

(defcustom jde-compile-option-classpath nil
"*Specify paths of classes required to compile this project.
The JDE uses the specified paths to construct a -classpath
argument to pass to the compiler. This option overrides the
`jde-global-classpath' option."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-sourcepath nil
"*Specify the source code path to search for class or interface definitions.

As with the user class path, source path entries  can be directories, JAR 
archives, or ZIP archives. If packages are used, the local path name within 
the directory or archive must reflect the package name. 

Note that classes found through the classpath are subject to automatic 
recompilation if their sources are found."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-directory ""
  "*Specifies the root directory of the class file hierarchy.
The compiler places compiled classes in the specified
directory. For example, specifying the class
directory as: 
  
  C:\\users\\dac\\classes

causes the class files for the classes in the MyProgram.java source
file to be saved in the directory C:\\users\\dac\\classes. If your class 
is in the package demos\\awt, the class files would be placed in directory
C:\\users\\dac\\classes\\demos\\awt."
  :group 'jde-compile-options
  :type 'directory)

(defcustom jde-compile-option-deprecation nil
  "*Warn use or override of a deprecated member or class. 
A member or class is deprecated if its documentation comment contains
the @deprecated tag. The compiler will emit a warning at the end of
compilation whether or not the deprecation option is on; this option
causes the location of each individual use or override to be noted.

Deprecated members or classes are deliberately not mentioned if the
source file containing the deprecation is being recompiled.  This can
happen because the file is on the command line or because the depend
option is on and the source file is out of date.
"
  :group 'jde-compile-options
  :type 'boolean)


(defcustom jde-compile-option-debug 
  (list "selected" (list t nil nil))
  "*Include debug information in classes.
The compiler includes line number information by default.

Before JDK 1.2, the the debug and optimize options were
mutually exclusive. In JDK 1.2, it is possible to combine debug and
optimize, but the shortcuts taken by optimized code may occasionally
produce surprising debugging results. For example, declared variables
may not exist and code may appear to move or not be executed at all.

The JDK 1.1.x versions of javac do not support inclusion of selected
debug information."
  :group 'jde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Debug info to include in class:"
	   (const "all")
	   (const "none")
	   (const "selected"))
	  (list
	   :tag "    info"
	   :indent 4
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Line Numbers")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Variables")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Source")))
	   
)


(defcustom jde-compile-option-optimize nil
"*Directs the compiler to try to generate faster code. 
This may slow down compilation, make larger class files, and/or make
it difficult to debug.

Prior to 1.2, the optimize option tried to inline methods across
classes. This created compatibility problems and sometimes generated
illegal bytecode. The optimize option also implicitly turned on the
depend option and implicitly turned off the debug option.

In JDK 1.2, the optimize option no longer inlines across classes and
so may safely be used for any java compilation. Optimize no longer
implicitly turns on depend or implicitly turns off debug."
  :group 'jde-compile-options
  :type 'boolean)


(defcustom jde-compile-option-depend nil
"*Analyze dependencies.
Causes recompilation of class files on which the source files given as
command line arguments recursively depend. Without this option, only
files that are directly depended on and missing or out-of-date will be
recompiled. Recompilation does not extend to missing or out-of-date
files only depended on by already up-to-date class files.

Note: if you are using a compiler other than post JDK 1.1.6 versions
of javac, you may need to specify the command-line switch used by
the compiler to specify dependency checking. See 
`jde-compile-option-depend-switch' for more information."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-depend-switch (list "-Xdepend")
"*Specify command line switch for depend option.
This option is necessary because the command-line switch for
dependency checking differs among Java compilers. Choose
from the following options:

  -Xdepend  Full dependency checking (post JDK 1.1.6)
  -depend   Full dependency checking (jikes and pre-JDK 1.1.6)
  +F        Check everything except jar and zip files (jikes only)
  +U        Check everything including jar and zip files (jikes only)"
  :group 'jde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Select -Xdepend (javac) or -depend (jikes):"
	   (const "-Xdepend")
	   (const "-depend")
	   (const "+F")
	   (const "+U"))))

(defcustom jde-compile-option-vm-args nil
"*Specify command-line arguments for Java interpreter.
Passes the specified arguments to the Java interpreter that runs the
compiler. The argument should not contain spaces. This is useful for
adjusting the compiler's execution environment or memory usage."
  :group 'jde-compile-options
  :type '(repeat (string :tag "Option")))

(defcustom jde-compile-option-verbose nil
"*Print verbose messages.
Causes the compiler and linker to print out messages about what source
files are being compiled and what class files are being loaded."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-nowarn nil
"*Turn off warnings.
If this option is specified, the compiler does not print out any
warnings."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-encoding ""
"*Specify the source file encoding name, such as EUCJIS\\SJIS.
If this option is not specified, then the platform default converter
is used."
  :group 'jde-compile-options
  :type 'string)

;;(makunbound 'jde-compile-option-source)
(defcustom jde-compile-option-source (list "default")
"*Enables JDK version-specific features to be used in
source files.

  1.3 	  The compiler does not support assertions
  
  1.4     The compiler accepts code containing assertions. 

  1.5     Enables 1.5-specific features.

  Select \"default\" to use the source features that
  the compiler supports by default, i.e., to not include the -source
  switch on the compiler command line. For example, the javac compiler
  defaults to 1.3 source features if the -source flag is not
  used.

   ***NOTE***

   This option is supported only by versions of javac shipped
   starting with J2SDK 1.4."
  :group 'jde-compile-options
  :type '(list
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Source release:"
	   (const "default")
	   (const "1.3")
	   (const "1.4")
	   (const "1.5"))))

;;(makunbound 'jde-compile-option-target)
(defcustom jde-compile-option-target (list "1.1")
"*Generate class files that will work on VMs with the specified version.
 
The default is to generate class files to be compatible with both
1.1 and 1.2 VMs. The versions supported by javac in JDK1.2 are: 

  1.1     Ensure that generated class files will be compatible 
          with 1.1 and 1.2 VMs. This is the default.
  
  1.2     Generate class files that will run on 1.2 VMs, but 
          not on 1.1 VMs.

  1.3     Generate class files that will run on VMs in the 
          Java 2 SDK, v 1.3 and later, but will not run 
          on 1.1 or 1.2 VMs

  1.4     Generate class files that are compatible only with
          1.4 VMs.

By default, classes are compiled against the bootstrap and extension classes
of the JDK that javac shipped with. But javac also supports cross-compiling, 
where classes are compiled against a bootstrap and extension classes of a 
different Java platform implementation. It is important to use 
`jde-compile-option-bootclasspath' and `jde-compile-option-extdirs' when 
cross-compiling."
  :group 'jde-compile-options
  :type '(list
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Target VM:"
	   (const "1.1")
	   (const "1.2")
	   (const "1.3")
	   (const "1.4"))))

(defcustom jde-compile-option-bootclasspath nil
"*Cross-compile against the specified set of boot classes.
As with the user class path, boot class path entries can be 
directories, JAR archives, or ZIP archives."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-extdirs nil
"*Cross-compile against the specified extension directories. 
Each JAR archive in the specified directories is searched for class files."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

;;(makunbound 'jde-compile-option-verbose-path)
(defcustom jde-compile-option-verbose-path nil
"*Describe how paths and standard extensions were searched to find
source and class files.

   ***NOTE***

   This option is supported only by the versions of javac shipped
   with JDK 1.1.x and 1.2.x and oldjavac in JDK 1.3."

  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-enable-kill-buffer nil
  "* If true the 'jde-compile-finish-kill-buffer will kill the compilation
buffer."
  :group 'jde-compile-options
  :type 'boolean)

(defun jde-compile-show-options-buffer ()
  "Show the JDE Compile Options panel."
  (interactive)
  (customize-apropos "jde-compile-options" 'groups))

(defclass jde-compile-server-buffer (bsh-compilation-buffer) ()
  "Compiler server buffer.")

(defmethod bsh-compilation-buffer-create-native-buffer ((this jde-compile-server-buffer))
  "Creates the native Emacs buffer for the JDEE compile server."
  (oset this buffer-name "*JDEE Compile Server*")
  (oset this buffer (get-buffer-create (oref this buffer-name))))

(defclass jde-compile-exec-buffer (bsh-compilation-buffer) ()
  "Compiler exec buffer.")

(defmethod initialize-instance ((this jde-compile-exec-buffer) &rest fields)
  "Constructor for exec compilation buffer instance."

  (bsh-compilation-buffer-create-native-buffer this)

  (oset 
   this 
   filter
   (lexical-let ((this-buf this))
     (lambda (process output)
       (bsh-compilation-buffer-filter this-buf process output))))
    
  (oset this process (get-buffer-process (oref this buffer)))

  ;; Make sure this buffer is not associated with a compiler process that is
  ;; already running.
  (if (oref this process)
      (if (or (not (eq (process-status (oref this process)) 'run))
	      (yes-or-no-p
	       "A compilation process is running; kill it?"))
	  (condition-case ()
	      (progn
		(interrupt-process (oref this process))
		(sit-for 1)
		(delete-process (oref this process)))
	    (error nil))
	(error "Cannot have two processes in `%s' at once"
	       (oref this buffer-name))))

  (bsh-compilation-buffer-set-mode this))

(defmethod bsh-compilation-buffer-create-native-buffer ((this jde-compile-exec-buffer))
  "Creates the native Emacs buffer for the JDEE compile server."
  (oset this buffer-name "*compilation*")
  (oset this buffer (get-buffer-create (oref this buffer-name))))


(defclass jde-compile-compiler ()
  ((name             :initarg :name
		     :type string
		     :documentation
		     "Name of compiler")
   (version          :initarg :version
		     :type string
		     :documentation
		     "Compiler version.")
   (path             :initarg :path
		     :type string
		     :documentation
		     "Path of the compiler executable.")
   (buffer           :initarg :buffer
	             :type bsh-compilation-buffer
	             :documentation
	             "Compilation buffer")
   (window           :initarg :window
		     :type window
		     :documentation
		     "Window that displays the compilation buffer.")
   (interactive-args :initarg :interactive-args
		     :type list
		     :documentation
		     "Arguments entered in the minibuffer.")
   (use-server-p     :initarg :use-server-p
		     :type boolean
		     :documentation
		     "Run as a compile server in the Beanshell."))
  "Class of Java compilers.")

(defmethod jde-compile-classpath-arg ((this jde-compile-compiler))
  "Returns the classpath argument for this compiler."
  (let ((classpath
	 (if jde-compile-option-classpath
	     jde-compile-option-classpath
	   (jde-get-global-classpath)))
	(symbol
	 (if jde-compile-option-classpath
	     'jde-compile-option-classpath
	   'jde-global-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jde-build-classpath
	  classpath 
	  symbol)
	 ))))

(defmethod jde-compile-sourcepath-arg ((this jde-compile-compiler))
  "Get the source path argument for this compiler."
    (if jde-compile-option-sourcepath
	(list
	 "-sourcepath"
	 (jde-build-classpath
	  jde-compile-option-sourcepath
	  'jde-compile-option-sourcepath))))

(defmethod jde-compile-bootclasspath-arg ((this jde-compile-compiler))
  "Get the boot classpath argument for this compiler."
  (if jde-compile-option-bootclasspath
      (list
       "-bootclasspath"
       (jde-build-classpath jde-compile-option-bootclasspath 
			    'jde-compile-option-bootclasspath))))

(defmethod jde-compile-extdirs-arg ((this jde-compile-compiler))
  "Get the extdirs argument for this compiler."
  (if jde-compile-option-extdirs
      (list
       "-extdirs"
       (jde-build-classpath 
	jde-compile-option-extdirs
	'jde-compile-option-extdirs))))


(defmethod jde-compile-encoding-arg ((this jde-compile-compiler))
  (if (not (string= jde-compile-option-encoding ""))
      (list 
       "-encoding"
       jde-compile-option-encoding)))

(defmethod jde-compile-debug-arg ((this jde-compile-compiler))
  "Get the debug arg for this compiler."
  (let* ((include-option (nth 0 jde-compile-option-debug))
	 (selected (nth 1 jde-compile-option-debug))
	 (lines (nth 0 selected))
	 (vars (nth 1 selected))
	 (src (nth 2 selected)))
    (cond
     ((and
       (string= include-option "selected")
       lines
       (not vars)
       (not src))
      nil)
     ((string= include-option "all")
      (list "-g"))
     ((string= include-option "none")
      (list "-g:none"))
     ((and
       (string= include-option "selected")
       (or lines vars src))
      (list 
       (concat 
	"-g:"
	(if lines
	    (if (or vars src) "lines,"
	      "lines"))
	(if vars
	    (if vars
		(if src "vars," "vars")))
	(if src "source")))))))

(defmethod jde-compile-output-dir-arg ((this jde-compile-compiler))
  "Get the ouput directory arg for this compiler."
    (if (not (string= jde-compile-option-directory ""))
	(list
	 "-d"
	 (jde-normalize-path 'jde-compile-option-directory))))

(defmethod jde-compile-deprecation-arg ((this jde-compile-compiler))
  "Get deprecation argument for this compiler."
    (if jde-compile-option-deprecation
	(list "-deprecation")))

(defmethod jde-compile-optimize-arg ((this jde-compile-compiler))
  "Get optimization argument for this compiler."
    (if jde-compile-option-optimize
	(list "-O")))

(defmethod jde-compile-depend-arg ((this jde-compile-compiler))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list (car jde-compile-option-depend-switch))))

(defmethod jde-compile-vm-args ((this jde-compile-compiler))
  "Get arguments to pass to the vm used to run this compiler."
    (if jde-compile-option-vm-args
	(mapcan
	 (lambda (arg)
	   (list (concat "-J" arg)))
	 jde-compile-option-vm-args)))

(defmethod jde-compile-verbose-arg ((this jde-compile-compiler))
  "Get verbosity level argument for this compiler."
    (if jde-compile-option-verbose
	(list "-verbose")))

(defmethod jde-compile-verbose-path-arg ((this jde-compile-compiler))
  "Get verbose path argument for this compiler."
    (if jde-compile-option-verbose-path
	(list "-Xverbosepath")))

(defmethod jde-compile-nowarn-arg ((this jde-compile-compiler))
  "Get no warning argument for this compiler."
    (if jde-compile-option-nowarn
	(list "-nowarn")))

(defmethod jde-compile-command-line-args ((this jde-compile-compiler))
  "Get additional command line arguments for this compiler."
	jde-compile-option-command-line-args)

(defmethod jde-compile-target-arg ((this jde-compile-compiler))
  "Get compiler target argument for this compiler."
    (let ((target (car jde-compile-option-target)))
      (if (not (string= target "1.1"))
	  (list "-target" target))))

(defmethod jde-compile-source-arg ((this jde-compile-compiler))
  "Get compiler source argument for this compiler."
  (let ((source (car jde-compile-option-source)))
    (if (not (string= source "default"))
        (list "-source" source))))

(defmethod jde-compile-get-args ((this jde-compile-compiler))
  (append
   (jde-compile-classpath-arg this)
   (jde-compile-sourcepath-arg this)
   (jde-compile-bootclasspath-arg this)
   (jde-compile-extdirs-arg this)
   (jde-compile-encoding-arg this)
   (jde-compile-debug-arg this)
   (jde-compile-output-dir-arg this)
   (jde-compile-deprecation-arg this)
   (jde-compile-optimize-arg this)
   (jde-compile-depend-arg this)
   (jde-compile-vm-args this)
   (jde-compile-verbose-arg this)
   (jde-compile-verbose-path-arg this)
   (jde-compile-nowarn-arg this)
   (jde-compile-target-arg this)
   (jde-compile-source-arg this)
   (jde-compile-command-line-args this)))


(defmethod jde-compile-run-exec ((this jde-compile-compiler))
  (let* ((outbuf (oref (oref this buffer) buffer))
	 (compiler-path (oref this :path))
	 (source-file (file-name-nondirectory buffer-file-name))
         (flag nil)
	 (args (append
		(jde-compile-get-args this)
		(oref this :interactive-args)
		(list source-file))))

    (save-excursion
      (set-buffer outbuf)

      (let ((inhibit-read-only t)) ; make compilation buffer temporarily writable
	(insert (format "cd %s\n" default-directory))
	(insert (concat
	       compiler-path
	       " "
               (mapconcat (lambda (x) 
                            (if (and flag
                                     jde-compile-option-hide-classpath)
                                (progn
                                  (setq flag nil)
                                  "...")
                              (if (not (string= x "-classpath"))
                                  x
                                (progn
                                  (setq flag t)
                                  x)))) args " ")
	       "\n\n")))

      (let* ((process-environment (cons "EMACS=t" process-environment))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (proc (apply 'start-process 
			  (downcase mode-name)
			  outbuf
			  compiler-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress))))))

(defmethod jde-compile-run-server ((this jde-compile-compiler))
  (let* ((directory-sep-char ?/)
	   (args
	    (append
	    (jde-compile-get-args this)))
	   (source-path 
	    (jde-normalize-path buffer-file-name))
	   (arg-array (concat "new String[] {\"" source-path "\"")))
    
    (if args
	(setq arg-array
	      (concat
	       arg-array
	       ","
	       (mapconcat
		(lambda (arg)
		  (concat "\"" arg "\""))
		args
		","))))

      (setq arg-array (concat arg-array "}"))
     
      (save-excursion
	(set-buffer (oref (oref this buffer) buffer))

	(let* ((inhibit-read-only t)
	       flag
	       (arg-string
		(mapconcat 
		 (lambda (x) 
		   (if (and flag
			    jde-compile-option-hide-classpath)
		       (progn
			 (setq flag nil)
			 "...")
		     (if (not (string= x "-classpath"))
			 x
		       (progn
			 (setq flag t)
			 x))))
		 args " ")))

	  (insert "CompileServer output:\n\n")
	  (insert arg-string " " source-path "\n")))

      (if (not (jde-bsh-running-p))
	  (progn
	    (bsh-launch (oref 'jde-bsh the-bsh))
	    (bsh-eval (oref 'jde-bsh the-bsh) (jde-create-prj-values-str))))

      (bsh-buffer-eval 
       (oref 'jde-bsh the-bsh)
       (concat
	(format
	 "jde.util.CompileServer.compile(%s);"
	 arg-array) 
	"\n")
       (oref this buffer))))


(defmethod jde-compile-launch ((this jde-compile-compiler))

  (if (oref this :use-server-p)
      (jde-compile-run-server this)
    (jde-compile-run-exec this))

  (set-buffer-modified-p nil))

(defmethod jde-compile-compile ((this jde-compile-compiler))

  (if (oref this :use-server-p)
      (oset this buffer (jde-compile-server-buffer "compilation buffer"))
    (oset this buffer (jde-compile-exec-buffer "compilation buffer")))
  

  ;; Pop to compilation buffer.
  (let* ((outbuf (oref (oref this buffer) buffer))
	  (outwin (display-buffer outbuf)))
    (compilation-set-window-height outwin)
    (oset this :window outwin)

    (if (not jde-xemacsp)
	(if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))     
    
    (jde-compile-launch this)

    (setq compilation-last-buffer outbuf)))


(defclass jde-compile-javac (jde-compile-compiler)
  ()
  "Class of javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler name.
  (oset this name "javac")  

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.1 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-11 (jde-compile-compiler)
  ()
  "Class of JDK 1.1 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-11) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.1"))

(defmethod jde-compile-debug-arg ((this jde-compile-javac-11))
  "Get the debug arg for this compiler."
   (let ((include-option (nth 0 jde-compile-option-debug)))
     (cond
      ((string= include-option "all")
       (list "-g"))
      ((string= include-option "selected")
       (error "JDK 1.1 version of javac does not support selected debug info.")))))

(defmethod jde-compile-depend-arg ((this jde-compile-javac-11))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list "-depend")))

(defmethod jde-compile-get-args ((this jde-compile-javac-11))
  (append
   (jde-compile-classpath-arg this)
   (jde-compile-encoding-arg this)
   (jde-compile-debug-arg this)
   (jde-compile-output-dir-arg this)
   (jde-compile-deprecation-arg this)
   (jde-compile-optimize-arg this)
   (jde-compile-depend-arg this)
   (jde-compile-vm-args this)
   (jde-compile-verbose-arg this)
   (jde-compile-nowarn-arg this)
   (jde-compile-command-line-args this)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.2 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-12 (jde-compile-compiler)
  ()
  "Class of JDK 1.2 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-12) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.2"))

(defmethod jde-compile-depend-arg ((this jde-compile-javac-12))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list "-Xdepend")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.3 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-13 (jde-compile-javac-12)
  ()
  "Class of JDK 1.3 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-13) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.3"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.4 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-14 (jde-compile-javac-13)
  ()
  "Class of JDK 1.4 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-14) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.4"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; J2SDK 1.5 Compiler                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-15 (jde-compile-javac-14)
  ()
  "Class of J2SDK 1.5 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-15) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.5"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Jikes Compiler                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-jikes (jde-compile-compiler)
  ()
  "Class of jikes compilers.")

(defmethod initialize-instance ((this jde-compile-jikes) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler name.
  (oset this :name "jikes")

  ;; Set compiler version.
  (oset this version "1.14"))

(defmethod jde-compile-debug-arg ((this jde-compile-jikes))
  "Get the debug arg for this compiler."
  (let ((include-option (nth 0 jde-compile-option-debug)))
    (cond
     ((string= include-option "all")
      (list "-g"))
     ((string= include-option "selected")
      (error "Jikes does not support jde-compile-option-debug's selected debug info option.")))))

(defmethod jde-compile-depend-arg ((this jde-compile-jikes))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list "-depend")))

(defmethod jde-compile-command-line-args ((this jde-compile-jikes))
  "Get additional command line arguments for this compiler."
	(append
	 (list "+E")
	 jde-compile-option-command-line-args))

(defmethod jde-compile-classpath-arg ((this jde-compile-jikes))
  "Returns the classpath argument for this compiler."
  (let ((classpath (call-next-method))
        (rt        (expand-file-name "jre/lib/rt.jar" (jde-get-jdk-dir))))
    (if (file-exists-p rt)
        (if classpath
	    (or (string-match "jre/lib/rt\.jar" (cadr classpath))
		(setcar (cdr classpath)
			(concat (cadr classpath)
				jde-classpath-separator
				rt)))
	  (setq classpath (list "-classpath" rt))))
    classpath))

(defmethod jde-compile-get-args ((this jde-compile-jikes))
  (append
   (jde-compile-classpath-arg this)
   (jde-compile-sourcepath-arg this)
   (jde-compile-bootclasspath-arg this)
   (jde-compile-extdirs-arg this)
   (jde-compile-encoding-arg this)
   (jde-compile-debug-arg this)
   (jde-compile-output-dir-arg this)
   (jde-compile-deprecation-arg this)
   (jde-compile-source-arg this)
   (jde-compile-optimize-arg this)
   (jde-compile-depend-arg this)
   (jde-compile-verbose-arg this)
   (jde-compile-verbose-path-arg this)
   (jde-compile-nowarn-arg this)
   (jde-compile-target-arg this)
   (jde-compile-command-line-args this)))


(defvar jde-compile-javac-compilers
  (list
   (jde-compile-javac-11 "javac 1.1.x")
   (jde-compile-javac-12 "javac 1.2.x")
   (jde-compile-javac-13 "javac 1.3.x")
   (jde-compile-javac-14 "javac 1.4.x")
   (jde-compile-javac-15 "javac 1.5.x"))
  "List of supported javac compilers.")

(defun jde-compile-get-javac ()
  (let* ((jdk-version (jde-java-version))
	 (jdk-split-version (split-string jdk-version "[.]"))
	 (jdk-major-version (nth 0 jdk-split-version))
	 (jdk-minor-version (nth 1 jdk-split-version))
	 (compiler
	  (find-if 
	   (lambda (compiler-x)
	     (let* ((compiler-split-version (split-string (oref compiler-x :version) "[.]"))
		    (compiler-major-version (nth 0 compiler-split-version))
		    (compiler-minor-version (nth 1 compiler-split-version)))
	       (and 
		(string= jdk-major-version compiler-major-version)
		(string= jdk-minor-version compiler-minor-version))))
	   jde-compile-javac-compilers)))
    (unless compiler
      (let ((latest-javac (car (last jde-compile-javac-compilers))))
	(if
	    (yes-or-no-p 
	     (format "The JDE does not recognize JDK %s javac. Assume JDK %s javac?"
		     jdk-version (oref latest-javac :version)))
	    (setq compiler latest-javac))))
    (if compiler 
	(if (string= (car jde-compiler) "javac server")
	    (oset compiler :use-server-p t)
	  (progn
	    (oset compiler :use-server-p nil)
	    (oset compiler 
		  :path 
		  (let ((compiler-path 
			 (substitute-in-file-name (nth 1 jde-compiler))))
		    (if (string= compiler-path "")
			(setq compiler-path (jde-get-jdk-prog 'javac))
		      (if (file-exists-p compiler-path)
			  compiler-path
			(error (format "Invalid compiler path %s"
				       compiler-path)))))))))
    compiler))
	   
	     
(defun jde-compile-get-jikes ()
  (let ((compiler-path 
		(substitute-in-file-name (nth 1 jde-compiler))))

    (if (string= compiler-path "")
	(if (executable-find "jikes")
	    (setq compiler-path "jikes")
	  (error "Cannot find jikes."))
      (unless
          (or
           (file-exists-p
            (if (and
                 (eq system-type 'windows-nt)
                 (not (string-match "[.]exe$" compiler-path)))
                (concat compiler-path ".exe")
              compiler-path))
           (executable-find compiler-path))
        (error "Invalid compiler path: %s" compiler-path)))

  (jde-compile-jikes
     "Jikes"
     :use-server-p nil
     :path compiler-path)))

(defun jde-compile-get-the-compiler ()
  "Get a compiler object that represents the compiler specified
by `jde-compiler'."
  (let ((compiler-name (car jde-compiler)))
    (cond 
     ((string-match "javac" compiler-name)
       (jde-compile-get-javac))
     ((string-match "jikes" compiler-name)
      (jde-compile-get-jikes))
     (t
      (error "The JDEE does not support a compiler named %s" compiler-name)))))
     

;;;###autoload
(defun jde-set-compile-options (options)
  "Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose."
  (interactive
   "sEnter options: ")
  (setq jde-compile-option-command-line-args (split-string options " ")))

;;;###autoload
(defun jde-compile ()
  "Compile the Java program in the current buffer.
This command invokes the compiler specified by `jde-compiler'
with the options specified by the JDE customization variables
that begin with `jde-compile'. If the variable
`jde-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled. If `jde-compiler' specifies the JDE compile
server, this command uses the compile server. Otherwise, it
uses the compiler executable specified by
`jde-compiler' to compile."
  (interactive)

  (if jde-read-compile-args
      (setq jde-interactive-compile-args
	      (read-from-minibuffer 
	       "Compile args: "
	       jde-interactive-compile-args
	       nil nil
	       '(jde-interactive-compile-arg-history . 1))))

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-compile from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not jde-xemacsp))	
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-compile
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (setq compilation-finish-function 
      (lambda (buf msg) 
	(run-hook-with-args 'jde-compile-finish-hook buf msg)
	(setq compilation-finish-function nil)))

    (let ((compiler (jde-compile-get-the-compiler)))
      (if compiler
	  (progn
	    (oset compiler 
		  :interactive-args 
		  (if (and jde-interactive-compile-args
			   (not (string= jde-interactive-compile-args "")))
		      (split-string jde-interactive-compile-args " ")))
	    (jde-compile-compile compiler))
	(error "Unknown compiler. Aborting compilation."))))
    


(provide 'jde-compile)

;; Change History
;; $Log: jde-compile.el,v $
;; Revision 1.65  2005/04/06 03:03:07  paulk
;; Fix jde-compile-kill-buffer to honor jde-compile-enable-kill-buffer.
;;
;; Revision 1.64  2005/02/02 03:49:31  paulk
;; Updated to be compatible with new Emacs policy of making compilation buffers read-only.
;;
;; Revision 1.63  2004/10/20 06:17:06  paulk
;; Update to support reloading classes from a single classpath entry. Thanks to Martin Schwamberger.
;;
;; Revision 1.62  2004/08/21 04:29:20  paulk
;; Update the wizard class list after compiling a class.
;;
;; Revision 1.61  2004/08/03 03:55:00  paulk
;; Emacs 21.3.5 compatibility fix: Updated jde-compile-finish-kill-buffer to include prefix argument which is not optional in Emacs 21.3.5.
;;
;; Revision 1.60  2004/05/26 05:15:33  paulk
;; Fix version-matching bug in jde-compile-get-javac.
;;
;; Revision 1.59  2004/04/15 03:58:59  paulk
;; Add "default" option to jde-compile-option-source.
;;
;; Revision 1.58  2004/02/22 08:02:29  paulk
;; Update to support J2SDK1.5.0.
;;
;; Revision 1.57  2003/07/15 11:56:23  paulk
;; Update jde-compile-option-target.
;;
;; Revision 1.56  2003/04/08 03:16:28  paulk
;; Fixes regression that causes compilation buffer's default directory to be set incorrectly. Also rename compile server buffer from *compilation* to *JDEE Compile Server*
;;
;; Revision 1.55  2003/04/05 06:26:19  paulk
;; Added jde-compile-jump-to-first-error option. Thanks to Sean Wellington.
;;
;; Revision 1.54  2003/04/04 13:35:00  jslopez
;; Adds the source argument to the jikes compiler.
;;
;; Revision 1.53  2003/03/04 10:12:50  paulk
;; Updated the compile server to use the new compilation-mode buffer support
;; provided by bsh class.
;;
;; Revision 1.52  2003/02/28 04:13:33  jslopez
;; Adds customization variable jde-compile-enable-kill-buffer. If true the
;; jde-compile-finish-kill-buffer hook will kill the compilation buffer.
;;
;; Revision 1.51  2003/02/17 08:13:05  paulk
;; Changes required to support new package-independent version of beanshell.el
;;
;; Revision 1.50  2003/02/07 00:31:57  jslopez
;; Adds jde-compile-option-source to be compile 1.4 code with
;; the assert key word on it.
;;
;; Revision 1.49  2002/12/12 05:24:46  paulk
;; Fixed bug  that caused the compile command to fail when jikes
;; is the compiler and jde-compiler does not specify the path of the
;; jikes executable.
;;
;; Revision 1.48  2002/11/22 09:32:13  paulk
;; Improve error reporting when a user attempts to use an unsupported compiler.
;;
;; Revision 1.47  2002/11/05 07:47:25  paulk
;; Mac OS (darwin) compatibility fix: find path of javac compiler. Thanks to Andrew Hyatt.
;;
;; Revision 1.46  2002/09/18 15:20:19  jslopez
;; Fixes bug in jde-compile-finish-kill-buffer.
;; The regexp BUILD FAILED was being match agains the wrong string.
;; Now it is being match against the contents of the compilation buffer.
;;
;; Revision 1.45  2002/09/16 04:23:50  paulk
;; Added missing -encoding switch for compiler encoding command line argument.
;; Thanks to Toru Takahashi.
;;
;; Revision 1.44  2002/08/07 06:36:20  paulk
;; Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
;; removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
;; for suggesting this restructuring. Also fixed a number of compiler warnings caused
;; by genuine bugs.
;;
;; Revision 1.43  2002/08/03 05:01:45  paulk
;; Added a jde-compile-finish-hook function that kills the compilation
;; buffer if no compilation errors occured. Thanks to Jack Donohue <donohuej@synovation.com>.
;;
;; Revision 1.42  2002/06/12 07:04:30  paulk
;; XEmacs compatibility fix: set win32-quote-process-args wherever
;; the JDEE sets w32-quote-process-args. This allows use of spaces in
;; paths passed as arguments to processes (e.g., javac)  started by
;; the JDEE.
;;
;; Revision 1.41  2002/06/11 06:24:27  paulk
;; Provides support for paths containing spaces as compiler arguments via the following changes;
;; - Locally set the w32-quote-process-args variable to a quotation mark.
;; - The compile server Lisp code now passes the compiler arguments to the
;;   compile server Java code as an array of strings instead of as a single space-delimited
;;   string of arguments.
;;
;; Revision 1.40  2002/05/12 06:23:20  paulk
;; Replaced call to jde-complete-get-name-of-this-class with call to
;; jde-parse-buffer-class.
;;
;; Revision 1.39  2002/03/17 14:20:40  jslopez
;; Fixes bug using jde-compile-option-vm-args.
;;
;; Revision 1.38  2002/03/03 14:42:26  jslopez
;; Fixes bug getting compilation status in
;; linux.
;;
;; Revision 1.37  2002/03/03 13:51:03  jslopez
;; Fixes bug getting compilation status code.
;; Makes the code more robust.
;;
;; Revision 1.36  2002/02/25 20:10:41  jslopez
;; The Compile Server now uses its own filter.
;;
;; Revision 1.35  2002/02/15 02:52:42  jslopez
;; Updates the compile server to be interactive.
;;
;; Revision 1.34  2001/12/12 05:40:53  paulk
;; Removed an extraneous print form from jde-compile-run-exec that was dumping
;; the compiler arguments into the minibuffer.
;;
;; Revision 1.33  2001/12/09 17:09:26  jslopez
;; Adding the -classpath flag when the classpath is nil.
;;
;; Revision 1.32  2001/12/09 17:01:04  jslopez
;; Fixes bug in the method jde-compile-classpath-arg (jikes)
;; that will cause an error when appending the path for rt.jar
;; to a nil classpath.
;;
;; Revision 1.31  2001/12/08 05:49:20  paulk
;; Fixed jde-compile-get-javac to handle unknown version of the JDK.
;;
;; Revision 1.30  2001/11/29 10:00:33  paulk
;; Fixed bug where the JDE was inserting a null string in the javac
;; argument list when the user did not specify arguments interactively, which is normally the case. This bug caused compiling with the javac executable to fail on XEmacs.
;;
;; Revision 1.29  2001/11/28 08:32:10  paulk
;; Compile server now runs jde-normalize-path on the path of the path of the source file.
;; This ensures that the path will be converted to a Windows path on the Cygwin version
;; of XEmacs.
;;
;; Revision 1.28  2001/11/23 14:21:47  paulk
;; Fixed cut-and-paste error in the debug options error messages for Jikes.
;;
;; Revision 1.27  2001/10/24 04:13:33  jslopez
;; Adds the customization variable jde-compile-option-hide-classpath.
;; A non-nil variable causes the classpath value in the compilation
;; output to be shown as ... . i.e java -classpath ... -g Test.
;;
;; Revision 1.26  2001/10/19 10:10:59  paulk
;; Removed space in front of -g option that made it unrecognizable to compiler.
;; Thanks to bert van vreckem.
;;
;; Revision 1.25  2001/10/19 04:14:20  paulk
;; Bug fix: compile command once again supports the jde-read-compile-args option.
;; Thanks to Luis Novais for reporting this bug.
;;
;; Revision 1.24  2001/10/16 05:10:50  paulk
;; - Fixed Lisp error that occurred when the default setting
;;   for jde-compile-option-debug is in effect.
;;
;; - XEmacs compatibility fix. Fixed undefined variable and
;;   function Lisp errors that occur when trying to compile
;;   a Java source file.
;;
;; - XEmacs compatibility fix. Fixed bug that produced a Beanshell
;;   error when trying to run the compile server in XEmacs.
;;
;; Revision 1.23  2001/10/08 13:06:11  paulk
;; Minor restructuring of compile server code.
;;
;; Revision 1.22  2001/10/05 11:02:27  paulk
;; Reordered the compile server output message so that the file name follows the other arguments.
;;
;; Revision 1.21  2001/10/05 04:08:01  paulk
;; - Fixed compile error that occurs when jde-compile-option-debug is set.
;;   This was due to the -g option being wrapped in an extra list.
;; - Now includes rt.jar in the classpath when jikes is the compiler.
;;   Thanks to David Ponce.
;;
;; Revision 1.20  2001/10/01 20:48:58  jslopez
;; Added support for jdk1.4.
;;
;; Revision 1.19  2001/10/01 12:02:34  paulk
;; - Now uses the version of javac shipped with the JDK specified by jde-jdk.
;; - Now generates the correct options for the various versions of javac.
;; - Now includes the +E options if jikes is the selected compiler.
;;
;; Revision 1.18  2001/10/01 02:24:49  paulk
;; - Created jde-compile-javac, jde-compile-jikes, jde-compile-javac-11x classes.
;;
;; - Fixed missing slot names in jde-compile-compiler class. Thanks to Eric Friedman.
;;
;; Revision 1.17  2001/09/28 05:10:10  paulk
;; - Redefined jde-compiler to allow selection of the
;;   following options: javac executable, javac server,
;;   and jikes executable.
;;
;; - Redefinded jde-compile-option-command-line-args to
;;   accept a list of strings, each corresponding to one
;;   argument.
;;
;; - Created a new class jde-compile-compiler to serve
;;   as the root class for all compilers supported by
;;   the JDE. This should promote sharing code among
;;   the compilers.
;;
;; Revision 1.16  2001/08/30 13:21:57  jslopez
;; Fixed bug in jde-compile-internal that did not scroll the compilation output
;; when the compile server and compilation-scroll-output were enable.
;;
;; Revision 1.15  2001/08/30 01:31:54  paulk
;; Adds support for compile server. Thanks to Javier Lopez.
;;
;; Revision 1.14  2001/04/16 05:47:33  paulk
;; Normalized paths. Thanks to Nick Sieger.
;;
;; Revision 1.13  2001/04/11 03:23:18  paulk
;; Updated to resolve relative paths relative to the project file that defines them. Thanks to Nick Sieger.
;;
;; Revision 1.12  2001/04/02 02:42:58  paulk
;; Remove extraneous definition of jde-build-classpath-arg.
;;
;; Revision 1.11  2001/03/13 04:03:47  paulk
;; Changed the type of jde-compile-option-directory from string to directory to permit path completion.
;;
;; Revision 1.10  2001/02/20 05:15:10  paulk
;; You can now use environment variables, tilde notation, and cygwin syntax in jde-compile-option-directory path.
;;
;; Revision 1.9  2001/02/17 17:43:34  paulk
;; Added support for JDK 1.3 targets.
;;
;; Revision 1.8  2001/02/03 08:18:44  paulk
;; Changed declarations of customized variables so that you can now use completion (M tab) to complete path names.
;;
;; Revision 1.7  2000/09/21 02:05:11  paulk
;; Fixes bug in formatting jde-compile-option-vm-args for the command line.
;;
;; Revision 1.6  2000/08/19 07:04:28  paulk
;; Adds compile finish hook.
;;
;; Revision 1.5  2000/08/11 05:04:45  paulk
;; Added jde-compile-finish-hook variable.
;;
;; Revision 1.4  2000/04/10 05:27:30  paulk
;; Compile command now supports Cygwin-style class paths.
;;
;; Revision 1.3  1999/01/15 22:04:15  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.2  1998/12/07 01:35:28  paulk
;; Updated compile options to reflect changes in command-line options
;; accepted by javac.
;;
;; Revision 1.1  1998/12/06 02:37:54  paulk
;; Initial revision
;;

;; End of jde-compile.el
