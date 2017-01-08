;;; jdee-compile.el -- Integrated Development Environment for Java.

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Author: Suraj Acharya <sacharya@cs.indiana.edu>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004, 2005, 2008 Paul Kinnucan.
;; Copyright (C) 2009 by Paul Landes
;; Copyright (C) 2006-2007 by Suraj Acharya

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

;; It includes code for using the Eclipse compiler originally written
;; by Suraj Acharya.

;; When customizing the jdee-compiler variable to use the option for
;; "eclipse java compiler server" you will also need to specify the
;; location of the eclipse java compiler classes.

;; If you've installed eclipse locally then this is the jdtcore.jar
;; under <eclipse dir>/plugins/org.eclipse.jdt.core_x.x.x/, where
;; x.x.x depends on the version of eclipse you have.

;; If you don't have eclipse you can download just the JDT
;; compiler.  Go to http://download.eclipse.org/eclipse/downloads/ and
;; pick the release you want, the latest release is usually stable
;; enough to use.  Once you get to the downloads page for the release,
;; scroll down to find the link to download the "JDT Core Batch
;; Compiler".  The 1 MB ecj.jar file is all you need to download.

;; Check that you have the correct jar by trying to run the compiler
;; from a command line like so:
;; java -cp <path to jar> org.eclipse.jdt.internal.compiler.batch.Main
;; This should print out a usage message for the "Eclipse Java Compiler".

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'compile)
(require 'jdee-backend)
(require 'jdee-classpath)
(require 'jdee-files)

;; FIXME: refactor to eliminate these
(declare-function jdee-complete-flush-classes-in-cache "jdee-complete" (class-list))
(declare-function jdee-get-jdk-prog "jdee" (progname))
(declare-function jdee-java-version "jdee" ())

(defvar jdee-complete-last-compiled-class)

(defgroup jdee-compile-options nil
  "JDEE Compiler Options"
  :group 'jdee
  :prefix "jdee-compile-option-")

;; (makunbound 'jdee-compiler)
(defcustom jdee-compiler '("javac server")
  "Specify the type, and if necessary, the location of the compiler to
be used to compile source files for the current project. The JDE
supports three compilers: javac server, javac executable, and
the eclipse java compiler (ecj). The javac server runs the com.sun.tools.javac package included
with the JDK in the Beanshell. The javac executable shipped with the
JDK also uses this package. The advantage of the javac server is that
it avoids the vm startup time that accounts for most of the
compilation time consumed by the javac executable. The javac server
uses the version of com.sun.tools.javac included in the JDK for the
current project. See `jdee-jdk' for more information. If you want to
use the javac executable to compile your project's source files,
select \"javac\" as the compiler type and, optionally, specify
the path to the executable in the \"Path\" field. If you do
not specify a path, the JDE uses the javac executable included in the
JDK for the current project. Similarly, to use ecj, select \"eclipse java compiler server\"
and specify the path of the eclipse compiler ecj.jar."
  :group 'jdee-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Compiler type"
           (item "javac")
	   (item "javac server")
	   (list :format "%v"
                 (const "eclipse java compiler server")
                 (file :tag "Path to  ecj.jar (or jdt core jar)"))
           ))
  )

;; convert jdee-compiler values from the old format to the one used these days
(let ((compiler-name (car jdee-compiler)))
(when (not (listp compiler-name))
  (setq jdee-compiler
        (cond
         ((equal compiler-name "javac") '("javac"))
         ((equal compiler-name "javac server") '("javac server"))
         (t "javac server")))))

(defcustom jdee-read-compile-args nil
"*Specify whether to prompt for additional compiler arguments.
If this variable is non-nil, the jdee-compile command prompts
you to enter additional compiler arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments
entered in the minibuffer."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-compiler-new-compile-el
  (boundp 'compilation-error-regexp-alist-alist)
  "Check if we have the new (21.3+) compile.el.
Set this to t if you are running an Emacs with the new compile.el
and want to get slightly better font-locking in the compile
buffer.  A value of nil will force the use of older style
compilation-error-regexp.  This variable tries to auto-detect the
compile.el version by checking if
`compilation-error-regexp-alist-alist' is defined."
  :group 'jdee-compile-options
  :type 'boolean)

(if jdee-compiler-new-compile-el
    (progn
      (setq compilation-error-regexp-alist
            (cons '("----------\n\\([0-9]+. ERROR in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)\\)"
                    2 3 nil 2 1 (6 compilation-error-face)
                    )
                  compilation-error-regexp-alist))

      (setq compilation-error-regexp-alist
            (cons '("----------\n\\([0-9]+. WARNING in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)\\)"
                    2 3 nil 1 1 (6 compilation-warning-face)
                    )
                  compilation-error-regexp-alist)))
  ;; else
  (setq compilation-error-regexp-alist
        (cons '("----------\n[0-9]+. \\(ERROR\\|WARNING\\) in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)"
                2 3)
              compilation-error-regexp-alist)))

(defvar jdee-interactive-compile-args ""
"String of compiler arguments entered in the minibuffer.")

(defvar jdee-interactive-compile-arg-history nil
"History of compiler arguments entered in the minibuffer.")

;; (makunbound 'jdee-compile-finish-hook)
(defcustom jdee-compile-finish-hook
  '(jdee-compile-finish-kill-buffer
    jdee-compile-finish-refresh-speedbar
    jdee-compile-finish-update-class-info)
  "List of functions to be invoked when compilation of a
Java source file terminates. Each function should accept
two arguments: the compilation buffer and a string
describing how the compilation finished."
  :group 'jdee
  :type 'hook)

(defcustom jdee-compile-option-hide-classpath nil
  "Substitute the classpath in the compilation window for
..."
  :group 'jdee-compile-options
  :type 'boolean)

(defvar jdee-compile-mute nil
  "Setting to non-nil will silence some of the message")


(defun jdee-compile-update-class-list ()
  (let ((class-dir
	 (if (string= jdee-compile-option-directory "")
	     (expand-file-name ".")
	   (jdee-normalize-path
	    jdee-compile-option-directory
	    'jdee-compile-option-directory))))
    (unless jdee-compile-mute
      (message (concat "Updating class list for " class-dir)))
    (jdee-backend-update-class-list class-dir)
    (unless jdee-compile-mute
      (message "Updating class list...done."))))

(defun jdee-compile-finish-update-class-info (buf msg)
  "Flush the classinfo cache and update the class list used by
JDEE wizards at the end of compilation.  Flush the entire cache as we
don't know which classes were recompiled."
  ;;Setting the last java buffer as the current buffer
  (condition-case nil
      (progn
	(set-buffer (car (buffer-list)))
	(if (eq major-mode 'jdee-mode)
	    (progn
	      (setq jdee-complete-last-compiled-class (jdee-parse-get-buffer-class))
	      (jdee-complete-flush-classes-in-cache (list jdee-complete-last-compiled-class))
              (unless jdee-compile-mute
                (message "Flushed completion cache."))
	      (setq jdee-complete-last-compiled-class nil)
	      (jdee-compile-update-class-list))))
    (error nil)))

;;; TODO: remove from here and add observer
(defun jdee-compile-finish-refresh-speedbar (buf msg)
  "Refresh speedbar at the end of a compilation."
  (if (and (boundp 'speedbar-frame)
	   (frame-live-p speedbar-frame)
	   (frame-visible-p speedbar-frame))
      (speedbar-refresh)))

(defcustom jdee-compile-jump-to-first-error t
  "*Automatically jump to the first error when a compilation process completes."
  :group 'jdee-compile-options
  :type 'boolean)

(defun jdee-compile-kill-buffer (buf)
  (delete-windows-on buf)
  (kill-buffer buf))

(defun jdee-compile--successful-compilation-p (msg buffer-content)
  "Return non-nil when `MSG' or `BUFFER-CONTENT' don't contain errors."
  (null (or (string-match "exited abnormally" msg)
            (string-match "BUILD FAILED" buffer-content))))

(defun jdee-compile--kill-compile-buffer (buf)
  "Make the compilation buffer `BUF' go away in a few seconds."
  (if (if (numberp jdee-compile-enable-kill-buffer)
          (not (minusp jdee-compile-enable-kill-buffer))
        jdee-compile-enable-kill-buffer)
      (lexical-let ((compile-buffer buf))
        (run-at-time
         (format "%d sec" (if (numberp jdee-compile-enable-kill-buffer)
                              jdee-compile-enable-kill-buffer 2))
         nil 'jdee-compile-kill-buffer
         compile-buffer)
        (message "No compilation errors"))))

;; Thanks to Jack Donohue <donohuej@synovation.com>.
(defun jdee-compile-finish-kill-buffer (buf msg)
  "Remove the jdee-compile window after a few seconds if no errors."
  (with-current-buffer buf
    (if (jdee-compile--successful-compilation-p msg (buffer-string))
        (jdee-compile--kill-compile-buffer buf)
      (when jdee-compile-jump-to-first-error
        (next-error 1)))))


(defcustom jdee-compile-option-command-line-args nil
  "*Specify options as a string of command-line arguments.
The value of this variable should be a list of switches understood
by the compiler, for example, -depend -g. This variable is intended to
be used to set compile options not otherwise defined by the JDE, in
particular, options not defined by javac but used by another compiler
that you might want to use with the JDE."
  :group 'jdee-compile-options
  :type '(repeat (string :tag "Argument:")))

(defcustom jdee-compile-option-classpath nil
"*Specify paths of classes required to compile this project.
The JDEE uses the specified paths to construct a -classpath
argument to pass to the compiler. If you do not specify this
option, the JDEE uses the value of the `jdee-global-classpath'
option to compile this project.

Starting in JDK 1.6, a class path element containing a basename
of * is considered equivalent to specifying a list of all the
files in the directory with the extension .jar or .JAR.  For
example, if directory foo contains a.jar and b.JAR, then the
class path element foo/* is expanded to A.jar;b.JAR, except that
the order of jar files is unspecified. All jar files in the
specified directory, even hidden ones, are included in the
list. A classpath entry consisting simply of * expands to a list
of all the jar files in the current directory. The CLASSPATH
environment variable, where defined, will be similarly
expanded. Note: Depending of the configuration of your command
line environment, you may have to quote the wild card character,
for example, javac -cp \"*.jar\" MyClass.java."
  :group 'jdee-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jdee-compile-option-sourcepath nil
"*Specify the source code path to search for class or interface definitions.

As with the user class path, source path entries  can be directories, JAR
archives, or ZIP archives. If packages are used, the local path name within
the directory or archive must reflect the package name.

Note that classes found through the classpath are subject to automatic
recompilation if their sources are found."
  :group 'jdee-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jdee-compile-option-directory ""
  "*Specifies the root directory of the class file hierarchy.
The compiler places compiled classes in the specified
directory. For example, specifying the class
directory as:

  C:\\users\\dac\\classes

causes the class files for the classes in the MyProgram.java source
file to be saved in the directory C:\\users\\dac\\classes. If your class
is in the package demos\\awt, the class files would be placed in directory
C:\\users\\dac\\classes\\demos\\awt."
  :group 'jdee-compile-options
  :type 'directory)

(defcustom jdee-compile-option-deprecation nil
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
  :group 'jdee-compile-options
  :type 'boolean)


(defcustom jdee-compile-option-debug
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
  :group 'jdee-compile-options
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


(defcustom jdee-compile-option-optimize nil
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
  :group 'jdee-compile-options
  :type 'boolean)


(defcustom jdee-compile-option-depend nil
"*Analyze dependencies.
Causes recompilation of class files on which the source files given as
command line arguments recursively depend. Without this option, only
files that are directly depended on and missing or out-of-date will be
recompiled. Recompilation does not extend to missing or out-of-date
files only depended on by already up-to-date class files.

Note: if you are using a compiler other than post JDK 1.1.6 versions
of javac, you may need to specify the command-line switch used by
the compiler to specify dependency checking. See
`jdee-compile-option-depend-switch' for more information."
  :group 'jdee-compile-options
  :type 'boolean)

(defcustom jdee-compile-option-depend-switch (list "-Xdepend")
"*Specify command line switch for depend option.
This option is necessary because the command-line switch for
dependency checking differs among Java compilers. Choose
from the following options:

  -Xdepend  Full dependency checking (post JDK 1.1.6)
  -depend   Full dependency checking (pre-JDK 1.1.6)"
  :group 'jdee-compile-options
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Select -Xdepend (javac) or -depend (pre-JDK 1.1.6):"
	   (const "-Xdepend")
	   (const "-depend"))))

(defcustom jdee-compile-option-vm-args nil
"*Specify command-line arguments for Java interpreter.
Passes the specified arguments to the Java interpreter that runs the
compiler. The argument should not contain spaces. This is useful for
adjusting the compiler's execution environment or memory usage."
  :group 'jdee-compile-options
  :type '(repeat (string :tag "Option")))

(defcustom jdee-compile-option-verbose nil
"*Print verbose messages.
Causes the compiler and linker to print out messages about what source
files are being compiled and what class files are being loaded."
  :group 'jdee-compile-options
  :type 'boolean)

(defcustom jdee-compile-option-nowarn nil
"*Turn off warnings.
If this option is specified, the compiler does not print out any
warnings."
  :group 'jdee-compile-options
  :type 'boolean)


;;(makunbound 'jdee-compile-option-annotation-processors)
(defcustom jdee-compile-option-annotation-processors nil
"*Names of the annotation processors to run. This bypasses the default discovery process."
  :group 'jdee-compile-options
  :type '(repeat (string :tag "Name")))

;;(makunbound 'jdee-compile-option-annotation-processing)
(defcustom jdee-compile-option-annotation-processing
  (list "compile and process annotations")
"*Controls whether annotation processing and/or compilation is done."
  :group 'jdee-compile-options
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Processing:"
	   (const "compile and process annotations")
	   (const "compile only")
	   (const "process annotations only"))))

;;(makunbound 'jdee-compile-option-annotation-processor-options)
(defcustom jdee-compile-option-annotation-processor-options nil
"*Options to pass to annotation processors. These are not
interpreted by javac directly, but are made available for use by
individual processors. key should be one or more identifiers
separated by \".\"."
  :group 'jdee-compile-options
  :type '(repeat
	  (cons :tag "Option"
	   (string :tag "Key")
	   (string :tag "Value"))))

(defcustom jdee-compile-option-encoding ""
"*Specify the source file encoding name, such as EUCJIS\\SJIS.
If this option is not specified, the platform default converter
is used."
  :group 'jdee-compile-options
  :type 'string)

;;(makunbound 'jdee-compile-option-implicit)
(defcustom jdee-compile-option-implicit
  (list "Generate and warn")
"*Specify whether to generate class files for source files loaded in
order to get type information needed to compile and/or process
annotations in other files. Options are:

  * Generate and warn

    Generate class files for source files loaded to get type info for other files.
    Do not generate class files for source files needed only to process
    annotations. Instead issue a warning.

  * Generate and do not warn

    Generate class files for source files needed to compile other files. Do
    not generate class files for source needed for processing annotions
    in other files and do not warn that class files are not generated.

  * Do not generate

    Do not generate class files for implicitly loaded source files.
"
  :group 'jdee-compile-options
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Processing:"
	   (const "Generate and warn")
	   (const "Generate and do not warn")
	   (const "Do not generate"))))


;;(makunbound 'jdee-compile-option-source)
(defcustom jdee-compile-option-source (list "default")
"*Enables JDK version-specific features to be used in
source files.

  1.3	  The compiler does not support assertions

  1.4     The compiler accepts code containing assertions.

  1.5     Enables 1.5-specific features.

  1.6     Enables 1.6-specific features.

  1.7     Enables 1.7-specific features.

  1.8     Enables 1.8-specific features.

  Select \"default\" to use the source features that
  the compiler supports by default, i.e., to not include the -source
  switch on the compiler command line. For example, the javac compiler
  defaults to 1.3 source features if the -source flag is not
  used.

   ***NOTE***

   This option is supported only by versions of javac shipped
   starting with J2SDK 1.4."
  :group 'jdee-compile-options
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Source release:"
	   (const "default")
	   (const "1.3")
	   (const "1.4")
	   (const "1.5")
	   (const "1.6")
	   (const "1.7")
	   (const "1.8"))))

;;(makunbound 'jdee-compile-option-target)
(defcustom jdee-compile-option-target (list "default")
"*Generate class files that will work on VMs with the specified version.

  1.1     Ensure that generated class files will be compatible
	  with 1.1 and 1.2 VMs.

  1.2     Generate class files that will run on 1.2 VMs, but
	  not on 1.1 VMs.

  1.3     Generate class files that will run on VMs in the
	  Java 2 SDK, v 1.3 and later, but will not run
	  on 1.1 or 1.2 VMs

  1.4     Generate class files that are compatible only with
	  1.4 VMs.

  1.5     Generate class files that are compatible only with
	  1.5 VMs.

  1.6     Generate class files that are compatible only with
	  1.6 VMs.

  1.7     Generate class files that are compatible only with
	  1.7 VMs.

  1.8     Generate class files that are compatible only with
	  1.8 VMs.

Select \"default\" to use the source features that the compiler
supports by default, i.e., to not include the -target switch on
the compiler command line.

By default, classes are compiled against the bootstrap and extension classes
of the JDK that javac shipped with. But javac also supports cross-compiling,
where classes are compiled against a bootstrap and extension classes of a
different Java platform implementation. It is important to use
`jdee-compile-option-bootclasspath' and `jdee-compile-option-extdirs' when
cross-compiling."
  :group 'jdee-compile-options
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Target VM:"
	   (const "default")
	   (const "1.1")
	   (const "1.2")
	   (const "1.3")
	   (const "1.4")
	   (const "1.5")
	   (const "1.6")
	   (const "1.7")
	   (const "1.8"))))

(defcustom jdee-compile-option-bootclasspath nil
"*Cross-compile against the specified set of boot classes.
As with the user class path, boot class path entries can be
directories, JAR archives, or ZIP archives."
  :group 'jdee-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jdee-compile-option-extdirs nil
"*Cross-compile against the specified extension directories.
Each JAR archive in the specified directories is searched for class files."
  :group 'jdee-compile-options
  :type '(repeat (file :tag "Path")))

;;(makunbound 'jdee-compile-option-verbose-path)
(defcustom jdee-compile-option-verbose-path nil
"*Describe how paths and standard extensions were searched to find
source and class files.

   ***NOTE***

   This option is supported only by the versions of javac shipped
   with JDK 1.1.x and 1.2.x and oldjavac in JDK 1.3."

  :group 'jdee-compile-options
  :type 'boolean)

(defcustom jdee-compile-enable-kill-buffer -1
  "* Time in seconds to display the compilation buffer before
`jdee-compile-finish-kill-buffer' will kill the compilation buffer.

If less than zero (or nil), do not kill the compilation buffer.
If t (or other non-nil non-number) then kill in 2 secs."
  :group 'jdee-compile-options
  :type 'number)

(defun jdee-compile-show-options-buffer ()
  "Show the JDE Compile Options panel."
  (interactive)
  (customize-apropos "jdee-compile-options" 'groups))

(defclass jdee-compile-server-buffer (bsh-compilation-buffer) ()
  "Compiler server buffer.")

(defmethod bsh-compilation-buffer-create-native-buffer ((this jdee-compile-server-buffer))
  "Creates the native Emacs buffer for the JDEE compile server."
  (oset this buffer-name "*JDEE Compile Server*")
  (oset this buffer (get-buffer-create (oref this buffer-name))))

(defclass jdee-compile-exec-buffer (bsh-compilation-buffer) ()
  "Compiler exec buffer.")

(defmethod initialize-instance ((this jdee-compile-exec-buffer) &rest fields)
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

(defmethod bsh-compilation-buffer-create-native-buffer ((this jdee-compile-exec-buffer))
  "Creates the native Emacs buffer for the JDEE compile server."
  (oset this buffer-name "*compilation*")
  (oset this buffer (get-buffer-create (oref this buffer-name))))


(defclass jdee-compile-compiler ()
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
                     :initform nil
		     :type list
		     :documentation
		     "Arguments entered in the minibuffer.")
   (use-server-p     :initarg :use-server-p
		     :type boolean
		     :documentation
		     "Run as a compile server in the Beanshell."))
  "Class of Java compilers.")

(defmethod jdee-compile-classpath-arg ((this jdee-compile-compiler))
  "Returns the classpath argument for this compiler."
  (let ((classpath
	 (if jdee-compile-option-classpath
	     jdee-compile-option-classpath
	   (jdee-get-global-classpath)))
	(symbol
	 (if jdee-compile-option-classpath
	     'jdee-compile-option-classpath
	   'jdee-global-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jdee-build-classpath
	  classpath
	  symbol)
	 ))))

(defmethod jdee-compile-sourcepath-arg ((this jdee-compile-compiler))
  "Get the source path argument for this compiler."
    (if jdee-compile-option-sourcepath
	(list
	 "-sourcepath"
	 (jdee-build-classpath
	  jdee-compile-option-sourcepath
	  'jdee-compile-option-sourcepath))))

(defmethod jdee-compile-bootclasspath-arg ((this jdee-compile-compiler))
  "Get the boot classpath argument for this compiler."
  (if jdee-compile-option-bootclasspath
      (list
       "-bootclasspath"
       (jdee-build-classpath jdee-compile-option-bootclasspath
			    'jdee-compile-option-bootclasspath))))

(defmethod jdee-compile-extdirs-arg ((this jdee-compile-compiler))
  "Get the extdirs argument for this compiler."
  (if jdee-compile-option-extdirs
      (list
       "-extdirs"
       (jdee-build-classpath
	jdee-compile-option-extdirs
	'jdee-compile-option-extdirs))))


(defmethod jdee-compile-encoding-arg ((this jdee-compile-compiler))
  (if (not (string= jdee-compile-option-encoding ""))
      (list
       "-encoding"
       jdee-compile-option-encoding)))

(defmethod jdee-compile-debug-arg ((this jdee-compile-compiler))
  "Get the debug arg for this compiler."
  (let* ((include-option (nth 0 jdee-compile-option-debug))
	 (selected (nth 1 jdee-compile-option-debug))
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

(defmethod jdee-compile-output-dir-arg ((this jdee-compile-compiler))
  "Get the ouput directory arg for this compiler."
    (if (not (string= jdee-compile-option-directory ""))
	(list
	 "-d"
	 (jdee-normalize-path 'jdee-compile-option-directory))))

(defmethod jdee-compile-deprecation-arg ((this jdee-compile-compiler))
  "Get deprecation argument for this compiler."
    (if jdee-compile-option-deprecation
	(list "-deprecation")))

(defmethod jdee-compile-optimize-arg ((this jdee-compile-compiler))
  "Get optimization argument for this compiler."
    (if jdee-compile-option-optimize
	(list "-O")))

(defmethod jdee-compile-depend-arg ((this jdee-compile-compiler))
  "Get dependency-checking argument for this compiler."
  (if jdee-compile-option-depend
    (list (car jdee-compile-option-depend-switch))))

(defmethod jdee-compile-vm-args ((this jdee-compile-compiler))
  "Get arguments to pass to the vm used to run this compiler."
    (if jdee-compile-option-vm-args
	(cl-mapcan
	 (lambda (arg)
	   (list (concat "-J" arg)))
	 jdee-compile-option-vm-args)))

(defmethod jdee-compile-verbose-arg ((this jdee-compile-compiler))
  "Get verbosity level argument for this compiler."
    (if jdee-compile-option-verbose
	(list "-verbose")))

(defmethod jdee-compile-verbose-path-arg ((this jdee-compile-compiler))
  "Get verbose path argument for this compiler."
    (if jdee-compile-option-verbose-path
	(list "-Xverbosepath")))

(defmethod jdee-compile-nowarn-arg ((this jdee-compile-compiler))
  "Get no warning argument for this compiler."
    (if jdee-compile-option-nowarn
	(list "-nowarn")))

(defmethod jdee-compile-command-line-args ((this jdee-compile-compiler))
  "Get additional command line arguments for this compiler."
	jdee-compile-option-command-line-args)

(defmethod jdee-compile-target-arg ((this jdee-compile-compiler))
  "Get compiler target argument for this compiler."
    (let ((target (car jdee-compile-option-target)))
      (if (not (string= target "default"))
	  (list "-target" target))))

(defmethod jdee-compile-source-arg ((this jdee-compile-compiler))
  "Get compiler source argument for this compiler."
  (let ((source (car jdee-compile-option-source)))
    (if (not (string= source "default"))
	(list "-source" source))))

(defmethod jdee-compile-get-args ((this jdee-compile-compiler))
  (append
   (jdee-compile-classpath-arg this)
   (jdee-compile-sourcepath-arg this)
   (jdee-compile-bootclasspath-arg this)
   (jdee-compile-extdirs-arg this)
   (jdee-compile-encoding-arg this)
   (jdee-compile-debug-arg this)
   (jdee-compile-output-dir-arg this)
   (jdee-compile-deprecation-arg this)
   (jdee-compile-optimize-arg this)
   (jdee-compile-depend-arg this)
   (jdee-compile-vm-args this)
   (jdee-compile-verbose-arg this)
   (jdee-compile-verbose-path-arg this)
   (jdee-compile-nowarn-arg this)
   (jdee-compile-target-arg this)
   (jdee-compile-source-arg this)
   (jdee-compile-command-line-args this)))


(defmethod jdee-compile-run-exec ((this jdee-compile-compiler))
  (let* ((outbuf (oref (oref this buffer) buffer))
	 (compiler-path (oref this :path))
	 (source-file (file-name-nondirectory buffer-file-name))
	 (flag nil)
	 (args (append
		(jdee-compile-get-args this)
		(oref this :interactive-args)
		(list source-file))))

    (with-current-buffer outbuf

      (let ((inhibit-read-only t)) ; make compilation buffer temporarily writable
	(insert (format "cd %s\n" default-directory))
	(insert (concat
                 compiler-path
                 " "
                 (mapconcat (lambda (x)
                              (if (and flag
                                       jdee-compile-option-hide-classpath)
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

;;; TODO: extract code duplicated with EJC version
(defmethod jdee-compile-run-server ((this jdee-compile-compiler))
  (let* ((directory-sep-char ?/)
         (args
          (append
           (jdee-compile-get-args this)))
         (source-path
          (jdee-normalize-path buffer-file-name))
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

    (with-current-buffer (oref (oref this buffer) buffer)

      (insert "CompileServer output:\n\n")

      (let* ((inhibit-read-only t)
             flag
             (arg-string
              (mapconcat
               (lambda (x)
                 (if (and flag
                          jdee-compile-option-hide-classpath)
                     (progn
                       (setq flag nil)
                       "...")
                   (if (not (string= x "-classpath"))
                       x
                     (progn
                       (setq flag t)
                       x))))
               args " ")))

        (insert arg-string " " source-path "\n")))

    (jdee-backend-compile arg-array (oref this buffer))))

(defmethod jdee-compile-launch ((this jdee-compile-compiler))

  (if (oref this :use-server-p)
      (jdee-compile-run-server this)
    (jdee-compile-run-exec this))

  (set-buffer-modified-p nil))

(defmethod jdee-compile-compile ((this jdee-compile-compiler))

  (if (oref this :use-server-p)
      (oset this buffer (jdee-compile-server-buffer "compilation buffer"))
    (oset this buffer (jdee-compile-exec-buffer "compilation buffer")))


  ;; Pop to compilation buffer.
  (let* ((outbuf (oref (oref this buffer) buffer))
         (outwin (display-buffer outbuf)))
    (compilation-set-window-height outwin)
    (oset this :window outwin)

    (if compilation-process-setup-function
        (funcall compilation-process-setup-function))

    (jdee-compile-launch this)

    (setq compilation-last-buffer outbuf)))


(defclass jdee-compile-javac (jdee-compile-compiler)
  ()
  "Class of javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac) &rest fields)
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
(defclass jdee-compile-javac-11 (jdee-compile-compiler)
  ()
  "Class of JDK 1.1 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-11) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.1"))

(defmethod jdee-compile-debug-arg ((this jdee-compile-javac-11))
  "Get the debug arg for this compiler."
   (let ((include-option (nth 0 jdee-compile-option-debug)))
     (cond
      ((string= include-option "all")
       (list "-g"))
      ((string= include-option "selected")
       (error "JDK 1.1 version of javac does not support selected debug info.")))))

(defmethod jdee-compile-depend-arg ((this jdee-compile-javac-11))
  "Get dependency-checking argument for this compiler."
  (if jdee-compile-option-depend
    (list "-depend")))

(defmethod jdee-compile-get-args ((this jdee-compile-javac-11))
  (append
   (jdee-compile-classpath-arg this)
   (jdee-compile-encoding-arg this)
   (jdee-compile-debug-arg this)
   (jdee-compile-output-dir-arg this)
   (jdee-compile-deprecation-arg this)
   (jdee-compile-optimize-arg this)
   (jdee-compile-depend-arg this)
   (jdee-compile-vm-args this)
   (jdee-compile-verbose-arg this)
   (jdee-compile-nowarn-arg this)
   (jdee-compile-command-line-args this)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.2 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-javac-12 (jdee-compile-compiler)
  ()
  "Class of JDK 1.2 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-12) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.2"))

(defmethod jdee-compile-depend-arg ((this jdee-compile-javac-12))
  "Get dependency-checking argument for this compiler."
  (if jdee-compile-option-depend
    (list "-Xdepend")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.3 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-javac-13 (jdee-compile-javac-12)
  ()
  "Class of JDK 1.3 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-13) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.3"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.4 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-javac-14 (jdee-compile-javac-13)
  ()
  "Class of JDK 1.4 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-14) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.4"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; J2SDK 1.5 Compiler                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-javac-15 (jdee-compile-javac-14)
  ()
  "Class of J2SDK 1.5 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-15) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.5"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; J2SDK 1.6 Compiler                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-javac-16 (jdee-compile-javac-15)
  ()
  "Class of JDK 1.6 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-16) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.6"))

(defmethod jdee-compile-annotation-processors-arg ((this jdee-compile-javac-16))
  "Get the annotation processors argument for this compiler."
  (if jdee-compile-option-annotation-processors
    (list
     "-processor"
     (mapconcat 'identity
		jdee-compile-option-annotation-processors
		","))))

(defmethod jdee-compile-annotation-processing-arg ((this jdee-compile-javac-16))
  (let ((option (car jdee-compile-option-annotation-processing)))
    (cond
     ((string= option "compile and process annotations")
      nil)
     ((string= option "compile only")
      (list "-proc:none"))
     ((string= option "process annotations only")
      (list "-proc:only")))))

(defmethod jdee-compile-annotation-processor-options-args ((this jdee-compile-javac-16))
    "Get property arguments."
    (mapcar
     (lambda (option)
       (let ((key (car option))
	     (value (cdr option)))
	 (if (string= value "")
	     (format "-A%s" key)
	   (format "-A%s=%s" key value))))
     jdee-compile-option-annotation-processor-options))

(defmethod jdee-compile-implicit-arg ((this jdee-compile-javac-16))
  (let ((option (car jdee-compile-option-implicit)))
    (cond
     ((string= option "Generate and warn")
      nil)
     ((string= option "Generate and do not warn")
      (list "-implicit:class"))
     ((string= option "Do not generate")
      (list "-implicit:none")))))

(defmethod jdee-compile-get-args ((this jdee-compile-javac-16))
  (append
   (jdee-compile-classpath-arg this)
   (jdee-compile-sourcepath-arg this)
   (jdee-compile-bootclasspath-arg this)
   (jdee-compile-extdirs-arg this)
   (jdee-compile-encoding-arg this)
   (jdee-compile-debug-arg this)
   (jdee-compile-output-dir-arg this)
   (jdee-compile-deprecation-arg this)
   (jdee-compile-optimize-arg this)
   (jdee-compile-depend-arg this)
   (jdee-compile-annotation-processing-arg this)
   (jdee-compile-annotation-processors-arg this)
   (jdee-compile-annotation-processor-options-args this)
   (jdee-compile-implicit-arg this)
   (jdee-compile-vm-args this)
   (jdee-compile-verbose-arg this)
   (jdee-compile-nowarn-arg this)
   (jdee-compile-target-arg this)
   (jdee-compile-source-arg this)
   (jdee-compile-command-line-args this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; J2SDK 1.7 Compiler                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-javac-17 (jdee-compile-javac-16)
  ()
  "Class of JDK 1.7 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-17) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.7"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; J2SDK 1.8 Compiler                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-javac-18 (jdee-compile-javac-17)
  ()
  "Class of JDK 1.7 javac compilers.")

(defmethod initialize-instance ((this jdee-compile-javac-18) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Eclipse Compiler                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-compile-ejc-server (jdee-compile-compiler)
  ()
  "Class for using the Eclipse java compiler as a JDEE compile server."
)

(defmethod jdee-compile-run-server ((this jdee-compile-ejc-server))
  (let* ((directory-sep-char ?/)
         (args
          (append
           (list
            "-Xemacs"
            "-noExit"
            ;;               "-sourcepath"
            ;;               (mapconcat 'identity (jdee-expand-wildcards-and-normalize jdee-sourcepath) ":")
            )
           (jdee-compile-get-args this)))
         (source-path
          (jdee-normalize-path buffer-file-name))
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

    (with-current-buffer (oref (oref this buffer) buffer)

      (insert "CompileServer output:\n\n")

      (let (flag temp)
        (setq temp
              (mapconcat
               (lambda (x)
                 (if (and flag
                          jdee-compile-option-hide-classpath)
                     (progn
                       (setq flag nil)
                       "...")
                   (if (not (string= x "-classpath"))
                       x
                     (progn
                       (setq flag t)
                       x)))) args " "))

        (insert temp " "))
      (insert source-path "\n"))

    (jdee-backend-compile-eclipse (oref this :path) arg-array (oref this buffer))))

(defvar jdee-compile-javac-compilers
  (list
   (jdee-compile-javac-11 "javac 1.1.x")
   (jdee-compile-javac-12 "javac 1.2.x")
   (jdee-compile-javac-13 "javac 1.3.x")
   (jdee-compile-javac-14 "javac 1.4.x")
   (jdee-compile-javac-15 "javac 1.5.x")
   (jdee-compile-javac-16 "javac 1.6.x")
   (jdee-compile-javac-17 "javac 1.7.x")
   (jdee-compile-javac-18 "javac 1.8.x"))
  "List of supported javac compilers.")

(defun jdee-compile-get-javac ()
  (let* ((jdk-version (jdee-java-version))
	 (jdk-split-version (split-string jdk-version "[.]"))
	 (jdk-major-version (nth 0 jdk-split-version))
	 (jdk-minor-version (nth 1 jdk-split-version))
	 (compiler
	  (cl-find-if
	   (lambda (compiler-x)
	     (let* ((compiler-split-version (split-string (oref compiler-x :version) "[.]"))
		    (compiler-major-version (nth 0 compiler-split-version))
		    (compiler-minor-version (nth 1 compiler-split-version)))
	       (and
		(string= jdk-major-version compiler-major-version)
		(string= jdk-minor-version compiler-minor-version))))
	   jdee-compile-javac-compilers)))
    (unless compiler
      (let ((latest-javac (car (last jdee-compile-javac-compilers))))
	(if
	    (yes-or-no-p
	     (format "The JDE does not recognize JDK %s javac. Assume JDK %s javac?"
		     jdk-version (oref latest-javac :version)))
	    (setq compiler latest-javac))))
    (if compiler
	(if (string= (car jdee-compiler) "javac server")
	    (oset compiler :use-server-p t)
	  (progn
	    (oset compiler :use-server-p nil)
	    (oset compiler
		  :path
		  (let ((compiler-path
                         (if (listp (car jdee-compiler))
                             (substitute-in-file-name (nth 1 (car jdee-compiler)))
                           "")))
		    (if (string= compiler-path "")
			(setq compiler-path (jdee-get-jdk-prog 'javac))
		      (if (file-exists-p compiler-path)
			  compiler-path
			(error (format "Invalid compiler path %s"
				       compiler-path)))))))))
    compiler))

(defun jdee-compile-get-ejc ()
  "Get the ejc compiler object."
  (let ((compiler-path
         (substitute-in-file-name (nth 1 (car jdee-compiler)))))

    (if (string= compiler-path "")
        (error "Cannot find jdt core jar"))
    (jdee-compile-ejc-server
     "Eclipse java compiler server"
     :use-server-p t
     :path compiler-path)))

(defun jdee-compile-get-the-compiler ()
  "Get a compiler object that represents the compiler specified
by `jdee-compiler'."
  (let* ((car-jdee-compiler (car jdee-compiler))
         (compiler-name (if (listp car-jdee-compiler) (car car-jdee-compiler) car-jdee-compiler)))
    (cond
     ((string-match "javac" compiler-name)
       (jdee-compile-get-javac))
     ((string-match "eclipse java compiler server" compiler-name)
      (jdee-compile-get-ejc))
     (t
      (error "The JDEE does not support a compiler named %s" compiler-name)))))


;;;###autoload
(defun jdee-set-compile-options (options)
  "Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose."
  (interactive
   "sEnter options: ")
  (setq jdee-compile-option-command-line-args (split-string options " ")))

;;;###autoload
(defun jdee-compile ()
  "Compile the Java program in the current buffer.
This command invokes the compiler specified by `jdee-compiler'
with the options specified by the JDE customization variables
that begin with `jdee-compile'. If the variable
`jdee-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled. If `jdee-compiler' specifies the JDE compile
server, this command uses the compile server. Otherwise, it
uses the compiler executable specified by
`jdee-compiler' to compile."
  (interactive)

  (if jdee-read-compile-args
      (setq jdee-interactive-compile-args
            (read-from-minibuffer
             "Compile args: "
             jdee-interactive-compile-args
             nil nil
             '(jdee-interactive-compile-arg-history . 1))))

  ;; Force save-some-buffers to use the minibuffer
  ;; to query user about whether to save modified buffers.
  ;; Otherwise, when user invokes jdee-compile from
  ;; menu, save-some-buffers tries to popup a menu
  ;; which seems not to be supported--at least on
  ;; the PC.
  (if (eq system-type 'windows-nt)
      (let ((temp last-nonmenu-event))
        ;; The next line makes emacs think that jdee-compile
        ;; was invoked from the minibuffer, even when it
        ;; is actually invoked from the menu-bar.
        (setq last-nonmenu-event t)
        (save-some-buffers (not compilation-ask-about-save) nil)
        (setq last-nonmenu-event temp))
    (save-some-buffers (not compilation-ask-about-save) nil))

  (setq compilation-finish-functions
        (lambda (buf msg)
          (run-hook-with-args 'jdee-compile-finish-hook buf msg)
          (setq compilation-finish-functions nil)))

  (let ((compiler (jdee-compile-get-the-compiler)))
    (if compiler
        (progn
          (oset compiler
                :interactive-args
                (if (and jdee-interactive-compile-args
                         (not (string= jdee-interactive-compile-args "")))
                    (split-string jdee-interactive-compile-args " ")))
          (jdee-compile-compile compiler))
      (error "Unknown compiler. Aborting compilation."))))

(provide 'jdee-compile)

;;; jdee-compile.el ends here
