Welcome to the Java IDE for Emacs, an Emacs-based integrated development
environment for developing Java applications. Features include:

  * source code editing with syntax highlighting and
    auto indendation

  * compilation with automatic jump from error messages
    to responsible line in the source code.

  * run Java application in an interactive (comint)
    Emacs buffer

  * integrated debugging with interactive debug
    command buffer and automatic display of
    current source file/line when stepping through code

  * browse JDK doc, using the browser of your choice

  * browse your source code, using the Emacs etags
    facility or a tree-structured speedbar.

  * supports latest version of JavaSoft's Java Development
    Kit

  * runs on any platform supported by Emacs and Sun's
    Java SDK (e.g., Win95/NT and Solaris)

  * easily and infinitely customizable

  * works with FSF Emacs and XEmacs

The IDE integrates several Emacs program
development packages (cc-mode, gud, and
compile) with JavaSoft's JDK.

This package includes:

   -- jde.el

   -- jde-run.el

   -- jde-db.el 

   -- speedbar.el

   -- shell script that tags java files,
      using the etags program. This script
      comes in two versions, one for 
      csh and the other for bash shells.

It requires:

   -- latest version of cc-mode.el

      (available from
       ftp://ftp.python.org/pub/emacs/cc-mode.tar.gz)

   -- andersl-java-font-lock.el

      (available from
       http:\\www.csd.uu.se\~andersl\emacs.shtml
       ftp:\\ftp.csd.uu.se\pub\users\andersl\emacs\)

      This is required for syntax highlighting.

   -- Java Development Kit (jdk) 
     
      (available from
        http://www.javasoft.com/products/jdk/1.1/index.html)

   -- Web browser

      (e.g., Netscape or Internet Explorer)

   -- Unix-style shell for Windows 95 or Windows/NT

      I recommend bash, available from www.cygnus.com.

To install this package:

1. Copy jde.el, jde-db.el, jde-run.el, and speedbar.el files
   to a directory in your emacs lisp file load
   path.

2. Byte-compile these files if you like.

3. Add the following code to your .emacs file:

   (load "jde")
   (setq jde-web-browser "BROWSER")
   (setq jde-doc-dir "JDK DIRECTORY")
   (jde-db-set-source-paths "SRCPTH1;SRCPTH2;...)

For example, suppose that Netscape is your
browser and is in your shell command path
and you have installed the JDK API doc at
c:/JDK1.1/docs. Then, you would add

   (load "jde")
   (setq jde-web-browser "netscape")
   (setq jde-doc-dir "c:/jdk1.1/docs/")
   (jde-db-set-source-paths "c:/jdk1.1/src/;c:/myjava/src/")

to your .emacs file.

If you are using the bash shell, you should 
specify this either by setting the SHELL
environment variable or the elisp variable
shell-file-name to bash. For example, for
Win95, enter
   
   set shell=bash

in your autoexec.bat file or

   (setq shell-file-name "bash")

in your .emacs file.

If you are using bash on Windows 95 or Windows/NT, you should
also put 

   (setq shell-command-switch "-c")

in your .emacs file.

To use the Java IDE, load the Java source file that
you wish to edit, then choose the appropriate options
from the Java menu in the Emacs menu bar.

COMPILING JAVA PROGRAMS

Choose "Compile" from the Java menu to compile the source
file in the current buffer.

Choose "Compile Options" to set javac compile options, such as,
-depend -verbose. The JDE displays a prompt in the minibuffer.
Enter the desired compile options as you would on the command
line and press enter. The JDE uses the specified command options
whenever you choose "Compile" from the "Java" menu. To specify
no options, simply press return at the prompt.

SETTING THE CLASSPATH

To set the classpath for both compiling and running Java
applications, enter

  M-x jde-set-classpath

at the command line and enter the classpath, using
the appropriate separator character (colon for Unix platforms,
semicolon for Windows). For example,

   .;d:jdk1.1.3/lib/classes.zip

RUNNING AN APPLICATION

Choose "Run App" To run an application. The JDE runs either
the class previously specified with the jde-run-set-app
command (see below) or the class file corresponding to the 
source file in the current buffer. In the latter case,
the JDE determines the class's package from the source
in the current buffer.

SPECIFYING THE APP MAIN CLASS

Execute M-x jde-run-set-app to specify your application's
main class (the class with the main() method). Don't forget
to qualify the class name if the main class is a member of
a package. 

SPECIFYING A JAVA INTERPRETER

By default, the JDE uses the JavaSoft JDK interpreter
(java for Unix platforms, javaw for Windows platforms) to
run applications. You can specify another interpreter
by entering

  M-x jde-run-set-vm

for Unix plattforms or

  M-x jde-run-set-vm-w

for Windows platforms.

SPECIFYING RUN OPTIONS

To specify run options supported by the Java interpreter,
enter

  M-x jde-run-set-args 

and enter the options as you would on the command line,
e.g.,

  -verbose -mx24m

Note: do not enter a -classpath option if you specified the
classpath via the jde-set-classpath command. The jde-set-classpath
command sets the classpath option for both compiling and
running applications.

RUNNING AN APPLET

Use Emacs' shell mode to run applets.

DEBUGGING AN APPLICATION

To debug an application:

1. Use jde-db-set-source-paths to specify  the
   paths of any source code that you expect
   visit while debugging your application (see
   below).

2. Specify the app's main class either by running the
   jde-run-set-app command (you need only do this
   once) or by switching to the buffer that contains
   the source file for the main class.
 
The JDE starts the debugger and opens a debugger
interaction window. Open the source file where
you want to set your first break and position
the point in the line where you want to set
the breakpoint and enter C-x C-a C-b. This
sets the breakpoint. Then go to the debugger
window and at the debugger prompt (> ), type the
run command. The debugger will run your app,
stopping at the initial breakpoint. The JDE displays
the source file containing the line at which
your program has stopped. An arrow (=>) points
to the current line. The menu bar of the source
window displays a menu (Jdb) of debug commands.

You can step into/over through your source, using
any of the following methods

  -- entering a jdb command (step or next) in
     the jdb interaction buffer

  -- typing an appropriate gud-mode key combination

  -- choosing Step or Next from the Jdb menu in
     the source window

For a list of jdb commands, type help at the
jdb command line. For a list of gud-mode key bindings,
see below. Note that you can step, set/clear breakpoints, 
etc, from either the debugger window or from a source window.
For example, to step from a source window,
type C-x C-a C-s or select step from the
Jdb menu. To step from the debugger
window, type C-c C-s.

SPECIFYING SOURCE CODE PATHS

Type M-x jde-db-set-source-paths to specify the source paths
of files you might visit while debugging your app. The JDE
displays a prompt in the minibuffer. Enter the source paths
separated by colons (on Unix systems) or semicolons (on Win32
systems) and press enter.

You must specify the paths of the top-level directories
of any source code that you might visit while debugging
your application. The source code directory structure
must mirror your app's package structure. For example,
suppose that your app includes a set of classes packaged
in the foo directory. Then, the source for those classes
must be reside in a directory named foo and you must
specify the path of foo's parent directory.

TIP:

If you want to step through the JDK source code, select
the source code install option when you install the JDK
and the use jde-db-set-source-paths to specify the
directory containing the source code. You need specify
only the path of the top-level directory. The JDE will
use the JDK's package structure to find the source code
in the subdirectories.

TIP:

Specify source paths in your .emacs file to avoid having
to set them every time you start a session. For example,
entering 

(jde-db-set-source-paths "c:/jdk/src/;c:/java_projects/")

in your .emacs file tells the JDE to look for source files
in the specified directories when stepping through your
applications.

SAVING AND RESTORING PROJECT SETTINGs

If you are working on more than one project, each with its
own source path, class path, and compile options, you can
save time by storing the settings for each project in .el
files and storing each project file in the directory that
contains that file. You can then restore the settings for
a particular project by type M-x load-file.

For example, here is a typical project file:

;; prj.el

;; Class path setting
(setenv "CLASSPATH" ".;c:/prj1/src;c:/prj1/classes;c:/jdk/lib/classes.zip")
;; Compile options
(jde-set-compile-options "-d \"c:/prj1/classes\" -debug")
;; Application's main class
(jde-run-set-app "foo.main")
;; Source paths
(jde-db-set-source-paths "c:/prj1/src;c:/jdk/src")

BROWSING JDK DOCUMENTATION

To view the JDK doc, choose Browse Doc from
the Java menu.

BROWSING YOUR SOURCE CODE

You can use Emacs' etags facility or the speedbar to browse your 
Java source code.

To use the speedbar, select Speedbar from the JDE
menu. The speedbar opens in a separate window, showing
a tree-structure list of the Java source files in the 
current directory. Click on the + button in front of
any file. The node for the file expands, showing 
the methods, class and instance variables, and
classes defined in that file as children of the
file node. Click on any method, variable, or class
name to see its definition.

To use the etags facility, you must
first construct a TAGS file that indexes every symbol
in your source code. The JDE package contains two shell
scripts that you can use to tag your source code, one
for csh shells and the other for bash. The bash version
is called jtags; the csh version, jtags.csh. 

To tag your source code, first copy the appropriate
shell script to a directory in your Emacs path. Then
start a shell (M-x shell). Change to the top-level
directory containing your source code and then
enter jtags. The jtags script will tag every .java
file in the current directory and in all descendants
of the current directory, storing the result in a 
file called TAGS in the top-level directory.

To find the definition of a symbol, put your cursor
anywhere in the symbol and enter M-x . Emacs 
responds by locating and opening (if necessary) the
file containing the definition and positioning the
point at the definition. (The first time you type
M-x ., Emacs prompts you to load the TAGS file.)

Please send bug reports and enhancement suggestions
to Paul Landes <landes <at> mailc dt net>

Enjoy

Paul Kinnucan

Doc for jdb version of gud-mode
========================================

(Note: I hope to incorporate the following into the mode info in a future release.)

Major mode for interacting with the JavaSoft debugger jdb.

You start it up with the commands M-x jdb.  It finishes by executing a
hook; `gud-mode-hook'.

After startup, the following commands are available in both the JDB
interaction buffer and any source buffer JDB visits due to a breakpoint stop
or step operation:

\\[gud-break] sets a breakpoint at the current file and line.  In the
JDB buffer, the current file and line are those of the last breakpoint or
step.  In a source buffer, they are the buffer's file and current line.

\\[gud-remove] removes breakpoints on the current file and line.

\\[gud-refresh] displays in the source window the last line referred to
in the gud buffer.

\\[gud-step] and \\[gud-next] do a step-one-line and a
step-one-line (not entering function calls), respectively,
and then update the source window with the current file and position.
\\[gud-cont] continues execution.

\\[gud-print] executes the toString method of the class in the current
source file buffer.

\\[gud-up] pops up through an enclosing stack
frame.  \\[gud-down] drops back down through one.

All the keystrokes above are accessible in the JDB buffer
with the prefix C-c, and in all buffers through the prefix C-x C-a.

All pre-defined functions for which the concept make sense repeat
themselves the appropriate number of times if you give a prefix
argument.

You may use the `jdb-def' macro in the initialization hook to define other
commands.

Other commands for interacting with the debugger process are inherited from
comint mode, which see.

$Id: readme.txt,v 1.2 1997/08/26 09:31:34 kinnucan Exp $
