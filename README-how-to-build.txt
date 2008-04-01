
This file describes how to build JDEE.
Author: Dimitre Liotev (dl@znain.net).

In order to build the Java and Elisp code you must have Ant
(ant.apache.org) installed on your system.

To compile the Elisp code you must set two environment variables:
CEDET_HOME and ELIB_HOME, which should point to the installations
directories of the Cedet and Elib packages.

Note that if you do not have the "emacs" command in your PATH, you must
set a variable EMACS_COMMAND in your environment. If this variable is
not set the command "emacs" will be used by default.

To build all code just invoke "ant": 

To compile only the Java code invoke "ant java". This will run Ant on
the build.xml file in the "java" directory. If you want to work only
with the java code you can go to the "java" directory and invoke Ant on
the build.xml file there. 

To compile only the Elisp code invoke "ant lisp".

To create a JDEE distribution directory: "and dist". To create a JDEE
zip file: "ant zip". To clean all: "ant clean"

Look in the build.xml file to see the other targets that you can invoke
with Ant.

The Elisp code can also be compiled by simply invoking this command:

emacs --script build.el

This is what Ant does too - it will invoke this command if any of the
*.elc files in the "lisp" directory are not up to date - in other words
if any *.elc file is older than the corresponding *.el file.
