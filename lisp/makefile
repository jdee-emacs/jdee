# $Revision: 1.17 $ $Date: 2004/08/10 04:00:26 $ 
#
# This makefile compiles the JDE lisp files.
#
# Adapted by Paul Kinnucan from a Windows/NT batch script 
# created by Steve Rowley <sgr@alum.mit.edu>
#

EMACS = emacs

# Default locations of the packages required to compile the JDE.
CEDET = ../../cedet
ELIB  = ../../elib
all:
	test -d $(CEDET) -a -d $(ELIB)
	rm -f *.elc jde-compile-script-init
	echo "(add-to-list 'load-path \".\")" > jde-compile-script-init
	echo "(add-to-list 'load-path \"$(ELIB)\")" >>jde-compile-script-init
	echo "(setq debug-on-error t)" >> jde-compile-script-init
	echo "(load-file \"$(CEDET)/common/cedet.el\")" >> jde-compile-script-init
	echo "(require 'jde-compat)" >> jde-compile-script-init
	echo "(require 'jde)" >>jde-compile-script-init
	if echo "$(EMACS)" | grep -w xemacs ; \
	  then $(EMACS) -batch -l jde-compile-script-init -f batch-byte-compile `echo *.el` ; \
	  else $(EMACS) -batch -l jde-compile-script-init \
	    -f batch-byte-compile `ls -1 *.el | egrep -v '.*-xemacs.el'`; \
	fi;
	if test -f jde-compile-script-init; then rm -f jde-compile-script-init; fi;


# 
# $Log: makefile,v $
# Revision 1.17  2004/08/10 04:00:26  paulk
# Compatibility fix to allow makefile to work on Sun systems. Thanks to Ed Mooney.
#
# Revision 1.16  2004/07/06 02:59:57  paulk
# Change makefile to not byte-compile any file whose name ends in -xemacs.el, e.g., jde-xemacs.el and efc-xemacs.el.
#
# Revision 1.15  2004/06/02 04:43:28  paulk
# Updated makefile for compiling the JDEE Lisp code to support cedet.
#
# Revision 1.14  2003/06/24 12:48:26  paulk
# Conditionalize to compile jde-xemacs.el only for XEmacs. Thanks to Raul Acevedo.
#
# Revision 1.13  2002/08/07 06:36:14  paulk
# Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
# removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
# for suggesting this restructuring. Also fixed a number of compiler warnings caused
# by genuine bugs.
#
# Revision 1.12  2001/11/14 06:26:13  paulk
# Now requires jde to make sure the JDE is loaded before compiling.
#
# Revision 1.11  2001/11/14 05:29:26  paulk
# Remove yet another mysterious added #.
#
# Revision 1.10  2001/11/14 05:27:41  paulk
# Remove comment characters.
#
# Revision 1.9  2001/11/14 05:24:31  paulk
# Use variables to specify location of required packages. Check for existence of required packages.
# 
# 

# End of makefile