#! /bin/csh
#
# $Id: jtags.csh,v 1.7 2002/09/16 04:55:27 paulk Exp $
#
# Usage:     jtags [-h | srcdir]
#
# Abstract:  This C shell script produces an Emacs
#            tags file for Java source code. The tags file
#            contains tags for classes, interfaces,
#            constructors, methods, and variables.
#
# Options:   -h  Print usage.
#
#            -srcdir  Path of top-level directory containing 
#                     Java source(*.java) files to be tagged.
#                     If omitted, this script tags files in
#                     the working directory and its subdirectories.
#
# By:        Paul Kinnucan 
#            The MathWorks, Inc.
#	     paulk@mathworks.com
#
# Thanks:    David Lim <david@ndc.com>
#            Michael Mirman <mmirman@mathworks.com>
#            Kent S. Gordon" <kgor@inetspace.com>
#


set etags_dir = ""
set java_dir = .

# Process command line parameters.
foreach arg ($*)
    switch ($arg) 
	case -h:
	case -help:
        case -usage:
	    set help = 1
	    breaksw
	case -e*:
	    set etags_dir = `expr substr $arg 3 200`
	    breaksw
	case -j*:
	    set java_dir = `expr substr $arg 3 200`
	    breaksw
	default:
	    set invalid_arg = 1
	    breaksw
    endsw
end

# Print help.
if ( $1 == "-h" || $1 == "-help" || $1 == "-usage" ) then
    echo ""
    echo "usage: jtags [-h | srcdir]"
    echo ""
    echo "srcdir   Path of the Java source directory."
    echo ""
    echo "This command tags all public classes, interfaces,"
    echo "methods, and variables in the specified Java source"
    echo "hierarchy. If you do not specify a source directory," 
    echo "jtags searches the current directory and its"
    echo "subdirectories."
    echo ""
    exit 1
endif

# Ensure that etags directory exists.
if ( $etags_dir != "" ) then
    if (! -d $etags_dir ) then
	echo "etags directory $etags_dir not found."
	exit 1
    endif
endif

# If not the help option, assume parameter is the source path.
if ( $1 == "" ) then
    set java_dir = "."
else
    set java_dir = $1
endif

# Ensure that Java source directory exists.
if (! -e $java_dir ) then
    echo "Java source directory $java_dir not found."
    exit 1
endif

set capital = 'A-Z'
set letter = 'a-zA-Z_.'
set digit = '0-9'
set ws = '[ \t]'

set primitive_type1 = '\<\(b\(oolean\|yte\)\|char\|double\|float\|int'
set primitive_type2 = '\|long\|short\|void\)\>'
set primitive_type = "$primitive_type1$primitive_type2"
set primitive_type_count = 2

set primitive_type3 = '\|long\|short\)\>'
set primitive_type_no_void = "$primitive_type1$primitive_type3"
set primitive_type_no_void_count = 2

set identifier = "\<\([$letter][$letter$digit]*\)\>"
set identifier_count = 1

set class_type = "\<\([$capital][a-zA-Z_$digit\.]*\)\>"
set class_type_count = 1

set modifier1 = '\<\(abstract\|const\|final\|native\|'
set modifier2 = 'p\(r\(ivate\|otected\)\|ublic\)\|'
set modifier3 = 's\(tatic\|ynchronized\)\|transient\|volatile\)\>'
set modifier = "$modifier1$modifier2$modifier3"
set modifier_count = 4

# Class patterns
set class1 = "/^$ws*\<\(class\|interface\)\>$ws*$identifier/\2/"
set class2 = "/^[^.*\/]*\($modifier$ws*\)*\<\(class\|interface\)\>$ws*$identifier/\7/"

# Constructor pattern
set constructor = "/^$ws*\($modifier$ws*\)*$class_type$ws*(/\6/"

# Pattern for methods that return primitive types, e.g.,
#
#   int[] foo()
#
set method1 = "/^[^.*\/]*$primitive_type$ws*\(\[$ws*\]$ws*\)*$identifier$ws*(/\4/"

# Pattern for methods that return class types, e.g.,
#
#   Foo[] foo()
#
set method2 = "/^[^.*\/]*$class_type$ws*\(\[$ws*\]$ws*\)*$identifier$ws*(/\3/"

# Pattern for matching primitive variable declarations.
set var1a = ".*$primitive_type_no_void$ws*\(\[$ws*\]$ws*\)*$identifier"
set var1b = "$ws*\(=\|;\)"
set var1 = "/$var1a$var1b/\4/"

# Pattern for matching user-defined variable declarations.
set var2a = ".*$class_type$ws*\(\[$ws*\]$ws*\)*$identifier"
set var2b = "$ws*\(=\|;\)"
set var2 = "/$var2a$var2b/\3/"
  
# Delete the old TAGS file.
# Note: the old file must be deleted because we have to run
# etags in append mode. If we don't remove the old file, 
# etags will append everything to the old file.

if ( -e $java_dir/TAGS ) then
    rm $java_dir/TAGS
    echo "Removed old TAGS file."
endif

# Use find to recurse through the source hierarchy, 
# finding every java source file.
# Use xargs to apply etags to the source files.
# Note that xargs may invoke etags multiple
# times, depending on how many files it can batch
# per invocation. That is why we run etags in
# append (-a) mode.

echo "Tagging classes and constructors"
find $java_dir \( -name RCS -prune \) -o \( -name CVS -prune \) -o \( -name '*.java' -print \) | xargs  \
"${etags_dir}etags"  -l none -a -o ${java_dir}/TAGS  \
"--regex=$class1" "--regex=$class2"  "--regex=$constructor"  

echo "Tagging methods"
find $java_dir \( -name RCS -prune \) -o \( -name CVS -prune \) -o \( -name '*.java' -print \) | xargs  \
"${etags_dir}etags"  -l none -a -o ${java_dir}/TAGS  \
"--regex=$method1" "--regex=$method2"

echo "Tagging variables"
find $java_dir \( -name RCS -prune \) -o \( -name CVS -prune \) -o \( -name '*.java' -print \) | xargs  \
"${etags_dir}etags"  -l none -a -o ${java_dir}/TAGS  \
"--regex=$var1" "--regex=$var2"

# History:
#
# $Log: jtags.csh,v $
# Revision 1.7  2002/09/16 04:55:27  paulk
# Quote path of etags executable to permit inclusion of spaces on Windows. Thanks to Jens Barnow.
#
# Revision 1.6  2001/01/06 05:27:49  paulk
# Add "\." below to the definition of class_type so
# that methods whose return type included a period (e.g, Foo.Bar)
# were tagged:
#
#   class_type="\<\([$capital][a-zA-Z_$digit\.]*\)\>"
#
# Thanks to Charles Rich <rich@merl.com>
#
# Revision 1.5  2000/12/23 04:28:15  paulk
# (1) Added "-l none" to the etags arguments (all four calls) to
# eliminate the additional spurious tags that were coming from etags'
# default java parsing:
#
#     ${etags_dir}etags -l none -a -o ${java_dir}/TAGS \
#
# (2) Added "\/" to the prohibited characters at the start of class2,
# method1 and method2 to prevent comment lines like
#
# // this is a nice interface for catching mice
#
# getting tagged:
#
#     class2="/^[^.*\/]*\($modifier$ws*\)*\<\(class\|interface\)\>$ws*$identifier/\7/"
#
# Thanks to Charles Rich <rich@merl.com>  for these improvements.
#
# Revision 1.4  1999/08/19 10:25:34  paulk
# Added prune clause for CVS directories.
#
# Revision 1.3  1998/02/19 04:44:48  kinnucan
# Updated to Bourne version.
#
# Revision 1.2  1997/08/26 09:08:21  kinnucan
# 1. Exclude RCS (version control) directories from tags search.
# 2. Added pattern for recognizing abstract classes.
#
#

# End of jtags.csh script

 
