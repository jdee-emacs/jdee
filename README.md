# jdee

The JDEE is an add-on software package that turns Emacs into a
comprehensive system for creating, editing, debugging, and documenting
Java applications.

Building JDEE requires ```ant``` available from
```http://ant.apache.org/bindownload.cgi```

JDEE can use beanshell for some operations - available from
http://www.beanshell.org/. Copy bsh-2.0b4.jar to
$JAVA_HOME/jre/lib/ext, or put it in CLASSPATH.

# Build options

1. Available Ant targets: ```ant -p```
2. Run unit tests: ```ant test```
3. Build project: ```ant bindist```
