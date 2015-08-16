# JDEE

The JDEE is an add-on software package that turns Emacs into a
comprehensive system for creating, editing, debugging, and documenting
Java applications.

## About the project

As of 2015-07-13, https://github.com/jdee-emacs/jdee/ is the primary source repository.

Additional information can be found at http://jdee.sourceforge.net/rootpage.html and at https://github.com/jdee-emacs/jdee/wiki

## Requirements

Emacs 24.3 is the oldest version that JDEE can be expected to run in.

Building JDEE requires [Apache Ant](https://ant.apache.org/bindownload.cgi) and
the [Ant Contrib](http://ant-contrib.sourceforge.net/) tasks.

JDEE can use Beanshell for some operations - available from
```http://www.beanshell.org/```. Copy bsh-2.0b4.jar to
```$JAVA_HOME/jre/lib/ext```, or put it in CLASSPATH.

## Install from distribution

FIXME: need ELPA package

For more information, see ```doc/install.html```

## Build from source options

1. Available Ant targets: ```ant -p```
2. (_Always_) Run unit tests: ```ant test```
3. Run JUnit tests: ```ant test-java```
4. Build project: ```ant bindist```

To use this built distribution without installation, in your .emacs add:
```emacs-lisp
  (add-to-list 'load-path "/path/to/jdee/dist/jdee-2.4.2/lisp")
  (load "jde-autoload")
```

If hacking on jde lisp, it is better to build in the git-controlled
source directory, by adding:

```
build.lisp.dir=${project.dir}/lisp
```

to ```~/.jdee-config.properties``` (```%USERDATA%/.jdee-config.properties``` on Windows)

## Features

Some of the features of JDEE include:

- source code editing with syntax highlighting, auto indentation using the
  native GNU Emacs Java mode
- Symbol completion (jde-complete-in-line)
- Code generation: templates, import insertion/deletion/ordering
  etc.
- browse JDK doc, using the browser of your choice (jde-help-symbol and
  jde-help-docsets)
- browse your source code (jde-find-class* and semantic integration)
- compilation with automatic jump from error messages to responsible line in the
  source code using ant (jde-ant-build).
- run Java application in an interactive (comint) Emacs buffer
- integrated debugging with interactive debug command buffer and automatic
  display of current source file/line when stepping through code (jde-debug)
- supports Oracle/OpenJDK Java 7
- runs on any platform supported by GNU Emacs 24.3 and later
- easily and infinitely customizable

### Known Issues

- Static imports don't quite work
- Limited support for Java template beyond basic highlighting, parsing, and
  indenting.
- Indentation after multi-line annotations might be unexpected
- It's a little crufty after all these years - patches welcome :-)

### Troubleshooting

- If you notice a bug, open an issue on Github on the
  [JDEE repository](https://github.com/jdee-emacs/jdee)

## Community

- The [mailing list](http://sourceforge.net/p/jdee/mailman/) is still hosted by
  sourceforge. It includes `jdee-announce`, `jdee-users`, and `jdee-devel`.

## Authors

- Paul Kinnucan (original author and contributor)
- Paul Landes (current maintainer)
- Shyamal Prasad (current maintainer)
