[![MELPA](http://melpa.org/packages/jdee-badge.svg)](http://melpa.org/#/jdee)
[![MELPA Stable](http://stable.melpa.org/packages/jdee-badge.svg)](http://stable.melpa.org/#/jdee)
[![Build Status](https://travis-ci.org/jdee-emacs/jdee.png?branch=master)](https://travis-ci.org/jdee-emacs/jdee)

# JDEE

The JDEE is an add-on software package that turns Emacs into a
comprehensive system for creating, editing, debugging, and documenting
Java applications.

## About the project

As of 2015-07-13, https://github.com/jdee-emacs/jdee/ is the primary source repository.

Additional information can be found at http://jdee.sourceforge.net/rootpage.html and at https://github.com/jdee-emacs/jdee/wiki

## Requirements

Emacs 24.3 is the oldest version that JDEE can be expected to run in.

JDEE requires [JDEE Server](http://github.com/jdee-emacs/jdee-server) for some operations.

## Install from distribution

The project is available in MELPA.

For more information, see ```doc/install.html```

## Build from source options

1. Run tests: ```make test```

To use this built distribution without installation, in your .emacs add:
```emacs-lisp
  (add-to-list 'load-path "/path/to/jdee")
  (require 'jdee)
```

## Features

Some of the features of JDEE include:

- source code editing with syntax highlighting, auto indentation using the
  native GNU Emacs Java mode
- Symbol completion (jdee-complete-in-line)
- Code generation: templates, import insertion/deletion/ordering
  etc.
- browse JDK doc, using the browser of your choice (jdee-help-symbol and
  jdee-help-docsets)
- browse your source code (jdee-find-class* and semantic integration)
- compilation with automatic jump from error messages to responsible line in the
  source code using ant (jdee-ant-build).
- run Java application in an interactive (comint) Emacs buffer
- integrated debugging with interactive debug command buffer and automatic
  display of current source file/line when stepping through code (jdee-debug)
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
