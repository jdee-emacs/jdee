;; -*- Mode: Emacs-Lisp -*- 

;; Author: Dimitre Liotev (dl@znain.net)

;; This file compiles the Elisp source files in the "lisp" directory.
;; Invoke it in the following way:

;;     emacs --script build.el

;; Note that you must have the appropriate versions of the Cedet and
;; Elib packages and the environment variables CEDET_HOME and ELIB_HOME
;; must be defined in your environment.

(add-to-list 'load-path (concat (getenv "CEDET_HOME") "/common"))
(add-to-list 'load-path (concat (getenv "CEDET_HOME") "/eieio"))
(add-to-list 'load-path (concat (getenv "CEDET_HOME") "/semantic"))
(add-to-list 'load-path (concat (getenv "CEDET_HOME") "/semantic/bovine"))
(add-to-list 'load-path (getenv "ELIB_HOME"))
(add-to-list 'load-path (expand-file-name"./lisp"))

(load (expand-file-name "lisp/jde.el"))

(byte-recompile-directory (expand-file-name "lisp") 0)
