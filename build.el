;; -*- Mode: Emacs-Lisp -*- 

;; Author: Dimitre Liotev (dl@znain.net)

;; This file compiles the Elisp source files in the "lisp" directory.
;; Invoke it in the following way:

;;     emacs --script build.el

;; Note that you must have the appropriate versions of the Cedet and
;; Elib packages and the environment variables CEDET_HOME and ELIB_HOME
;; must be defined in your environment.


(load-file "./build-options.el")
(add-to-list 'load-path (concat cedet-home "/common"))
(add-to-list 'load-path (concat cedet-home "/eieio"))
(add-to-list 'load-path (concat cedet-home "/semantic"))
(add-to-list 'load-path (concat cedet-home "/semantic/bovine"))
(add-to-list 'load-path (concat cedet-home "/speedbar"))
(add-to-list 'load-path elib-home)
(add-to-list 'load-path (expand-file-name"./lisp"))


(message "%s" load-path)

(load (expand-file-name "lisp/jde.el"))

(byte-recompile-directory (expand-file-name "lisp") 0)
