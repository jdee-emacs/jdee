;;; jde-java-grammar.el
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 2000, 2001, 2004 Paul Kinnucan.
;; Copyright (C) 2009 by Paul Landes

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

(require 'semantic-java)
(require 'jde-parse)
(eval-when-compile
  (require 'senator)
  (require 'jde-which-method))

(defun jde-parse-semantic-default-setup ()
  "Setup the semantic bovinator for the JDE.
Should be run when Semantic is ready to parse, that is, via
`semantic-init-hooks'."

  ;; Remove me from `semantic-init-hooks'
  (remove-hook 'semantic-init-hooks 'jde-parse-semantic-default-setup)

  (when jde-auto-parse-enable
    ;; JDE delegates auto-parse to Semantic if possible.  Since
    ;; version 1.4beta12, Semantic uses an idle timer to
    ;; reparse modified buffers if the corresponding minor mode is
    ;; enabled. Activate Semantic auto-parse in this buffer, unless
    ;; it is already enabled globally.
    (cond
     ;; Since Semantic 2.0beta2
     ((boundp 'global-semantic-idle-scheduler-mode)
      (or global-semantic-idle-scheduler-mode
	  (semantic-idle-scheduler-mode 1)))
     ;; Since Semantic 1.4beta12
     ((boundp 'global-semantic-auto-parse-mode)
      (or global-semantic-auto-parse-mode
	  (semantic-auto-parse-mode 1)))
     (t
      ;; Default to JDE's auto-parse
      (make-local-hook 'semantic-change-hooks)
      (add-hook 'semantic-change-hooks
		'jde-parse-buffer-changed-hook t t))))

  ;; Track full reparses
  (make-local-hook 'semantic-after-toplevel-cache-change-hook)
  (add-hook 'semantic-after-toplevel-cache-change-hook
	    'jde-parse-update-after-parse nil t)

  ;; Track partial reparses
  (make-local-hook 'semantic-after-partial-cache-change-hook)
  (add-hook 'semantic-after-partial-cache-change-hook
	    'jde-parse-update-after-partial-parse nil t)

  (when jde-enable-senator
    ;; Enable `senator-minor-mode' in this buffer, unless it is
    ;; already enabled globally (since Semantic 1.4beta12).
    (or (and (boundp 'global-senator-minor-mode)
	     global-senator-minor-mode)
	(senator-minor-mode 1)))

  ;; imenu & speedbar setup
  (jde-imenu-setup)

  (if (and jde-which-method-mode
	   (not jde-which-method-idle-timer))
      (setq jde-which-method-idle-timer
	    (run-with-idle-timer .25 t 'jde-which-method-update)))

  ;; initial parsing of the current buffer
  (semantic-fetch-tags))

(provide 'jde-java-grammar)

;; End of jde-java-grammar.el