;;; jde-java-grammar.el
;; $Revision: 1.9 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 2000, 2001, 2004 Paul Kinnucan.

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

;;; Change History:

;; $Log: jde-java-grammar.el,v $
;; Revision 1.9  2004/12/17 04:23:36  paulk
;; Update to latest version of semantic.
;;
;; Revision 1.8  2004/02/22 06:47:07  paulk
;; Update to support cedet 1.0beta2. Thanks to David Ponce.
;;
;; Revision 1.7  2001/11/11 07:18:17  paulk
;; Moves all the `jde-mode' code depending on Semantic
;; into `jde-parse-semantic-default-setup' which is now run as a
;; `semantic-init-hooks' ensuring that at this point the buffer is ready
;; for parsing.
;;
;; This solves a nasty problem of synchronization between `jde-mode' and
;; Semantic initialization.  This problem was particularly annoying in
;; XEmacs where `jde-mode' buffers were not parsed the first time.  In
;; some rare conditions the problem occurred with Emacs too.
;;
;; Thanks to David Ponce.
;;
;; Revision 1.6  2001/11/08 06:16:03  paulk
;; Updated to support semantic 1.4 beta 12.
;;
;; Revision 1.5  2001/09/16 17:54:00  paulk
;; David Ponce moved all Semantic setup code from `jde-mode-internal' to
;; `jde-parse-semantic-default-setup' (which is called by
;; `jde-mode-internal') and added a hook to support partial re-parsing
;; of buffers.
;;
;; Revision 1.4  2001/05/19 02:35:59  paulk
;; Updated to support semantic 1.4. Thanks to David Ponce.
;;
;; Revision 1.3  2001/02/21 05:55:38  paulk
;; Added require for semantic package.
;;
;; Revision 1.2  2001/01/25 05:38:39  paulk
;; Changed the definition of formal_parameter_list to improve performance (less
;; backtracking) when parsing parameters in method and constructor
;; declarations. Thanks to David Ponce.
;;
;; Revision 1.1  2000/10/25 04:30:31  paulk
;; Initial revision.
;;

;; End of jde-java-grammar.el