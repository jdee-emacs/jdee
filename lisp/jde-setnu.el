;; Display line-numbers, but has problems with filling (Emacs hangs) and
;; killing lines (confusing the number display).
(require 'setnu)

(defcustom jde-turn-on-setnu-mode-threshold 20000
 "Maximum number of bytes in a file (buffer) that can result in
automatic line numbering.")

(defvar jde-setnu-deletion-check t "deletion check")
(make-variable-buffer-local 'jde-setnu-deletion-check)

(add-hook 
 'after-change-functions 
 ;; when in setnu-mode toggles setnu-mode off and on.
 (lambda (start end length)
   (if setnu-mode
       (if (or
	    (and
	     (> length 0)
	     jde-setnu-deletion-check)
	    (string-match 
		  "[\n\r]" 
		  (buffer-substring-no-properties start end)))
	   (run-with-timer 
	    0.001 nil
	    ;; setnu toggler      
	   (lambda () (setnu-mode) (setnu-mode))))
     (setq jde-setnu-deletion-check nil))))

(add-hook 
 'before-change-functions 
 ;; Determines whether any newlines were deleted
 (lambda (start end) 
   (if setnu-mode
       (if (> end start) 
	   (setq jde-setnu-deletion-check 
		 (string-match "[\n\r]" (buffer-substring-no-properties start end)))))))

(provide 'jde-setnu)
