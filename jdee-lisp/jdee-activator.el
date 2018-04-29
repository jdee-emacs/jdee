;;; jdee-activator.el --- JDEE mode internal monitoring

;; Maintainer: jdee-devel

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

;;; Commentary:

;;; Code:

(defvar jdee-monitor-post-command-hook-timer nil
  "Timer that runs `jdee-monitor-post-command-hook' during idle moments.")

(defvar jdee-entering-java-buffer-hook
  '(jdee-reload-project-file
    jdee-which-method-update-on-entering-buffer)
  "Lists functions to run when entering a jdee-mode source buffer from another
jdee-mode buffer. Note that these functions do not run when reentering the same
jdee-mode buffer from a non-jdee-mode buffer. You should use this hook only for
functions that need to be run when you switch from one jdee-mode buffer to
a different jdee-mode buffer. Use `jdee-mode-hook' if the function needs to run
only once, when the buffer is created.")

(defvar jdee-current-buffer (current-buffer)
  "Internal JDEE variable that holds the current active buffer.")

(defun jdee-detect-java-buffer-activation ()
  "Detects when a user activates a buffer.
If the activated buffer is a Java buffer, runs the
`jdee-entering-java-buffer-hook' hooks."
  (let ((curr-buff (current-buffer)))
    (unless (equal curr-buff jdee-current-buffer)
      (setq jdee-current-buffer curr-buff)
      (if (eq major-mode 'jdee-mode)
	  (condition-case err
	      (run-hooks 'jdee-entering-java-buffer-hook)
	    (error
	     (message "jdee-entering-java-buffer-hook error: %s"
		      (error-message-string err))))))))

(defun jdee-monitor-post-command-hook ()
  "Check whether `post-command-hook' includes all hooks required by JDEE.
If not, it adds the required hooks."
  (if (eq major-mode 'jdee-mode)
      (dolist (hook (list 'jdee-detect-java-buffer-activation))
	(when (not (member hook post-command-hook))
	  (add-hook 'post-command-hook hook)))))

(defun jdee-count-open-java-buffers ()
  "Return non-nil if any Java buffers are open."
  (cl-count
   ".java"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (save-match-data
             (string-match file-type file-name)))))))

(defun jdee-clean-up-after-jde ()
  "Remove `jdee-detect-java-buffer-activation-hook' when
all Java source buffers have been closed."
  (if (eq major-mode 'jdee-mode)
      (unless  (> (jdee-count-open-java-buffers) 1)
	(remove-hook 'post-command-hook 'jdee-detect-java-buffer-activation)
	(when jdee-monitor-post-command-hook-timer
	  (cancel-timer jdee-monitor-post-command-hook-timer)
	  (setq jdee-monitor-post-command-hook-timer nil))
	(remove-hook 'kill-buffer-hook 'jdee-clean-up-after-jde))))

(defun jdee-activator-init ()
  "Setup jdee-mode activation and deactivation in Java buffers."
  ;; Enable support for automatic project switching.
  ;; This feature loads the appropriate project settings whenever
  ;; a user switches from a Java buffer belonging to one project
  ;; to a buffer belonging to another.
  (add-hook 'post-command-hook
            'jdee-detect-java-buffer-activation
            nil
            t ;; XEmacs ignores this argument if symbol is not local.
            )

  (unless jdee-monitor-post-command-hook-timer
    (setq
     jdee-monitor-post-command-hook-timer
     (run-with-idle-timer 1 t 'jdee-monitor-post-command-hook)))

  (unless (member 'jdee-clean-up-after-jde kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'jdee-clean-up-after-jde)))

(provide 'jdee-activator)
;;; jdee-activator.el ends here
