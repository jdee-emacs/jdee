;;; jdee-issues.el -- Reporting issues in JDEE

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

(require 'jdee-deps)

;; Problem reporting functions contributed by
;; Phillip Lord <plord < at > hgmp.mrc.ac.uk>.
(defconst jdee-problem-report-mail-address
  (concat "jdee-devel" (char-to-string ?@) "lists.sourceforge.net")
  "Send email to this address for JDEE problem reporting.")

(defun jdee-submit-problem-report()
  "Submit a problem report for the JDEE."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a problem report on the JDEE? ")
   (progn
     (message "Preparing problem report...")
     ;;prepare the basic buffer
     (reporter-submit-bug-report
      jdee-problem-report-mail-address
      (format "JDEE version %s\nRequired packages: cedet-%s\n"
              jdee-version cedet-version)
      (jdee-problem-report-list-all-variables)
      nil
      'jdee-problem-report-post-hooks
      "Please enter the details of your bug report here" )
     (message "Preparing bug report...done"))))

(defun jdee-problem-report-post-hooks()
  "Function run the reporter package done its work.
It looks for a JDEBug buffer and inserts the contents of that, and then prompts
for insertion of the .emacs file"
  (save-excursion
    (goto-char (point-max))
    (let* ((debug-buffer (get-buffer "*JDEbug*"))
           (messages-buffer
            (get-buffer "*Messages*"))
           (backtrace-buffer (get-buffer "*Backtrace*"))
           (jdee-log-buffer (jdee-log-get-log-buffer))
           (process
            (let ((proc (jdee-dbs-get-target-process)))
              (if (not proc)
                  (let ((dead-proc-alist
                         (oref jdee-dbs-the-process-morgue proc-alist)))
                    (if dead-proc-alist
                        (setq proc (cdr (car dead-proc-alist))))))
              proc))
           (cli-buffer (if (and process (slot-boundp process 'cli-buf))
                           (oref process cli-buf)))
           (locals-buffer (if (and process (slot-boundp process 'locals-buf))
                              (oref process locals-buf))))


      ;;insert the contents of the debug buffer if it is there.
      (if debug-buffer
          (progn
            (insert "\n\n\nThe contents of the *JDEBug* buffer were\n\n")
            (insert-buffer-substring debug-buffer)
            (insert "\n\n\nEnd Insert *JDEbug* buffer" ))
        (insert "\n\n\nThere was no *JDEBug* buffer" ))

      ;;insert the contents of the CLI buffer if it exists.
      (if cli-buffer
          (progn
            (insert "\n\n\nThe contents of the CLI buffer are\n\n")
            (insert-buffer-substring cli-buffer)
            (insert "\n\n\nEnd Insert CLI buffer" ))
        (insert "\n\n\nThere is no CLI buffer" ))


      ;;insert the contents of the locals buffer if it exists.
      (if locals-buffer
          (progn
            (insert "\n\n\nThe contents of the locals buffer are\n\n")
            (insert-buffer-substring locals-buffer)
            (insert "\n\n\nEnd Insert locals buffer" ))
        (insert "\n\n\nThere is no locals buffer" ))

      ;;insert the contents of the backtrace buffer if it is there.
      (if backtrace-buffer
          (progn
            (insert "\n\n\nThe contents of the *Backtrace* buffer were\n\n")
            (insert-buffer-substring backtrace-buffer)
            (insert "\n\n\nEnd Insert *Backtrace* buffer" ))
        (insert "\n\n\nThere was no *Backtrace* buffer" ))


      ;;insert the contents of the messages buffer if it is there.
      (if messages-buffer
          (progn
            (insert "\n\n\nThe contents of the *Messages* buffer were\n\n")
            (insert-buffer-substring messages-buffer)
            (insert "\n\n\nEnd Insert *Messages* buffer" ))
        (insert "\n\n\nThere was no *Messages* buffer" ))

      ;;insert the contents of the jdee-log buffer if it is there.
      (if jdee-log-buffer
          (progn
            (insert "\n\n\nThe contents of the *jdee-log* buffer were\n\n")
            (insert-buffer-substring jdee-log-buffer)
            (insert "\n\n\nEnd Insert *jdee-log* buffer" ))
        (insert "\n\n\nThere was no *jdee-log* buffer" )))

    (when process-environment
      (insert "\n\n\nProcess environment: \n\n")
      (insert (mapconcat (lambda (var) var) process-environment "\n")))

    (let* ((init-file-name ".emacs")
           (buf (get-buffer-create
                 (format "*Insert %s*" init-file-name)))
           (mail-buf (current-buffer)))

      (set-buffer buf)

      (widget-insert
       (format       "You should include the entire contents of your %s file.\n"
                     init-file-name))

      (widget-insert
       (format       "This is because often parts of the %s file that appear\n"
                     init-file-name))

      (widget-insert "not to be JDEE-related do in fact contain the cause of\n")
      (widget-insert "reported bugs.\n\n")

      (widget-insert
       (format "If you choose not to send your %s file or the file loads many\n"
               init-file-name))

      (widget-insert
       "other files, please attempt to replicate the bug, using the\n")

      (widget-insert
       (format "minimal %s file suggested in the JDEE documentation, and note\n"
               init-file-name))

      (widget-insert "that you have done this in this bug report.\n")
      (switch-to-buffer buf)

      (set-buffer mail-buf)
      (goto-char (point-max))

      (if (y-or-n-p
           (format "Insert your %s file into the problem report? "
                   init-file-name))
          (progn
            (insert
             (format "\n\n\nThe contents of the %s file was\n\n\n"
                     init-file-name))

            (insert-file-contents "~/.emacs")
            (goto-char (point-max))
            (insert
             (format "\n\n\n=====end inserted %s file"
                     init-file-name)))
        (insert
         (format "\n\n\nThe user choose not to insert their %s file\n"
                 init-file-name)))

      ;;clean up the prompt buffer
      (kill-buffer buf))))

(defun jdee-problem-report-list-all-variables()
  "List all variables starting with `jde' or `bsh'."
  (let (vars)
    (mapatoms
     (lambda (symbol)
       (if  (jdee-symbol-p symbol)
           (setq vars (cons symbol vars)))))
    vars))

(provide 'jdee-issues)

;;; jdee-issues.el ends here
