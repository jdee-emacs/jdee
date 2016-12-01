;;; jdee-flycheck.el -- Flycheck mode for jdee

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 2000, 2001, 2002, 2003, 2004, 2005 Paul Kinnucan.
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
;; Boston, MA 02111-1307, US
;;; Commentary:

;; Integrate flycheck mode with jdee mode.  Creates a temporary buffer
;; and compiles that one as the user continues to edit the original buffer.

;;; Code:

(require 'flycheck)

(defclass jdee-flycheck-compiler (jdee-compile-compiler)
  ((post-fn         :initarg :post-fn
                    :type function
                    :documentation
                    "Function to run after compilation is complete."))
  "Compiler used by flycheck mode to compile in the background.
  Hides the output buffer so the user can continue to edit the
  file.")

(defmethod initialize-instance ((this jdee-flycheck-compiler) &rest fields)
 ;; Call parent initializer.
  (call-next-method)

  (let ((compiler (jdee-compile-get-the-compiler)))
    (when (slot-boundp compiler 'use-server-p)
      (oset this use-server-p (oref compiler use-server-p)))
    (when (slot-boundp compiler 'name)
      (oset this name (oref compiler name)))
    (when (slot-boundp compiler 'version)
      (oset this version (oref compiler version)))
    (when (slot-boundp compiler 'path)
      (oset this path (oref compiler path)))
    (when (slot-boundp compiler 'buffer)
      (oset this buffer (oref compiler buffer)))
    (when (slot-boundp compiler 'window)
      (oset this window (oref compiler window)))
    (when (slot-boundp compiler 'interactive-args)
      (oset this interactive-args (oref compiler interactive-args)))))

(defun jdee-flycheck-unmute (&rest _)
  (set 'jdee-compile-mute nil))

(defmethod jdee-compile-compile ((this jdee-flycheck-compiler))

  (if (oref this use-server-p)
      (oset this buffer (jdee-compile-server-buffer "compilation buffer"))
    (oset this buffer (jdee-compile-exec-buffer "compilation buffer")))
  (with-current-buffer (oref (oref this buffer) buffer)
    (add-hook 'jdee-compile-finish-hook
              (oref this post-fn)
              nil t)
    (add-hook 'jdee-compile-finish-hook 'jdee-flycheck-unmute t))

  ;; Pop to compilation buffer.
  (let* ((outbuf (oref (oref this buffer) buffer)))
    ;;    (outwin (display-buffer outbuf)))
    ;;    (compilation-set-window-height outwin)
    ;;    (oset this :window outwin)

    (if compilation-process-setup-function
        (funcall compilation-process-setup-function))

    (jdee-compile-launch this)

    (setq compilation-last-buffer outbuf)))

(defun jdee-flycheck-javac-command (checker cback)
  ;;(message "Calling jdee-flycheck-javac-command")
  (jdee-flycheck-compile-buffer checker cback))

(defun jdee-flycheck-compile-buffer-error (checker file line col message buffer)
  "Expects a match with file line message"
  (flycheck-error-new
   :buffer buffer
   :checker checker
   :filename file
   :line (string-to-number line)
   :column col
   :message message
   :level 'error))

(defun jdee-flymake-get-col ()
  "Get the column of the compiliation error.

An error looks like:

/Users/c08848/projects/jdee-server/src/main/java/jde/util/AntServer.java:48: error: incompatible types: int cannot be converted to Class
    private static Class ant = 00;
                               ^

The caret indicates the column of the error.  This function looks
for the caret and converts it to a column number."
  (when (looking-at "\\( *\\)^")
    (1+ (length (match-string 1)))))

(defun jdee-flycheck-compile-buffer-after (checker cback orig-file orig-buffer
                                                   temp-file temp-buffer)
  "Callback function called when the compilation is complete.
Looks for errors and converts then to flycheck errors.  Also
cleans up after the compilation."
  ;;
  ;; There are parts of this file that break if the entire file is lexically bound.
  (lexical-let ((orig-file orig-file)
                (orig-buffer orig-buffer)
                (temp-file temp-file)
                (cback cback)
                (checker checker)
                (temp-buffer temp-buffer))
    (lambda (buf msg)
      (with-current-buffer buf
        (goto-char (point-min))
        (while (re-search-forward temp-file nil t)
          (replace-match orig-file))
        (let ((errors nil))
          (goto-char (point-min))
          (while (jdee-flycheck-find-next-error)
            (forward-line 2)
            (let ((file (match-string 1))
                  (line (match-string 2))
                  (message (match-string 3))
                  ;; jdee-flymake-1get-col changes search data; do it last
                  (col (jdee-flymake-get-col)))
              (add-to-list 'errors
                           (jdee-flycheck-compile-buffer-error checker
                                                               file
                                                               line
                                                               col
                                                               message
                                                               orig-buffer))))

          (kill-buffer temp-buffer)
          ;;(kill-buffer buf)
          (set (make-local-variable 'jdee-compile-jump-to-first-error) nil)
          (set 'jdee-compile-mute t)
          ;;(message "ERRORS: %s" errors)
          (funcall cback 'finished errors))))))

(defun jdee-flycheck-find-next-error ()
  ;; To avoid stack overflow while executing regex search over possibly long lines,
  ;; hone in on only those lines that contain the magic string "error:"
  (if (search-forward "error:" nil t)
      (progn
        (beginning-of-line)
        (or (re-search-forward "^\\(.*\\):\\([0-9]+\\): *error:\\(.*\\)$"
                               (save-excursion (end-of-line) (point))
                               t)
            (progn (forward-line) (jdee-flycheck-find-next-error))))))

(defun jdee-flycheck-cleanup ()
  "Cleans up after flycheck.

Deletes the temporary files listed in `jdee-flycheck-temp-files'"
  (dolist (temp-file jdee-flycheck-temp-files)
    (cond
     ((file-directory-p temp-file) (delete-directory temp-file t))
     ((file-exists-p temp-file) (delete-file temp-file)))))

(defvar jdee-flycheck-temp-files nil
  "Files to delete whean the buffer is killed.")

(defun jdee-flycheck-compile-buffer (checker cback &optional buffer)
  "Compile the buffer without saving it.  Creates a temporary
file and buffer with the contents of the current buffer and compiles that one."
  (let* ((name (file-name-nondirectory buffer-file-name))
         (orig-file buffer-file-name)
         (orig-buffer (or buffer (current-buffer)))
         (dir (make-temp-file "JDEE_flycheck_" t))
         (temp-file (expand-file-name name dir)))
    (with-current-buffer orig-buffer
      (write-region (point-min) (point-max) temp-file nil :silent))
    (with-current-buffer (generate-new-buffer name)
      (add-to-list (make-local-variable 'jdee-flycheck-temp-files) dir)
      (insert-file-contents-literally temp-file)
      (setq buffer-file-name temp-file)

      (add-hook 'kill-buffer-hook 'jdee-flycheck-cleanup nil t)
      ;; Don't write the class file to the source directory
      (unless jdee-compile-option-directory
        (set (make-local-variable 'jdee-compile-option-directory)
             (expand-file-name "classes" dir)))

      (setq compilation-finish-functions
            (lambda (buf msg)
              (run-hook-with-args 'jdee-compile-finish-hook buf msg)
              (setq compilation-finish-functions nil)))

      (let ((compiler (jdee-flycheck-compiler "flycheck")))
        (oset compiler post-fn
              (jdee-flycheck-compile-buffer-after checker cback
                                                  orig-file orig-buffer
                                                  temp-file (current-buffer)))
        (jdee-compile-compile compiler)))))



;; (defun jdee-flycheck-command-using-server ( checker cback )
;;   "Use flycheck to search the current buffer for compiler errrors."
;;   (if (not (comint-check-proc "*groovy*"))
;;       (funcall cback 'finished nil)
;;     (let* ((pom-path malabar-mode-project-file)
;;         (pm  malabar-mode-project-manager)
;;         (buffer (current-buffer))
;;         (func (if (buffer-modified-p) 'malabar-parse-scriptbody-raw 'malabar-parse-script-raw))
;;         (script (if (buffer-modified-p) (buffer-string) (buffer-file-name))))

;;       ;;(message "flycheck with func:%s" func)
;;       (funcall func
;;        (lambda (_status)
;;       ;(message "%s %s %s" status (current-buffer) url-http-end-of-headers)
;;       (condition-case err
;;           (progn
;;             (goto-char url-http-end-of-headers)
;;             (let ((error-list (jdee-flycheck-error-parser (json-read) checker buffer)))
;;               (kill-buffer (current-buffer))
;;               ;(message "ERROR LIST:%s" error-list)
;;               (with-current-buffer buffer
;;                 (funcall cback 'finished error-list))))
;;         (error (let ((msg (error-message-string err)))
;;                  (message "flycheck error: %s" msg)
;;                  (pop-to-buffer (current-buffer))
;;                  (funcall cback 'errored msg)))))
;;        pm pom-path script))))




;; (defun jdee-flycheck-error-new (checker error-info buffer)
;;   "Create a flycheck error from "
;;   (flycheck-error-new
;;    :buffer buffer
;;    :checker checker
;;    :filename (buffer-file-name buffer)
;;    :line (cdr (assq     'line error-info))
;;    :column (cdr (assq   'startColumn error-info))
;;    :message (cdr (assq  'message error-info))
;;    :level 'error))



;; (defun jdee-flycheck-error-parser (output checker buffer)
;;   "Parse errors in OUTPUT which is a JSON array"
;;   (let ((rtnval (mapcar (lambda (e)
;;                        (jdee-flycheck-error-new checker e buffer))
;;                      output)))
;;     rtnval))

;;;###autoload
(defun jdee-flycheck-mode ()
  (flycheck-define-generic-checker 'jdee-flycheck-javac-checker
    "Integrate flycheck with the jdee using javac."
    :start #'jdee-flycheck-javac-command
    :modes '(jdee-mode))

  (add-to-list 'flycheck-checkers 'jdee-flycheck-javac-checker)
  (flycheck-mode))

(provide 'jdee-flycheck)

;;; jdee-flycheck.el ends here
