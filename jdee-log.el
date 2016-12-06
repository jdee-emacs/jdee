;;; jdee-log.el --- JDEE logger  -*- lexical-binding: t -*-

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; JDEE logging functions direct all messages to common `jdee-log' buffer.
;; If `jdee-log-max' is set to a number, the buffer rotates after this
;; many lines.

;;; Code:

(defconst jdee-log-buffer-name "*jdee-log*")

(defcustom jdee-log-max 500
  "Maximum number of lines to keep in the JDEE log buffer.
If nil, disable logging.  If t, don't truncate the buffer."
  :group 'jdee-project
  :type '(choice (integer :tag "Number of lines to keep")
                 (boolean :tag "Disable/Unlimited")))

(defun jdee-log-get-log-buffer ()
  "Return log buffer if exists or nil."
  (get-buffer "*jdee-log*"))

(defun jdee-log-msg (msg &rest args)
  "Log message MSG to the *jdee-log* buffer.
Optional ARGS are used to `format' MSG.
Does nothing if `jdee-log-max' is nil."
  (if jdee-log-max
      (save-match-data
        (with-current-buffer (get-buffer-create jdee-log-buffer-name)
          (goto-char (point-max))
          (insert (apply 'format msg args))
          (insert "\n")
          (if (integerp jdee-log-max)
              (let ((line-cnt 0))
                (while (search-backward "\n" nil t)
                  (setq line-cnt (1+ line-cnt)))
                (goto-char (point-min))
                (while (> line-cnt jdee-log-max)
                  (delete-region (point) (search-forward "\n" nil t))
                  (setq line-cnt (1- line-cnt)))))))))

(defun jdee-log-msg-t (msg &rest args)
  "Log message MSG to the *jdee-log* buffer, and return t.
Optional ARGS are used to `format' MSG.
Does nothing but return t if `jdee-log-max' is nil."
  (jdee-log-msg msg args)
  t)

(defun jdee-log-msg-nil (msg &rest args)
  "Log message MSG to the *jdee-log* buffer, and return nil.
Optional ARGS are used to `format' MSG.
Does nothing but return nil if `jdee-log-max' is nil."
  (jdee-log-msg msg args)
  nil)

(provide 'jdee-log)

;;; jdee-log.el ends here
