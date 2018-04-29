;;; jdee-which-method.el --- Print current method in mode line

;; Copyright (C) 1997-2004 Paul Kinnucan
;; Copyright (C) 2009 by Paul Landes

;; Author: Paul Kinnucan (paulk@mathworks.com)
;; Inspired by Alex Rezinsky's which-func package.
;; Keywords: mode-line, tools

;; This file is not part of GNU Emacs.

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

;; This package displays the name of the method at point
;; in the Emacs mode line.

;;; Code:

(require 'jdee-parse)

(defgroup jdee-which-method nil
  "Mode to display the current function name in the modeline."
  :group 'jdee)

;;;###autoload
(defcustom jdee-which-method-mode t
  "Enables the JDEE's which method mode.
When which method mode is enabled, the current method name is
displayed in the mode line."
  :group 'jdee-which-method
  :type  'boolean)

(defcustom jdee-which-method-format '("[" jdee-which-method-current "]")
  "Format for displaying the function in the mode line."
  :group 'jdee-which-method
  :type 'sexp)

(defcustom jdee-mode-line-format
  '("-"
    mode-line-mule-info
    mode-line-modified
    mode-line-frame-identification
    mode-line-buffer-identification
    "   "
    global-mode-string
    "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"
    (line-number-mode "L%l--")
    (column-number-mode "C%c--")
    (-3 . "%p")
    (jdee-which-method-mode
     ("--" jdee-which-method-format "--"))
    "-%-")
  "Format for the JDEE source buffer mode line."
  :group 'jdee
  :type 'sexp)

(defcustom jdee-which-full-class-name nil
  "Display full inner-class name in JDE's which method mode.
If nil then display only the last component of class name.
\(see `jdee-which-method-max-length', `jdee-which-method-class-min-length')."
  :group 'jdee-which-method
  :type  'boolean)

(defcustom jdee-which-method-max-length 20
  "Specify max length of the `which-method-string'.
If nil, the string is not truncated.  See `jdee-which-method-format'."
  :type '(choice (const :tag "No truncation" :value nil)
                 (integer :tag "Max. length"))
  :group 'jdee-which-method)

(defcustom jdee-which-method-class-min-length 4
  "Specifies the minimum length of the class part of the full method
name after truncation of the class name, but only if the class
is displayed and if `jdee-which-method-max-length'
is not nil. If the full method name is still greater than
`jdee-which-method-max-length', the method part of the name is truncated."
  :type '(integer :tag "Min. length")
  :group 'jdee-which-method)

(defcustom jdee-which-method-abbrev-symbol "~"
  "Symbol used to indicate abbreviated part of a method name."
  :group 'jdee-which-method
  :type  'string)

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;

(defvar jdee-which-method-idle-timer nil
  "Timer that updates the mode line.")

(defvar jdee-which-method-unknown "???"
  "String to display in the mode line when the current method is unknown.")

(defvar jdee-which-method-current jdee-which-method-unknown)
(make-variable-buffer-local 'jdee-which-method-current)

(defvar jdee-which-method-previous jdee-which-method-unknown)
(make-variable-buffer-local 'jdee-which-method-previous)

(defvar jdee-which-method-current-point-loc -1)
(make-variable-buffer-local 'jdee-which-method-current-point-loc)

(defvar jdee-which-method-current-method-bounds (cons -1 -1))
(make-variable-buffer-local 'jdee-which-method-current-method-bounds)


(defun jdee-which-method-truncate-begin (str truncation)
  (if (> truncation (length jdee-which-method-abbrev-symbol))
      (concat jdee-which-method-abbrev-symbol (substring str truncation))
    str))

(defun jdee-which-method-truncate-end (str truncation)
  (let ((str-length (length str)))
    (if (> truncation (length jdee-which-method-abbrev-symbol))
        (concat (substring str 0 (- str-length truncation))
                jdee-which-method-abbrev-symbol)
      str)))

(defun jdee-which-method-class-name(name)
  (if jdee-which-full-class-name
      (car (jdee-which-full-class-namef (jdee-parse-get-innermost-class-at-point)))
    (caar name)))

(defun jdee-which-method-update ()
  (interactive)
  (if (and jdee-which-method-mode
           (eq major-mode 'jdee-mode))
      (condition-case info
          (let ((p (point)))
            (unless (or
                     (= jdee-which-method-current-point-loc p)
                     (and
                      (>= p (car jdee-which-method-current-method-bounds))
                      (<= p (cdr jdee-which-method-current-method-bounds))))
              (let ((name ;; (jdee-parse-get-method-at-point)
                     (if jdee-parse-the-method-map
                         (jdee-parse-method-map-get-method-at jdee-parse-the-method-map))
                     ))
                (if name
                    (let* ((name-pair (car name))
                           (class (jdee-which-method-class-name name))
                           (method (cdr name-pair))
                           (bounds (cdr name))
                           (class-length (length class))
                           (method-length (length method))
                           ;; initialize the truncation with 0!
                           (trunc-class 0)
                           (trunc-method 0)
                           (trunc-complete 0))
                      (when jdee-which-method-max-length
                        ;; compute necessary truncation of method and/or class
                        (if jdee-parse-buffer-contains-multiple-classes-p
                            (when (> (+ class-length method-length 1)
                                     jdee-which-method-max-length)
                              (setq trunc-complete (- (+ class-length
                                                         method-length 1)
                                                      jdee-which-method-max-length))
                              (if (< (- class-length trunc-complete)
                                     jdee-which-method-class-min-length)
                                  (setq trunc-class
                                        (- class-length
                                           jdee-which-method-class-min-length)
                                        trunc-method (- trunc-complete
                                                        trunc-class))
                                (setq trunc-method 0
                                      trunc-class trunc-complete)))
                          (when (> method-length jdee-which-method-max-length)
                            (setq trunc-method (- method-length
                                                  jdee-which-method-max-length)))))
                      ;; truncate method and class with the computed truncation
                      ;; (possible 0, then no truncation is done in fact)
                      (setq class (jdee-which-method-truncate-end class trunc-class)
                            method (jdee-which-method-truncate-end method trunc-method))
                      ;; set the displayed string from the (possible truncated)
                      ;; class and method parts according to
                      ;; jdee-parse-buffer-contains-multiple-classes-p.
                      (setq jdee-which-method-current
                            (if jdee-parse-buffer-contains-multiple-classes-p
                                (format "M:%s.%s" class method)
                              (format "M:%s" method)))
                      (setq jdee-which-method-current-point-loc p)
                      (setq jdee-which-method-current-method-bounds bounds))
                  (progn
                    (setq name (jdee-which-class-name (jdee-parse-get-innermost-class-at-point)))
                    (setq jdee-which-method-current-point-loc p)
                    (setq jdee-which-method-current-method-bounds (cons -1 -1))
                    (if name
                        (let* ((class (car name))
                               (class-length (length class)))
                          ;; possible truncate the string to display
                          (when (and jdee-which-method-max-length
                                     (> class-length jdee-which-method-max-length))
                            (setq class (jdee-which-method-truncate-begin class
                                                                          (- class-length
                                                                             jdee-which-method-max-length))))
                          (setq jdee-which-method-current (format "C:%s" class)))
                      (setq jdee-which-method-current jdee-which-method-unknown))))
                (unless (equal jdee-which-method-current jdee-which-method-previous)
                  (setq jdee-which-method-previous jdee-which-method-current)
                  (force-mode-line-update)))))
        (error
         ;; (debug)
         (cancel-timer jdee-which-method-idle-timer)
         (setq jdee-which-method-idle-timer nil)
         (message "Error in jdee-which-method-update: %s" info)))))

(defun jdee-which-full-class-namef (name)
  "name and return value is: (string . point) or nil."
  (save-excursion
    (do ((rv name)) ((not name) rv)
      (goto-char (cdr name))
      (setq name (jdee-parse-get-innermost-class-at-point))
      (when name
        (setf (car rv) (concat (car name) "." (car rv)))))))

(defun jdee-which-class-name (name)
  "Use given `NAME' or gather full-name."
  (if jdee-which-full-class-name
      (jdee-which-full-class-namef name)
    name))

(defun jdee-which-method-update-on-entering-buffer ()
  "This is a hook function.
Catch all errors to avoid canceling other hooks."

  (condition-case err
      (progn
        (setq jdee-which-method-current-point-loc 0)
        (setq jdee-which-method-current-method-bounds (cons -1 -1))
        (jdee-which-method-update))
    (error (message
            "Which method update error: %s"
            (error-message-string err)))))

(provide 'jdee-which-method)

;;; jdee-which-method.el ends here
