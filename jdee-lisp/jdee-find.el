;;; jdee-find.el -- Find command

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

(require 'jdee-classpath)
(require 'jdee-files)
(require 'efc)

;; Hack required by faulty XEmacs implementation of executable-find.
(defun jdee-find-get-find-exec ()
  "Return path to `find' command or nil."
  (executable-find
   (if (eq system-type 'windows-nt) "find.exe" "find")))

(defun jdee-find-get-grep-exec ()
  "Return path to `grep' command or nil."
  (executable-find
   (if (eq system-type 'windows-nt) "grep.exe" "grep")))

(defcustom jdee-find-case-sensitive nil
  "*Specifies whether the jdee-find command performs a case-sensitive search.
If non-nil, the search is case-sensitive; otherwise, the search ignores case."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-find-granularity '("Character")
  "Specifies the granularity of the expression search
conducted by `jdee-find': Character (expression starting
on any character), Word (match words only), Line
(match lines only)."
  :group 'jdee-project
  :type  '(list
           (radio-button-choice
            :format "%t \n%v"
            :tag "Search granularity:"
            (item "Character")
            (item "Word")
            (item "Line"))))

(defcustom jdee-find-file-regexp '("*.java")
  "Specifies the regular expression that the jdee-find command uses
to select files to be searched. You can use any regular expression
supported by the -name option of the GNU find command."
  :group 'jdee-project
  :type '(repeat (string :tag "Find regexp")))

(defclass jdee-find-dialog (efc-dialog)
  ((expr-field     :initarg :expr-field
                   :documentation "Edit field for expression to find.")
   (expression     :initarg :expression
                   :type string
                   :initform ""
                   :documentation "Regular expression to find.")
   (dir-fields     :initarg :dir-fields
                   :documentation "List of directory path fields.")
   (dirs           :initarg :dirs
                   :type list
                   :documentation "Directories to search recursively.")
   (file-fields    :initarg :file-fields
                   :documentation "Regular expression fields for files to search.")
   (files          :initarg :files
                   :type list
                   :initform ("*.java")
                   :documentation "Files to search.")
   (ignore-case-cb :initarg :ignore-case
                   :documentation "Ignore case check box.")
   (ignore-case-p  :initarg :ignore-case-p
                   :type boolean
                   :initform t
                   :documentation "If true, ignore case when searching.")
   (grain-rbs      :initarg :grain-rbs
                   :documentation "Granularity radio buttons.")
   (grain          :initarg :grain
                   :type string
                   :initform "Character"
                   :documentation "Search granularity: Character, Word, Line")
   (ok             :initarg :ok
                   :type boolean
                   :initform nil
                   :documentation "True if user clicked the OK button.")
   (the-dialog     :type (or null jdee-find-dialog)
                   :allocation :class
                   :initform nil
                   :documentation
                   "The only instance of the find expression dialog buffer."))
  "Dialog displayed by `jdee-find' command.")

(defmethod initialize-instance ((this jdee-find-dialog) &rest fields)
  "Find options dialog constructor."
  (oset this title "Find Dialog")
  (call-next-method))

(defmethod efc-dialog-create ((this jdee-find-dialog))

  (widget-insert "Find Expression Options\n\n")

  (oset this expr-field
        (widget-create
         (list
          'text
          :tab-order 1
          :format "%t %v"
          :tag "Expression:"
          :value (oref this expression))))

  (widget-insert "\n")

  (oset this dir-fields
        (widget-create
         (list
          'repeat
          :tag "Directories to search recursively"
          :value (if (slot-boundp this 'dirs)
                     (oref this dirs)
                   (mapcar
                    (lambda (p)
                      (jdee-normalize-path p 'jdee-sourcepath))
                    jdee-sourcepath))
          (list 'file :tag "Path"))))
  (widget-insert "\n")

  (oset this file-fields
        (widget-create
         (list
          'repeat
          :tag "File types to search"
          :value (oref this files)
          (list 'file :tag "File regexp"))))

  (widget-insert "\n")

  (oset this ignore-case-cb
        (widget-create
         (list 'checkbox
               :format "%[%v%] %t"
               :tag "Ignore case"
               :value (oref this ignore-case-p)
               )))

  (widget-insert "\n\n")

  (oset this grain-rbs
        (widget-create
         (list
          'radio-button-choice
          :format "%t\n%v"
          :tag "Search granularity:"
          :value (oref this grain)
          :args (list
                 (list 'item "Character")
                 (list 'item "Word")
                 (list 'item "Line")))))

  (widget-insert "\n"))

(defmethod efc-dialog-show ((this jdee-find-dialog))
  "Shows the options dialog buffer. After showing the dialog buffer,
this method invokes recursive-edit to emulate the behavior of a modal
dialog. This suspends the current command until the user has selected
an option or canceled the dialog. See `efc-dialog-ok' and
`efc-dialog-cancel' for more information."
  (call-next-method)
  (recursive-edit))

(defmethod efc-dialog-ok ((this jdee-find-dialog))
  "Invoked when the user selects the OK button on the options
dialog. Sets the :dirs field of THIS to the search paths chosen by the
user, kills the dialog buffer, and exits recursive-edit mode."

  (oset this
        expression
        (widget-value (oref this expr-field)))

  (oset this
        dirs
        (widget-value (oref this dir-fields)))

  (oset this
        files
        (widget-value (oref this file-fields)))

  (oset this
        ignore-case-p
        (widget-value (oref this ignore-case-cb)))

  (oset this
        grain
        (widget-value (oref this grain-rbs)))

  (oset this ok t)

  (delete-window)
  (kill-buffer (oref this buf))
  (pop-to-buffer (oref this initbuf))
  (set-buffer (oref this initbuf))

  (exit-recursive-edit))

(defmethod efc-dialog-cancel ((this jdee-find-dialog))
  "Invoked when the user clicks the dialog's Cancel button.  Invokes
the default cancel method, sets the :selection field of THIS to nil,
and then exits recursive edit mode."
  (call-next-method)
  (oset this ok nil)
  (exit-recursive-edit))

(defvar jdee-find-root-history nil
  "History of directory trees searched in this session.")

(defvar jdee-find-regexp-history nil
  "History of search expressions used in this session.")

(defun jdee-find-grep-internal (regexp files &optional dirs no-case grain)
  "Find a regular expression REGEXP in files of type FILES in
 DIRS, where DIRS is a string of space-separated paths of
directories to search recursively. If NO-CASE is nonnil, ignore
case. GRAIN is a string that indicates the granularity of the search,
i.e., match any \"Character\" string, a \"Word\" only, or a \"Line\"
only."
  (if (not (jdee-find-get-grep-exec))
      (error "This command requires the Unix grep utility"))
  (if (not (jdee-find-get-find-exec))
      (error (list "This command requires the Unix find utility.")))
  (let* ((directories-option
          (if dirs dirs "."))
         (case-sensitive-option
          (if no-case  "-i" ""))
         (granularity-option
          (cond
           ((and grain (string= grain "Word"))
            "-w")
           ((and grain (string= grain "Line"))
            "-x")
           (t
            " ")))
         (file-regexp-option
          (mapconcat
           (lambda (x)
             (format "-name \"%s\"" x))
           files
           " -or "))
         (cmd
          (format "find %s %s -type f | xargs grep %s %s -n \"%s\" /dev/null"
                  directories-option
                  file-regexp-option
                  case-sensitive-option
                  granularity-option
                  regexp)))
    (grep cmd)))

;;;###autoload
(defun jdee-find (&optional regexp)
  "Find a regular expression REGEXP in all of the files in the
current JDEE project. Tests each of the following path variables
`jdee-sourcepath', `jdee-compile-option-sourcepath',
`jdee-compile-option-classpath', or `jdee-global-classpath' and uses the
directories specified by the first path variable that has a nonnil
value. The `jdee-find-case-sensitive' variable controls case
sensitivity, `jdee-find-granularity' determines the granularity of the
search (character, word, line), and `jdee-find-file-regexp' determines
the type of files to be searched. Use `jdee-find-dlg' if you want to
set case sensitivity, granularity, or file types interactively. This
command requires that the Unix grep and find utilities be installed on
your system in the Emacs command path. The Cygwin package contains
Windows versions of both utilities."
  (interactive)
  (let ((regexp
         (if (and (boundp 'regexp) regexp)
             regexp
           (read-from-minibuffer
            "Search for regexp: "
            (if (boundp 'jdee-find-regexp-history)
                (car jdee-find-regexp-history)
              nil)
            nil nil 'jdee-find-regexp-history)))
        (search-path
         (read-from-minibuffer
          "Search directories: "
          (cons
           (mapconcat
            (lambda (x) x)
            (cond
             (jdee-sourcepath
              (mapcar
               (lambda (path)
                 (jdee-normalize-path path 'jdee-sourcepath))
               jdee-sourcepath))
             (jdee-compile-option-sourcepath
              (mapcar
               (lambda (path)
                 (jdee-normalize-path path 'jdee-compile-option-sourcepath))
               jdee-compile-option-sourcepath))
             (jdee-compile-option-classpath
              (mapcar
               (lambda (path)
                 (jdee-normalize-path path 'jdee-compile-option-classpath))
               jdee-compile-option-classpath))
             (jdee-global-classpath
              (mapcar
               (lambda (path)
                 (jdee-normalize-path path 'jdee-global-classpath))
               jdee-global-classpath))
             (t
              (list default-directory)))
            " ")
           0)
          nil nil 'jdee-find-root-history)))
    (jdee-find-grep-internal
     regexp
     jdee-find-file-regexp
     search-path
     (not jdee-find-case-sensitive)
     (car jdee-find-granularity))))

;;;###autoload
(defun jdee-find-dlg ()
  "Displays a dialog buffer that allows you to set all search options
interactively. Pressing the dialog's OK button initiates the
search. Use `jdee-find' if you need to set only the expression to be
found and the directories to be searched and prefer using the
minibuffer."
  (interactive)
  (let ((dialog
         (progn
           (if (not (oref-default 'jdee-find-dialog the-dialog))
               (oset-default 'jdee-find-dialog the-dialog (jdee-find-dialog "find dialog")))
           (oref-default 'jdee-find-dialog the-dialog))))
    (efc-dialog-show dialog)
    (when (oref dialog ok)
      (jdee-find-grep-internal
       (oref dialog expression)
       (oref dialog files)
       (mapconcat
        'jdee-normalize-path
        (oref dialog dirs)
        " ")
       (oref dialog ignore-case-p)
       (oref dialog grain)))))

(provide 'jdee-find)

;;; jdee-find.el ends here
