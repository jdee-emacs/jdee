;;; jde-util.el -- JDE utilities.
;; $Revision: 1.19.2.2 $ $Date: 2006/03/08 12:09:08 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 2001-2006 Paul Kinnucan.

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

;; This package provides useful macros, functions, and classes
;; required by multiple JDE packages. You should not put any macros,
;; functions, or classes in this package that require other
;; JDE packages.
;; 
;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://jde.sunsite.dk>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@comcast.net.

;;; Code:

(require 'efc)

(if (featurep 'xemacs)
     (load "arc-mode")
  (require 'arc-mode))

(defsubst jde-line-beginning-position (&optional n)
  (if (featurep 'xemacs)
      (save-excursion (beginning-of-line n) (point))
    (line-beginning-position n)))

(defsubst jde-line-end-position (&optional n)
  (if (featurep 'xemacs)
      (save-excursion (end-of-line n) (point))
    (line-end-position)))

;;;###autoload
(defun jde-require (feature)
   "Require FEATURE, either pre-installed or from the distribution.
 That is, first try to load the FEATURE library. Then try to load the
 jde-FEATURE library from the JDEE's distribution.
 Signal an error if FEATURE can't be found."
   (condition-case nil
      ;; If the library if available, use it.
       (require feature)
     (error
      ;; Try to use the one from the JDEE's distribution.
      (require feature (format "jde-%s" feature)))))

(defmacro jde-assert-source-buffer ()
  "Asserts that the current buffer is a
Java source or a debug buffer."
  '(assert  (eq major-mode 'jde-mode) nil 
    "This command works only in a Java source or debug buffer."))


;; Provided for XEmacs compatibility.
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
	    (newstr (if inplace string (copy-sequence string))))
	(while (> i 0)
	  (setq i (1- i))
	  (if (eq (aref newstr i) fromchar)
	      (aset newstr i tochar)))
	newstr)))

(if (not (fboundp 'replace-in-string))
    (defun replace-in-string  (string regexp newtext &optional literal)
      "Replace REGEXP with NEWTEXT in STRIng."
      (if (string-match regexp string)
	  (replace-match newtext nil nil string)
	string)))                      


(defun jde-get-line-at-point (&optional pos)
  "Get the number of the line at point."
  (let* ((point (or pos (point)))
	 (ln (if (= point 1)
		 1
	       (count-lines (point-min) point))))
    (save-excursion
      (goto-char point)
      (if (eq (char-before) ?\n)
	  (1+ ln)
	ln))))

(defmacro jde-with-file-contents (file &rest body)
  "If FILE exists and is readable creates a temporary buffer with the contents
of FILE, points to beginning of buffer, evaluates BODY and return the value of
the last form of BODY. If FILE does not exist or is not readable nil is
returned.
Note: No major/minor-mode is activated and no local variables are evaluated
for FILE, but proper EOL-conversion and charcater interpretation is done!"
  (let ((exp-filename (make-symbol "exp-filename")))
    `(let ((,exp-filename (expand-file-name ,file)))
       (if (and (file-exists-p ,exp-filename)
                (file-readable-p ,exp-filename))
           (with-temp-buffer
             (insert-file-contents ,exp-filename)
             (beginning-of-buffer)
             ,@body)
         nil))))

(defmacro jde-normalize-paths (pathlist &optional symbol)
  "Normalize all paths of the list PATHLIST and returns a list with the
expanded paths."
  (` (mapcar (lambda (path)
               (jde-normalize-path path (, symbol)))
             (, pathlist))))


(defun jde-remove-inner-class (class) 
  "Removes the inner class name for the class"
  (car (split-string class "[$]")))

(defun jde-find-class-source-file (class)
  "Find the source file for a specified class.
CLASS is the fully qualified name of the class. This function searchs
the directories and source file archives (i.e., jar or zip files)
specified by `jde-sourcepath' for the source file corresponding to
CLASS. If it finds the source file in a directory, it returns the
file's path. If it finds the source file in an archive, it returns a
buffer containing the contents of the file. If this function does not
find the source for the class, it returns nil."
  (let* ((verified-name (jde-parse-class-exists class))
         (outer-class (jde-remove-inner-class verified-name))
	 (file (concat
		(jde-parse-get-unqualified-name outer-class)
		".java"))
	 (package (jde-parse-get-package-from-name outer-class)))
    (catch 'found
      (loop for path in jde-sourcepath do
	    (progn
	      (setq path (jde-normalize-path path 'jde-sourcepath))
	      (if (and (file-exists-p path)
		       (or (string-match "\.jar$" path)
			   (string-match "\.zip$" path)))
		  (let* ((bufname (concat file " (" (file-name-nondirectory path) ")"))
			 (buffer (get-buffer bufname)))
		    (if buffer
			  (throw 'found buffer)
		      (let* ((pkg-path (subst-char-in-string ?. ?/ package))
			     (class-file-name (concat  pkg-path "/" file)) 
			     success)
		      (setq buffer (get-buffer-create bufname))
		      (save-excursion
			(set-buffer buffer)
			(setq buffer-file-name (expand-file-name (concat path ":" class-file-name)))
			(setq buffer-file-truename file)			
			(let ((exit-status 
			       (archive-extract-by-stdout path class-file-name archive-zip-extract)))
			  (if (and (numberp exit-status) (= exit-status 0))
			      (progn
				(jde-mode)
				(goto-char (point-min))
				(setq buffer-undo-list nil)
				(setq buffer-saved-size (buffer-size))
				(set-buffer-modified-p nil)
				(setq buffer-read-only t)
				(throw 'found buffer))
			  (progn
			    (set-buffer-modified-p nil)
			    (kill-buffer buffer))))))))
		(if (file-exists-p (expand-file-name file path))
		    (throw 'found (expand-file-name file path))
		  (let* ((pkg-path (subst-char-in-string ?. ?/ package))
			 (pkg-dir (expand-file-name pkg-path path))
			 (file-path (expand-file-name file pkg-dir)))
		    (if (file-exists-p file-path)
			(throw 'found file-path))))))))))
    

(defun jde-find-class-source (class &optional other-window)
  "*Find the source file for a specified class.
Calls `jde-find-class-source-file' to do the search.
If it finds the source file, it opens the file in a buffer."
  (interactive "sClass: ")
  (let ((source (jde-find-class-source-file class)))
    (if source
	(progn
	  (if (typep source 'buffer)
	      (switch-to-buffer source)
	      ;; (pop-to-buffer source other-window)
	    (if (not (string-equal (buffer-file-name)  source))
		(if other-window
		    (find-file-other-window source)
		  (find-file source))))
	  (if (fboundp 'senator-re-search-forward)
	      (let ((inner-class-pos (string-match "\\$" class)))
		(if inner-class-pos
		    (let ((inner-class (substring class (+ 1 inner-class-pos))))
		      (when inner-class
			(beginning-of-buffer)
			(senator-parse)
			(senator-re-search-forward (concat "\\b" inner-class "\\b") nil t)))))))
      (message "JDE error: Could not find source for \"%s\" in this 
project's source path. See `jde-sourcepath' for more information." class))))



(defun jde-root()
  "Return the path of the root directory of this JDEE
installation. The root directory is the parent of the
directory that contains the JDEE's Lisp files. On 
Emacs and on XEmacs installations that use the
JDEE distributable, the root directory is the root
directory that results from unpacking the distributable.
On installations based on the version of the JDEE 
packaged with XEmacs, the root directory is 
xemacs-packages/lisp."
  (let ((directory-sep-char ?/))
    (expand-file-name 
	     "../" 
	     (file-name-directory (locate-library "jde")))))

(defun jde-find-jde-data-directory ()
  "Return the path of the JDE data directory.
Returns the path of the directory containing the
JDE java and documentation directories;  nil if the 
directory cannot be found. If XEmacs, returns the location of
the data directory in the XEmacs distribution hierarchy. On all other Emacs versions, 
the JDE expects to find the documentation and Java class directories
in the same directory that contains the JDE lisp directory."
  (let ((directory-sep-char ?/))
    (if (featurep 'xemacs)
	(let ((dir (locate-data-directory "jde")))
	  (if dir dir (jde-root))))
    (jde-root)))

(defun jde-temp-directory ()
"Get the location used by the host system to store temporary files."
  (or (if (boundp 'temporary-file-directory) temporary-file-directory)
      (if (fboundp 'temp-directory) (temp-directory)
        (if (member system-type '(cygwin32 cygwin))
            (jde-cygwin-path-converter-cygpath (temp-directory))
          (temp-directory)))))

(defun jde-get-java-source-buffers ()
  "Return a list of Java source buffers open in the current session."
  (delq 
   nil 
   (mapcar 
    #'(lambda (buffer)
	(with-current-buffer buffer
	  (if (eq major-mode 'jde-mode)
	      buffer)))
    (buffer-list))))

(defun jde-get-project-source-buffers (&optional project-file) 
  "Return a list of the Java source buffers belonging to the project
whose project file is PROJECT-FILE. If PROJECT-FILE is not specified,
this function returns the buffers belonging to the project in the
currently selected source buffer."
  (let ((proj-file 
	 (or project-file
	     (if (boundp 'jde-current-project)
		 jde-current-project))))
    (delq 
     nil 
     (mapcar 
      (lambda (buffer)
	(with-current-buffer buffer
	 (if (equal jde-buffer-project-file proj-file)
	     buffer)))
      (jde-get-java-source-buffers)))))

(defun jde-get-visible-source-buffers ()
  "Return a list of visible Java source buffers."
  (delq nil (mapcar #'(lambda (buffer) 
                        (if (get-buffer-window buffer 'visible)
                            buffer))
                    (jde-get-java-source-buffers))))

(defun jde-get-selected-source-buffer ()
  (with-current-buffer (window-buffer (selected-window))
    (if (eq major-mode 'jde-mode)
        (current-buffer))))


(provide 'jde-util)

;; Change History 

;;
;; $Log: jde-util.el,v $
;; Revision 1.19.2.2  2006/03/08 12:09:08  paulk
;; Define replace-in-string if necessary.
;;
;; Revision 1.19.2.1  2006/03/01 05:00:58  paulk
;; Fix jde-find-class-source-file to handle inner classes.
;;
;; Revision 1.19  2004/12/17 04:21:25  paulk
;; Create infrastructure for supporting source archives in jde-sourcepath.
;;
;; Revision 1.18  2004/11/10 05:13:28  paulk
;; Clean up jde-find-class-source-file.
;;
;; Revision 1.17  2004/11/09 05:11:24  paulk
;; Enhance jde-find-class-source-file to find class source files stored in jar or zip files. Thanks to Christophe" <plalleme@free.fr>.
;;
;; Revision 1.16  2004/07/06 01:47:42  paulk
;; - Move jde-get-java-source-buffers and allied functions to jde-util.el.
;; - Create jde-get-project-source-buffers.
;; - Replace jde-get-current-project-buffers with jde-get-project-source-buffers.
;;
;; Revision 1.15  2004/06/06 03:54:06  paulk
;; Move subst-char-in-string definition from jde.el to here.
;;
;; Revision 1.14  2004/05/14 03:18:24  paulk
;; Moved jde-line-beginning-position and jde-line-end-position from
;; jde-db.el to jde-util.el. Ultimately, these functions should be
;; replaced by line-beginning-position and line-end-position as
;; jde-xemacs.el provides XEmacs definitions for both functions.
;;
;; Revision 1.13  2004/05/03 17:48:54  paulk
;; Adds jde-require function. Thanks to David Ponce.
;;
;; Revision 1.12  2003/12/08 03:51:51  paulk
;; Fix bug in jde-find-class-source.
;;
;; Revision 1.11  2003/12/08 03:00:18  paulk
;; Update jde-find-class-source to work for inner classes.
;;
;; Revision 1.10  2003/07/15 01:18:39  paulk
;; Add jde-temp-directory function.
;;
;; Revision 1.9  2003/06/27 04:08:11  paulk
;; Expand docstring for jde-root function.
;;
;; Revision 1.8  2003/04/09 04:05:19  paulk
;; Adds jde-root function. Rewrote jde-find-jde-data-directory to use jde-root function.
;;
;; Revision 1.7  2002/08/20 03:09:05  jslopez
;; Fixes bug in jde-find-class-source-file that was not handling
;; inner class properly.
;;
;; Revision 1.6  2002/06/22 06:13:29  paulk
;; Fixed bug in jde-normalize-paths macro.
;;
;; Revision 1.5  2002/05/12 06:48:52  paulk
;; Added jde-with-file-contents jde-normalize-paths jde-search-src-dirs
;; jde-find-class-source-file jde-find-class-source functions.
;;
;; Revision 1.4  2002/03/31 07:49:49  paulk
;; Renamed jde-db-source-directories. The new name is jde-sourcepath.
;;
;; Revision 1.3  2002/02/03 06:52:54  paulk
;; Fixed quote bug in jde-assert-source-buffer macro.
;;
;; Revision 1.2  2002/01/19 06:42:22  paulk
;; Minor updates.
;;
;; Revision 1.1  2002/01/04 07:15:20  paulk
;; Initial revision.
;;

;; End of jde-util.el

