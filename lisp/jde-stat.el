;;; jde-stat.el -- Integrated Development Environment for Java.
;; $Revision: 1.7 $ $Date: 2003/03/28 05:33:30 $ 

;; Author: Stephane Nicolas <s.nicolas@videotron.ca>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 2000, 2002 Stephane Nicolas, Paul Kinnucan

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

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

;;;###autoload
(defun jde-stat-loc-report (&optional count &optional total-files)
  "Generates a report showing the number of code, comment,
javadoc, and blank lines in the current Java source buffer. Optionally
a total count could be passed to be displayes, as well as the number of
processed files."
  (interactive)
  (flet ((perc2x2 (a b)
		  (format "%.1f" (* 100(/ (float a) (float b))))))
    (let* ((fname (buffer-file-name))
	   (result (if count count (jde-stat-count-loc)))
	   (total (nth 0 result))
	   (comment (nth 1 result)) 
	   (javadoc (nth 2 result))
	   (blank (nth 3 result))
	   (code (- total (+ comment javadoc blank)))
	   (code-perc (perc2x2 code total))
	   (doc-perc (perc2x2 comment total))
	   (jdoc-perc (perc2x2 javadoc total))
	   (blank-perc (perc2x2 blank total)))
      (with-output-to-temp-buffer "LOC Report"
	(princ "Lines of Code Report\n\n")
        (if (and count total-files)
            (princ (format "Total files:  %d\n" total-files))
          (progn 
            (princ (format "File name: %s\n" fname))
            (princ (format "File date: %s\n" (format-time-string "%D" (nth 5 (file-attributes fname)))))))
	(princ "------------------- \n")
	(princ (format "Code lines:    %d (%s%%)\n" code code-perc))
	(princ (format "Javadoc lines: %d (%s%%)\n" javadoc jdoc-perc))
	(princ (format "Comment lines: %d (%s%%)\n" comment doc-perc))
	(princ (format "Blank lines:   %d (%s%%)\n" blank blank-perc))
	(princ (format "Total lines:   %d  \n" total))
	(princ "")))))


(defun jde-stat-parse-token-out-of-quote (token line)
  (let (result)
     ;;;Does the line contain '//'?
    (if (string-match token line)
         ;;;if so, does it contain '"'
	(if (not (string-match "\"" line ))
             ;;;no! Ok, it's a comment
	    (setq result t)
           ;;;yes!? so, we've got to parse to see if '//' exists without enclosing quote
	  (let (
                ;;;we split the line in '"' delimited parts
		(to-parse (split-string line "\""))
		(temp "")
		(count-even 0))
            ;;;we consider the even numbered parts of split
            ;;;to see if the contain a'//'
            ;;;if so, this is a doc line
	    (while temp
	      (setq temp (nth count-even to-parse))
	      (if temp
		  (if (string-match token temp)
		      (progn 
			(setq result t)
			(setq temp nil))))
	      (setq count-even (+ 2  count-even))))))
    result))

(defun jde-stat-count-loc ()
  "Counts the code, comments, javadoc, and blank lines in the current buffer.
Returns the counts in a list: (TOTAL-LINES COMMENT-LINES JAVADOC-LINES BLANK-LINES)."
  (let ((count 0)
	(line "")
	(javadoc-count 0)
	(comment-count 0)
	(blank-count 0)
	in-javadoc
	in-comment
        (test-b t)
	start
	end)
    (save-excursion
      (goto-char (point-min))
      (while test-b
	(beginning-of-line 1)
	(setq start (point))
	(end-of-line 1)
	(setq end (point))
	(setq line (buffer-substring start end))
	(setq count (+ 1 count))

      ;;;To match a blank line, we search the pattern representing an empty line 
      ;;;or a line that just contains spaces
	(if (string-match "^ *$" line)
	    (setq blank-count (+ 1 blank-count) ))

      ;;;To match a comment line, we search the pattern '//' 
      ;;;but we must disgard the '//' patterns enclosed in a pair of quote '"'
	(if (jde-stat-parse-token-out-of-quote "//" line)
	    (setq comment-count (+ 1 comment-count))) 

      ;;;To match a comment block start, we search the pattern '/*' and exclude those of type '/**' 
      ;;;but we must disgard the '/*' patterns enclosed in a pair of quote '"'
	(if (and 
	     (jde-stat-parse-token-out-of-quote "/\\*" line)
	     (not (jde-stat-parse-token-out-of-quote "/\\*\\*" line)))
	    (setq in-comment t)) 

      ;;;To match a javadoc block start, we search the pattern '/**'
      ;;;but we must disgard the '/**' patterns enclosed in a pair of quote '"'
	(if (jde-stat-parse-token-out-of-quote "/\\*\\*" line)
	    (setq in-javadoc t)) 

      ;;;To match a block end, we search the pattern '*/'
      ;;;but we must disgard the '*/' patterns enclosed in a pair of quote '"'
	(if (jde-stat-parse-token-out-of-quote "\\*/" line)
	    (progn  
	      (if in-javadoc
		  (setq javadoc-count (+ 1 javadoc-count)))
	      (if in-comment
		  (setq comment-count (+ 1 comment-count)))
	      (setq in-javadoc nil)
	      (setq in-comment nil)))
	(if in-javadoc
	    (setq javadoc-count (+ 1 javadoc-count)))
	(if in-comment 
	    (setq comment-count (+ 1 comment-count)))
	(if (not (= (forward-line 1) 0))
	    (setq test-b nil))))
    (list count comment-count javadoc-count blank-count))) 
 
;;;###autoload
(defun jde-stat-loc-report-project (dir)
  "Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory and subdirectories. This method will kill any
buffer containing a java file contained in dir."
  (interactive "D directory name: ")
  (let ((count (jde-stat-loc-count-directories dir)))
    (jde-stat-loc-report (car count) (cadr count))))

(defun jde-stat-loc-count-directories (dir)
  "Counts the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory and subdirectories. This method will kill any buffer
containing a java file contained in dir. It returns a list containing
two elements, a list of the number of code lines, comment lines,
javadoc lines and blank lines and the number fo files."
  (let* ((directories (jde-stat-get-directories dir))
         (count (jde-stat-loc-count-directory dir))
         (current-count (car count))
         (number-of-files (cadr count)))
    (while directories
      (setq count (jde-stat-loc-count-directories (car directories)))
      (setq current-count (jde-stat-add current-count (car count)))
      (setq number-of-files (+ number-of-files (cadr count)))
      (setq directories (cdr directories)))
    (list current-count number-of-files)))
      
(defun jde-stat-get-directories (dir)
  "Returns a list of the subdirectories in dir."
  (let (result)
    (apply 'nconc
           (mapcar (function
                    (lambda (file)
                      (if (and (not (string= (substring file -1) "."))
                               (file-directory-p file))
                          (setq result (append result (list file))))))
                   (directory-files dir t nil t)))
    result))

;;;###autoload
(defun jde-stat-loc-report-directory (dir)
  "Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory. This method will kill any buffer containing a java file
contained in dir."
  (interactive "D directory name: ")
  (let ((count (jde-stat-loc-count-directory dir)))
    (jde-stat-loc-report (car count) (cadr count))))

(defun jde-stat-loc-count-directory (dir)
  "Counts the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory. This method will kill any buffer containing a java file
contained in dir. It returns a list containing two elements,
a list of the number of code lines, comment lines,
javadoc lines and blank lines and the number fo files."
  (let* ((files (directory-files dir t (wildcard-to-regexp "*.java") t))
         (number-of-files (length files))
         (count (list 0 0 0 0)))
    (while files
      (switch-to-buffer (find-file-noselect (car files) nil t))
      (setq count (jde-stat-add count (jde-stat-count-loc)))
      (kill-buffer (current-buffer))
      (setq files (cdr files)))
    (list count number-of-files)))

(defun jde-stat-add (count count2)
  "It takes as an argument two lists of 4 elements. The first element
is the total number of lines, the second the number of comment lines,
the third the number of javadoc lines, and fourth the number of
blank lines. It adds the respective elements in each list and returns another
list of four elements."
  (list (+ (nth 0 count) (nth 0 count2))
        (+ (nth 1 count) (nth 1 count2))
        (+ (nth 2 count) (nth 2 count2))
        (+ (nth 3 count) (nth 3 count2))))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-stat)

;; Change History
;;
;; $Log: jde-stat.el,v $
;; Revision 1.7  2003/03/28 05:33:30  andyp
;; XEmacs optimizations for JDEbug and efc.
;;
;; Revision 1.6  2002/11/21 04:18:41  paulk
;; These packages, when autoloaded, now register and initialize the customization variables
;; that they define to the values specified in the current project file.
;;
;; Revision 1.5  2002/10/11 05:53:21  paulk
;; Added more packages to the list of packages that are demand loaded. This is intended to reduce the startup time for the JDEE.
;;
;; Revision 1.4  2002/10/01 05:43:10  paulk
;; Made commands defined by this package autoloadable.
;;
;; Revision 1.3  2001/08/28 12:53:23  jslopez
;; Fixing typo in the documentation for jde-stat-loc-report-project.
;; Formatting the documentation of jde-stat-loc-count-directories and
;; jde-stat-loc-count-directory.
;;
;; Revision 1.2  2001/08/28 00:50:33  jslopez
;; Modified jde-stat-loc-report to take a count and a total number of files as
;; arguments.
;; Added jde-stat-loc-report-project and jde-stat-loc-report directory.
;;
;; Revision 1.1  2000/07/28 05:59:43  paulk
;; Initial revision. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>
;; for contributing the initial version of this package.
;;
;;
;; End of jde-stat.el
