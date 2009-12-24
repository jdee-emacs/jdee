;;; jde-help.el
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
;;         Phillip Lord <plord@hgmp.mrc.ac.uk>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1999, 2001, 2002, 2003, 2004 Paul Kinnucan.
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
;; Boston, MA 02111-1307, USA.

(require 'beanshell)
(require 'jde-widgets)
(require 'eieio)
(require 'jde-util)

(defcustom jde-help-docsets
  (list (list "JDK API"
	      "http://java.sun.com/javase/6/docs/api"
	      nil)
	)
  "*Lists collections of HTML files documenting Java classes.
This list is used by the `jde-help-class' command to find help for
a class. You can specify the following information for each docset:

Docset type

  The following types are valid:

  * JDK API

    JDK API documentation created by JDK developer (e.g., Sun)

  * User (javadoc)

    User collection in javadoc format.

  * User (not javadoc)

    User collection not in javadoc format.

Docset directory

   Directory containing the collection, e.g., d:/jdk1.3/docs/api.  If
   the docset type is `JDK API' and this field is the empty string, the
   JDE looks for the docset in the `docs/api' subdirectory of the directory
   returned by `jde-get-jdk-dir'. The docset directory may be located on
   a remote system in which case this field should specify the URL of the
   docset directory, e.g., http://www.javasoft.com/j2se/1.3/docs/api. The
   GNU utility, wget, or the w3 function `url-file-exists' must be
   installed on your system to find javadoc pages located on remote
   systems. Native Windows and cygwin ports of wget are readily available
   on the Internet. Make sure that wget is in Emacs `exec-path' before
   attempting to access documentation on remote systems. See
   `jde-help-remote-file-exists-function' for more information.

Doc lookup function

   Should specify a function that accepts a fully qualified class name,
   e.g., java.awt.String, and a docset directory and returns a path to
   an HTML file that documents that class, e.g.,
   d:/jdk1.2/docs/api/java/awt/String.html. This field must be specified
   for non-javadoc collections. It is ignored for javadoc colletions.
"
  :group 'jde-project
  :type '(repeat
	  (list
	   (radio-button-choice
	    :format "%t \n%v"
	    :tag "Docset type:"
	    (const "JDK API")
	    (const "User (javadoc)")
	    (const "User (not javadoc)"))
	   (file :tag "Docset directory")
	   ;;(symbol :tag "Doc lookup function")
	   (choice :tag "Doc lookup function"
		   (const nil)
		   function))))

(defcustom jde-help-use-frames t
   "A non-nil value makes the functions:`jde-help-symbol',
 `jde-help-class-member' and `jde-help-class' use frames when displaying
 the html javadocs."
   :group 'jde-project
   :type 'boolean)

(defcustom jde-help-remote-file-exists-function (list "wget")
  "Specifies the function the jde uses to retrieve remote documents.
wget is a Unix utility available on Windows as part of the Cygwin
package. `url-file-exists' is part of the url Emacs Lisp library,
which is included in the Emacs w3 package."
  :group 'jde-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Function:"
	   (const "wget")
	   (const "url-file-exists")))
  :set '(lambda (sym val)
	  (if (and
	       (string= (car val) "url-file-exists")
	       (locate-library "url"))
	       (autoload 'url-file-exists "url" nil nil nil))
	  (set-default sym val)))

(defcustom jde-help-wget-tries nil
  "Specifies the number of times that the JDEE should try getting
documentation from a remote site This option applies only if wget is
the`jde-help-remote-file-exists-function'. It sets wget's --tries
command-line option. It is helpful in situations where
one of the the sites in `jde-help-docsets' is not always reachable."
  :group 'jde-project
  :type '(choice (const :tag "Try once" :value nil)
		 (const :tag "Never stop trying" :value 0)
		 (integer :tag "Number of retries" :value 1)))

(defcustom jde-help-wget-timeout nil
  "Specifies the length of time that the JDEE should wait for a remote site
to respond to a request for a document. This option applies only if wget is
the`jde-help-remote-file-exists-function'. It sets wget's --timeout
command-line option. It is helpful in situations where
one of the the sites in `jde-help-docsets' is not always reachable."
  :group 'jde-project
  :type '(choice (const :tag "Default (900 s.)" :value nil)
		 (const :tag "Disable timeout checking" :value 0)
		 (integer :tag "Timeout (seconds)" :value 900)))

(defcustom jde-help-wget-command-line-options "--quiet"
  "Specifies additional options (beyond --spider, --tries and
--timeout) to pass to wget, if wget is used for
`jde-help-remote-file-exists-function'."
  :group 'jde-project
  :type 'string)

(defcustom jde-help-browser-function "browse-url"
  "Specifies the function that the JDEE uses to display
HTML pages."
  :group 'jde-project
  :type '(choice
	  (const :tag "browse-url" :value "browse-url")
	  (const :tag "w3m-browse-url" :value "w3m-browse-url")
	  (function :tag "Other" :value identity)))


(defun jde-file-to-url (file)
  "Convert FILE path to a URL. If FILE is a DOS path, this
function replaces the colon in the drive specifier with a
vertical bar (|) because both Internet Explorer and Netscape
accept the resulting URL whereas Netscape does not accept
a drive specifier with a colon."
  (if (or (string-match "http:" file)
	  (string-match "file:" file))
      file
    (format "file://localhost%s"
	    (let ((file (jde-convert-cygwin-path file)))
	      ;; Check for DOS path.
	      (if (string-match "[a-zA-Z]:" file)
		  (concat "/" (substitute ?| ?: file))
		file)))))


(defun jde-help-docset-get-type (docset)
  (nth 0 docset))

(defun jde-help-docset-get-dir (docset)
  (let ((path (nth 1 docset)))
    (if (or (string-match "http:" path)
	    (string= path ""))
	path
      (jde-normalize-path path 'jde-help-docsets))))

(defun jde-help-docset-get-lookup-function (docset)
  (nth 2 docset))

(defun jde-help-find-javadoc (class docset-dir)
  "Searches DOCSET-DIR for the javadoc HTML page
for CLASS and, if found, returns the URL of the
javadoc page for CLASS. This function uses the function
specified by `jde-help-remote-file-exists-function'
to verify the existence of pages located on remote systems."
  (let ((class-path
	 (concat (substitute ?/ ?. class) ".html"))
	url)
    (cond
     ((string-match "http:" docset-dir)
      (setq url (concat docset-dir "/" class-path))
	     (if (string=
		  (car jde-help-remote-file-exists-function)
		  "url-file-exists")
	  (if (fboundp 'url-file-exists)
	      (if (not
		   (url-file-exists url))
		  (setq url nil))
	    (error "Cannot find url-file-exists function"))
	(if (executable-find
	     (if (eq system-type 'windows-nt) "wget.exe" "wget"))
	    (let ((cmd (concat "wget --spider "
			   (if jde-help-wget-tries
			       (concat "--tries=" jde-help-wget-tries))
			   (if jde-help-wget-timeout
			       (concat "--timeout=" jde-help-wget-timeout))
			   jde-help-wget-command-line-options
			       " " url)))
	      (unless (= 0 (shell-command cmd))
		(setq url nil)))
	  (error
	   (concat "Cannot find wget. This utility is needed "
		   "to access javadoc on remote systems.")))))
     (t
      (let ((doc-path
	     (expand-file-name class-path docset-dir)))
	(if (file-exists-p doc-path)
	    (setq url (jde-file-to-url doc-path))))))
    url))

(defun jde-help-get-root-dir (docfile)
  "Returns the javadoc root directory of docfile"
  (if jde-help-docsets
      (let ((docsets jde-help-docsets)
	    dir)
	(while docsets
	  (let ((docset (car docsets)))
	    (setq dir (jde-help-docset-get-dir docset))
	    (if (and
		 (or (not dir)
		     (string= dir ""))
		 (string= (jde-help-docset-get-type docset) "JDK API"))
		(setq dir (expand-file-name "docs/api" (jde-get-jdk-dir))))
	    (setq dir (jde-file-to-url (concat dir "/")))
	    (if (string-match dir docfile)
		(setq docsets nil)
	      (setq docsets (cdr docsets)))))
	  dir)))

(defun jde-help-get-doc (class)
"Gets URL to the HTML file for CLASS where CLASS is a
qualified class name."
  (if class
      (if jde-help-docsets
	(let ((paths
	       (mapcar
		(lambda (docset)
		  (cond
		   ((string= (jde-help-docset-get-type docset) "JDK API")
		    (let ((dir (jde-help-docset-get-dir docset)))
		      (if (string= dir "")
			  (progn
			    (setq dir (expand-file-name "docs/api" (jde-get-jdk-dir)))
			    (if (not (file-exists-p dir))
				(error "Cannot find default JDK API doc directory: %s"
				       dir))))
		      (jde-help-find-javadoc class dir)))
		   ((string= (jde-help-docset-get-type docset) "User (javadoc)")
		    (jde-help-find-javadoc
		     class
		      (jde-help-docset-get-dir docset)))
		   (t
		    (apply
		     (jde-help-docset-get-lookup-function docset)
		     class
		     (list (jde-help-docset-get-dir docset))))))
		jde-help-docsets)))
	  (setq paths (delq nil paths))
	  ;; Return first file found.
	  (if paths (car paths) paths))
	(error "%s" "Help error: No docsets available. See jde-help-docsets."))))


(defun jde-help-symbol-internal (class &optional method-name)
  (let ((classinfo (jde-complete-get-classinfo class))
	(doc-file (jde-help-get-doc class))
	method-signature member pos)
    (if (and method-name classinfo)
	(setq method-signature (jde-complete-find-all-completions
				(list class method-name)
				classinfo)))
    (if method-signature
	(progn
	  (setq member (caar method-signature))
	  (setq pos (string-match " : " member))
	  (if pos
	      (setq member (substring member 0 pos)))))
    (if doc-file
	(jde-help-show-class-member-doc doc-file member)
      (message "Error: cannot find documentation for class %s " class))))

(defun jde-help-symbol ()
  "Displays help for a symbol. The symbol may reference an object, a
class, or a method or field. If the symbol references a class, this
function displays the javadoc for the class. If the symbol references
an object, this method displays the javadoc for the class of the
object. If the symbol references a field or a method, this function
displays the javadoc for the class of the object of which the field or
method is a member at the point where the method of field is
documented."
  (interactive)
  (condition-case err
      (let* ((parse-result (jde-help-parse-symbol-at-point))
	     (unqualified-name (thing-at-point 'symbol))
	     (class-name (jde-parse-get-qualified-name unqualified-name t))
	     (pair (jde-parse-java-variable-at-point)))
	(if (not class-name)
	    (if parse-result
		(progn
		  (setq unqualified-name  (car parse-result))
		  (setq class-name (jde-parse-get-qualified-name unqualified-name t)))))
	(if class-name
	    (jde-help-symbol-internal class-name (cdr parse-result))
	  (if (not (string= (car pair) ""))
	      (progn
		(setq class-name (jde-parse-get-qualified-name (car pair) t))
		(jde-help-symbol-internal class-name unqualified-name))
	    (message "Error: cannot find class '%s' on the current classpath." unqualified-name))))
    (error
     (message "%s" (error-message-string err)))))


(defun jde-help-show-document (doc-url &rest args)
  "Displays DOC-URL in the browser specified by `jde-help-browser-function'."
  (let ((browser-function
	 (cond
	  ((functionp jde-help-browser-function)
	   jde-help-browser-function)
	  ((string= jde-help-browser-function "w3m-browse-url")
	   'w3m-browse-url)
	  (t
	   'browse-url))))
  (apply browser-function doc-url args)))

(defun jde-help-parse-symbol-at-point ()
  "Returns (cons TYPE MEMBER) where TYPE is the declared type of
the object referenced by the (qualified) name at point and MEMBER is the
field or method referenced by the name if qualified."
  (let ((parse-result (jde-parse-qualified-name-at-point)))
    (if parse-result
	(let* ((qualifier (car parse-result))
	       (name (cdr parse-result))
	       (obj (if qualifier qualifier name))
	       (member (if qualifier name)))
	  (if (not
	       (and
		qualifier
		(string-match "[.]" qualifier)))
	      (let ((declared-type (car (jde-parse-declared-type-of obj))))
		(if declared-type
		    (cons declared-type  member))))))))

(defun jde-help-class (&optional class-name)
  "Return help for CLASS."
  (interactive)
  (let* ((class
	  (or class-name
	     (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
	 (fq-class-name
	  (jde-parse-select-qualified-class-name class))
	 (doc-file
	  (if fq-class-name
	      (jde-help-get-doc fq-class-name))))
    (if doc-file
	(jde-help-show-class-member-doc doc-file)
      (message "Error: cannot find documentation for %s" class))))


(defun jde-help-show-class-member-doc (docfile &optional member)
  "Show DOCFILE in the browser defined by `jde-help-browser-function'
where DOCFILE is the class and MEMBER is the anchor for a class
member. If the `jde-help-browser-function' is is not w3m-browse-url,
this function creates a temporary HTML file that redirects the
browser to DOCFILE. This is a workaround made necessary by the fact
that the default browser function for Windows uses the Windows
ShellExecute function to invoke Internet Explorer and for some reason
ShellExecute does not pass the anchor to IE. If `jde-help-use-frames'
is nonnil, this function creates a metafile that displays the
multiframe version of the standard Javadoc page."
  (let* ((anchor (if member (concat docfile "#" member)
		  docfile))
	 (doc-url (jde-file-to-url anchor)))
    (unless (string= jde-help-browser-function "w3m-browse-url")
      (let* ((temp-directory (jde-temp-directory))
	     (metafile (expand-file-name "jde_meta.html" temp-directory))
	     (buff (find-file-noselect metafile nil t))
	     (standard-output buff)
	     (pos (string-match "[^/]*$" docfile))
	     (root (jde-help-get-root-dir docfile)))
	(if buff
	    (progn
	      (save-excursion
		(set-buffer buff)
		(erase-buffer)
		(if jde-help-use-frames
		    (progn
		      (princ "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">\n")
		      (princ "<html>\n<head>\n")
		      (princ "<title>JDE javadoc window</title>\n</head>\n")
		      (princ "<frameset cols=\"20%,80%\" onload=\"window.focus()\">\n")
		      (princ "<frameset rows=\"30%,70%\">\n")
		      (princ
		       (format "<frame src=\"%soverview-frame.html\" name=\"packageListFrame\">\n"
			       root))
		      (princ
		       (format "<frame src=\"%spackage-frame.html\" name=\"packageFrame\">\n"
			       (substring docfile 0 pos)))
		      (princ
		       (format "<frame src=\"%sallclasses-frame.html\" name=\"packageFrame\">\n"
			     root))
		      (princ "</frameset>\n")
		      (princ
		       (format "<frame src=\"%s\" name=\"classFrame\">\n" doc-url))
		      (princ "</frameset>\n")
		      (princ "</html>\n"))
		  (progn
		    (princ "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n")
		    (princ "<html>\n<head>\n")
		    (princ
		     (format "<meta http-equiv=\"Refresh\" content=\"0; URL=%s\">\n"  doc-url))
			     (princ "</head>\n<body onload=\"window.focus()\">\n</body>\n</html>")))
		(save-buffer)
		(kill-buffer buff))
	      (setq doc-url metafile))
	  (error "Unable to create a temporary file in %s directory."
		 temporary-file-directory))))
    (jde-help-show-document doc-url)))

(defun jde-help-popup-class-member-menu (class &optional title)
  "Popup a menu of the fields and methods defined by CLASS.
Return the member selected by the user."
  (let ((classinfo
	  (jde-complete-get-classinfo class)))
    (if classinfo
	(let (pos
	      (pair
	       (if (= (length classinfo) 1)
		   ;; if only one item match, return it
		   (car classinfo)

		 (if (eq jde-complete-function 'jde-complete-menu)
		     ;; delegates menu handling to imenu :-) Popup window at
		     ;; text cursor
		     (imenu--mouse-menu classinfo
					(jde-cursor-posn-as-event)
					(or title "Class Members"))
		   (assoc (completing-read (or title "Completion: ")
					   classinfo)
			  classinfo)))))
	  (setq pos (string-match " : " (car pair)))
	  (if pos
	      (substring (car pair) 0 pos)
	    (cdr pair)))
      (message "Class %s has no members." class))))

(defun jde-help-class-member (&optional class-name)
  "Pop up a menu of the fields and methods of CLASS.
Then search `jde-help-docsets' for javadoc for CLASS. If found,
point the browser to the doc for the selected method or
field. Note: this command does not check whether the doc
for CLASS actually documents the selected class member."
  (interactive)
  (let* ((class
	 (or class-name
	     (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
	 (fq-class-name
	  (jde-parse-select-qualified-class-name class))
	 (doc-file (jde-help-get-doc fq-class-name)))
    (if doc-file
	(let ((member (jde-help-popup-class-member-menu fq-class-name)))
	  (if member
	      (jde-help-show-class-member-doc doc-file member)))
      (message "Error: cannot find documentation for %s" fq-class-name))))


(defun jde-help-get-jdk-doc-url ()
  "Gets a URL for the JDK documentation."
  (cond
   ((string= jde-jdk-doc-url "")
    (let ((path (expand-file-name "docs/index.html" (jde-get-jdk-dir))))
      (if (file-exists-p path)
	  path
	(error "Cannot find JDK documentation at default location: "
	       path))))
   ((string-match "http:" jde-jdk-doc-url)
    (if (or (and
	     (fboundp 'url-file-exists)
	     (not (url-file-exists jde-jdk-doc-url)))
	    (and
	     (executable-find (if (eq system-type 'windows-nt) "wget.exe" "wget"))
	     (not (string-match "200"
				(shell-command-to-string
				 (concat "wget --spider "
					 (if jde-help-wget-tries
					     (concat "--tries=" jde-help-wget-tries))
					 (if jde-help-wget-timeout
					     (concat "--timeout=" jde-help-wget-timeout))
					 jde-help-wget-command-line-options
					 " " jde-jdk-doc-url))))))
	(error "JDK doc does not exist at jde-jdk-doc-url value: %s" jde-jdk-doc-url))
    jde-jdk-doc-url)
   ((string-match "file://" jde-jdk-doc-url)
    (let ((path (substring jde-jdk-doc-url 7)))
      (if (file-exists-p path)
	  path
	(error "JDK doc does not exist at jde-jdk-doc-url value file://%s" path))))
   ((not (string-match jde-jdk-doc-url ""))
    (let ((path (jde-normalize-path jde-jdk-doc-url)))
      (if (file-exists-p path)
	  path
	(error "JDK doc does not exist at jde-jdk-doc-url value %s" path))))
   (t
    (let ((path (expand-file-name "docs/index.html"
				  (jde-normalize-path (jde-get-jdk-dir)))))
      (if (file-exists-p path)
	  path
	(error "Cannot find JDK doc. Please set jde-jdk-directory or jde-jdk-doc-url"))))))

;;;###autoload
(defun jde-help-beanshell ()
  "Displays the BeanShell documentation."
  (interactive)
  (browse-url "http://www.beanshell.org/manual/contents.html"))

;;;###autoload
(defun jde-help-browse-jdk-doc ()
  "Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation."
  (interactive)
  (browse-url (jde-help-get-jdk-doc-url)))

(provide 'jde-help)

;; End of jde-help.el
