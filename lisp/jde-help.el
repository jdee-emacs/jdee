;;; jde-help.el
;; $Revision: 1.72 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>, Phillip Lord <plord@hgmp.mrc.ac.uk>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1999, 2001, 2002, 2003, 2004 Paul Kinnucan.

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

;; (makunbound 'jde-help-docsets)
(defcustom jde-help-docsets
  (list (list "JDK API"
	      "http://java.sun.com/javase/6/docs/api"
	      nil)
	(list "User (javadoc)"
	 "http://static.springsource.org/spring/docs/3.0.x/javadoc-api/"
	 nil)
	(list "User (javadoc)"
	 "http://commons.apache.org/sandbox/functor/apidocs/"
	 nil)
	(list "User (javadoc)"
	 "https://www.hibernate.org/hib_docs/v3/api"
	 nil)
	(list "User (javadoc)"
	 "http://acegisecurity.org/acegi-security/apidocs"
	 nil)
	(list "User (javadoc)"
	 "http://wicket.apache.org/docs/1.4"
	 nil))
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
  
;;(makunbound 'jde-help-remote-file-exists-function)
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

;; (makunbound 'jde-help-wget-tries)
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

;; (makunbound 'jde-help-wget-timeout)
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

;; (makunbound 'jde-help-browser-function)
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
    (browse-url 
    "http://www.beanshell.org/manual/contents.html"
     (if (boundp 
 	 'browse-url-new-window-flag)
         browse-url-new-window-flag
       browse-url-new-window-p)))

;;;###autoload
(defun jde-help-browse-jdk-doc ()
  "Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation."
  (interactive)
  (browse-url 
   (jde-help-get-jdk-doc-url)
   (if (boundp 
	'browse-url-new-window-flag)
       browse-url-new-window-flag
     browse-url-new-window-p)))

(provide 'jde-help)

;; $Log: jde-help.el,v $
;; Revision 1.72  2004/11/25 05:48:20  jslopez
;; Fixes regression bug to jde-help-class-member caused by modification to
;; jde-complete-function.
;;
;; Revision 1.71  2004/10/06 05:10:38  paulk
;; Removed dead code.
;;
;; Revision 1.70  2004/10/06 05:07:43  paulk
;; Fixes bug that caused generation of an extra slash in  URLs for local files on UNIX systems.
;;
;; Revision 1.69  2004/10/06 04:33:58  paulk
;; Add jde-help-browser-function customization variable.
;;
;; Revision 1.68  2004/09/27 04:49:18  paulk
;; Fix jde-help-show-class-member-doc so that it creates a metafile only
;; if the user is using the default Windows browser, i.e., Internet
;; Explorer. This change allows the JDEE to use w3m (specifically,
;; w3m-browse-url) to display Javadoc.
;;
;; Revision 1.67  2004/08/25 12:28:33  paulk
;; Add jde-help-beanshell.
;;
;; Revision 1.66  2004/08/01 11:59:04  paulk
;; Fixed jde-help-show-class-member-doc to call jde-help-show-document from the JDEE buffer instead of the temp buffer to ensure that JDEE customizations are in effect, in particular, browse-url-browser-function. Thanks to Dan Katz.
;;
;; Revision 1.65  2004/03/04 04:43:31  paulk
;; Adds the jde-help-wget-tries, jde-help-wget-timeout, and jde-help-wget-command-line-options
;; variables. Thanks to Nick Sieger.
;;
;; Revision 1.64  2003/08/28 05:18:15  paulk
;; Add jde-help-get-jdk-doc-url function.
;;
;; Revision 1.63  2003/07/15 01:25:06  paulk
;; Use jde-temp-directory to get directory for temporary files.
;;
;; Revision 1.62  2003/02/25 15:01:02  jslopez
;; Modifies jde-parse-get-qualified-name to take an extra parameters.
;; If it does not find the qualified name it tries importing the class.
;; And updates a few places where it is call to do that.
;;
;; Revision 1.61  2003/01/12 05:52:44  paulk
;; Some HTML validity tweaks to jde_meta.html and add window.focus() so the
;; browser window gets raised when new content is loaded into it.
;; Thanks to Ville SkyttÅ‰ <scop@xemacs.org>.
;;
;; Revision 1.60  2002/12/14 04:36:10  paulk
;; The jde-help-show-class-member-doc now opens the jde-metafile.html helper file as a raw file. This avoids loading any html modes associated with html files on a user's system. This in turn fixes a bug whereby displaying JAVA API doc triggers loading of html32-mode which in turn turns on transient mark mode. Thanks to James Cox for this fix.
;;
;; Revision 1.59  2002/09/16 05:05:58  paulk
;; Cygwin Emacs compatibility fix. Check for Cygwin Emacs when processing paths. Thanks
;; to Klaus Berndl.
;;
;; Revision 1.58  2002/07/01 04:52:11  paulk
;; - Moved jde-open-class-source, jde-show-superclass-source, jde-show-interface-source from jde-help.el
;;   to jde-open-source.el.
;;
;; - Removed jde-open-source-for-symbol because it has been superceded by jde-open-class-at-point.
;;
;; Revision 1.57  2002/06/25 15:17:05  jslopez
;; Fixes logic error in jde-help-symbol-internal.
;; It was calling jde-help-show-class-member-doc with an empty doc-file.
;;
;; Revision 1.56  2002/05/29 04:51:15  paulk
;; jde-file-to-url now correctly converts cygwin paths to URLs.
;; Thanks to Michael Lipp <michael.lipp@danet.de>.
;;
;; Revision 1.55  2002/05/13 06:50:35  paulk
;; Removed jde-open-base-class-source as it has been superceded by jde-show-superclass-source.
;;
;; Revision 1.54  2002/04/17 00:13:13  jslopez
;; Updates jde-open-base-class-source to not prompt the user.
;; Updates the use of the variable jde-complete-use-menu.
;;
;; Revision 1.53  2002/04/16 03:17:07  jslopez
;; Integrates jde-open-source.
;;
;; Revision 1.52  2002/03/31 07:49:50  paulk
;; Renamed jde-db-source-directories. The new name is jde-sourcepath.
;;
;; Revision 1.51  2002/03/29 12:49:18  paulk
;; Adds jde-show-interface-source and jde-show-superclass-source. Thanks to
;; Sandip Chitale <sandip.chitale@blazesoft.com>
;;
;; Revision 1.50  2002/03/18 03:50:33  paulk
;; XEmacs compatibility fix to jde-help-show-class-member-doc. Thanks to
;; Dr. Volker Zell <Dr.Volker.Zell@oracle.com>.
;;
;; Revision 1.49  2002/01/28 07:39:00  paulk
;; Updated jde-file-to-url to insert `localhost' into URL.
;;
;; Revision 1.48  2002/01/23 07:23:26  paulk
;; Added jde-open-source-for-symbol functon. Thanks to "Max Rydahl Andersen" <max@eos.dk>.
;;
;; Revision 1.47  2001/12/19 07:51:34  paulk
;; jde-help-show-class-member-doc now kills the temporary buffer for jde_meta.html.
;;
;; Revision 1.46  2001/12/12 05:26:15  paulk
;; Fixed a small bug in jde-help-get-root-dir.
;;
;; Revision 1.45  2001/12/04 05:30:11  paulk
;; Updated to reflect change in dialog class package name prefix from jde- to efc-.
;;
;; Revision 1.44  2001/11/25 06:59:50  paulk
;; Changed doc look up function field in jde-help-docsets to offer a choice between
;; a value of nil and a function. Thanks to Miles Bader.
;;
;; Revision 1.43  2001/11/25 04:39:32  paulk
;; * Restored and extended jde-help-use-frames. Thanks to Javier Lopez.
;; * Changed type of lookup function field of jde-help-docsets from function to
;;   symbol because customize apparently does not support nil as a function
;;   type.
;;
;; Revision 1.42  2001/11/24 15:15:51  paulk
;; * Added a JDK API option to jde-help-docsets and made it the default. This option
;;   specifies the API doc for the currently selected JDK (specified by jde-jdk,
;;   $JAVA_VERSION, or $PATH).
;;
;; Revision 1.41  2001/11/18 06:40:17  paulk
;; Fixed typo in jde-help-docsets.
;;
;; Revision 1.40  2001/11/16 20:14:08  jslopez
;; Fixes bug in jde-help-show-class-member-doc
;; caused by spaces.
;;
;; Revision 1.39  2001/11/14 11:58:52  jslopez
;; Adds customization variable jde-help-use-frames.
;; Modifies the functions jde-help-symbol, jde-help-class,
;; jde-help-class-member to make use of this new variable.
;;
;; Revision 1.38  2001/11/09 03:16:22  jslopez
;; Enhanced jde-help-symbol to handle static calls.
;; i.e. ResourceBundle.getBundle
;;
;; Revision 1.37  2001/11/05 04:58:08  paulk
;; Cosmetic.
;;
;; Revision 1.36  2001/10/31 08:58:44  paulk
;; Emacs 21 compatibility fix: added support for browse-url-new-window-flag variable (replaces browse-url-new-window-p in Emacs 21).
;;
;; Revision 1.35  2001/10/28 17:44:22  jslopez
;; Modifies jde-help-symbol to position the javadoc at the point
;; where the method or field is documented.
;;
;; Revision 1.34  2001/10/28 16:09:38  jslopez
;; Modify jde-help-class-member to use the
;; minubuffer for completion when jde-complete-use-menu is nil
;;
;; Revision 1.33  2001/10/26 06:41:56  paulk
;; Updated to reflect the new modal behavior of jde-option-dialog.
;;
;; Revision 1.32  2001/10/01 19:45:52  jslopez
;; Fixed bug in jde-help-browse-jdk-doc, that was calling jde-normalize-path with no arguments, when
;; jde-jdk-doc-url does not start with http://, file://, or equal to "".
;;
;; Revision 1.31  2001/08/30 04:10:28  paulk
;; Moved jde-browse-jdk-doc to this library and renamed it jde-help-browse-jdk-doc. The command command now looks for the JDK doc at jde-jdk-directory/docs/index.html if it cannot find the doc at jde-jdk-doc-url. The command now also checks for the existence of the doc locally or on the web and errors out if it cannot find it.
;;
;; Revision 1.30  2001/08/10 06:14:01  paulk
;; * Add support for cygwin paths to jde-file-to-url.
;;
;; Revision 1.29  2001/08/09 04:46:55  paulk
;; * XEmacs compatibility fix. Now accommodates the way XEmacs specifies the temp directory. Thanks to Dr. Volker Zell.
;;
;; * Now replaces the colon in the DOS drive prefix with a vertical bar when forming URL's. This is done to accommodate Netscape.
;;
;; Revision 1.28  2001/08/08 05:56:12  paulk
;; Removed prompt from jde-help-symbol.
;;
;; Revision 1.27  2001/08/08 05:22:18  paulk
;; Adds jde-help-class-member command.
;;
;; Revision 1.26  2001/08/04 05:30:20  paulk
;; Fixed jde-help-symbol so that it prompts you to enter the symbol. Also, if more than once class of the same name exists, prompts you to select the appropriate class.
;;
;; Revision 1.25  2001/06/12 07:18:55  paulk
;; Changed jde-parse-declared-type-of to return a qualified type.
;; Thanks to "Evan Easton" <evan@eeaston.com> .
;;
;; Revision 1.24  2001/05/31 05:14:39  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.23  2001/05/31 02:25:39  paulk
;; User can now force the JDE to use either wget or url-file-exists to verify existence of a remote file.
;; Thanks to Luis Miguel Hernanz Iglesias <luish@germinus.com> for suggesting this enhancement.
;;
;; Revision 1.22  2001/04/19 04:39:41  paulk
;; Fixed regression error caused by normalizing paths. Now checks to ensure that path is local before trying to normalize it.
;;
;; Revision 1.21  2001/04/16 05:53:51  paulk
;; Normalized paths.
;;
;; Revision 1.20  2001/04/08 04:11:40  paulk
;; jde-help-find-javadoc now uses url-file-exists (from the w3 package) if it is in your load-path. Otherwise, it uses wget. Thanks to Knut Wannheden <wannhedenk@post.ch> and  klaus.berndl@sdm.de for this fix.
;;
;; Revision 1.19  2001/03/29 02:46:52  paulk
;; Replaced jde-find-exec with executable-find, which is defined by executable.el available with both the Emacs and XEmacs distributions.
;;
;; Revision 1.18  2001/03/27 17:49:33  paulk
;; Eliminate dependency on which package by including the function jde-find-exec and replacing references to the which command with jde-find-exec. Thanks to klaus.berndl@sdm.de for suggesting this change and providing the implementation of jde-find-exec.
;;
;; Revision 1.17  2001/03/27 16:44:50  paulk
;; Updated to require which package. Removed extraneous definition of jde-help-find-javadoc. Thanks to klaus.berndl@sdm.de and Brad Giaccio <bgiaccio@psrw.com> for reporting these problems.
;;
;; Revision 1.16  2001/03/12 05:30:15  paulk
;; Can now access javadoc anywhere on the Web. Thanks to Adrian Robert <arobert@polexis.com> for providing the initial version of this enhancement.
;;
;; Revision 1.15  2001/02/04 01:31:13  paulk
;; Changed declaration of customized variables to permit completion of paths.
;;
;; Revision 1.14  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.13  2000/08/12 04:47:10  paulk
;; Fixed regression error in jde-help-symbol-at-point.
;;
;; Revision 1.12  2000/02/09 05:06:49  paulk
;; Replaced jde-help-class with jde-help-symbol method. The new method
;; gets help for the symbol at point. The symbol may refer to a class,
;; an object, or a method or field.
;;
;; Revision 1.11  2000/02/01 04:11:56  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.10  2000/01/18 07:11:25  paulk
;; Added jde-show-class-source. Thanks to Phil Lord for the initial
;; implementation of this command.
;;
;; Revision 1.9  2000/01/15 08:06:25  paulk
;; Eliminated some globally bound symbols.
;;
;; Revision 1.8  1999/09/30 04:46:10  paulk
;; Fixed typo spotted by David Biesack.
;;
;; Revision 1.7  1999/09/18 03:26:39  paulk
;; Now prepends "file://" to doc file when invoking browse-url. Hopefully
;; this will fix the problem reported by one user where the browser
;; prepends http://www to doc file path.
;;
;; Revision 1.6  1999/08/20 00:44:43  paulk
;; Corrected spelling of Phillip Lord's name.
;;
;; Revision 1.5  1999/06/26 00:00:12  paulk
;; Type javadoc now sufficient to specify both Java 1 and Java 2 javadoc docsets.
;;
;; Revision 1.4  1999/06/25 01:38:17  paulk
;; Enhanced to support doc collections of any type.
;;
;; Revision 1.3  1999/06/17 22:27:33  paulk
;; Bug fix.
;;
;; Revision 1.2  1999/06/17 21:53:05  paulk
;; Eliminated separate customization group for help variables.
;;
;; Revision 1.1  1999/06/17 21:47:15  paulk
;; Initial revision
;;

;; End of jde-help.el
