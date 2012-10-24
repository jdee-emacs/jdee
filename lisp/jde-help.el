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

(defgroup jde-help nil
  "Java Development Environment"
  :group 'jde-project
  :prefix "jde-help-")

(defcustom jde-help-docsets
  '((nil "http://java.sun.com/javase/6/docs/api" "1.6")
    (nil "http://java.sun.com/j2se/1.5.0/docs/api" "1.5"))
  "*Lists collections of HTML files documenting Java classes.
This list is used by the `jde-help-class' command to find help for
a class. You can specify the following information for each docset:

Description: the description of the Javadoc (i.e. `Hibernate
3.4').  If you leave this as `Derived', the JDEE will try to
create a name for the docset based on the URL or if it is a JDK
Javadoc.

Base URL: the full URL to the Java doc.  Examples:

  http://java.sun.com/javase/6/docs/api
  file:///Users/paul/opt/doc/tech/jdk-1.5/api
  file://c|/doc/jdk-1.5/api (windows on the C: drive)

Docset Type:

  The following types are valid:

  * JDK: JDK API documentation created by JDK developer (e.g., Sun)

  * Third Party: User collection in javadoc format.

JDK Version: The version of the JDK (i.e. 1.5)."
  :group 'jde-help
  :type '(repeat :tag "Documentation Sets"
		 (list :tag "Documentation"
		       (choice :tag "Description"
			       (const :tag "Derived" nil)
			       (string :tag "Named"))
		       (string :tag "Base URL")
		       (radio :tag "Docset Type"
			      (const :tag "Third Party" nil)
			      (string :tag "JDK Version"))))
  :set '(lambda (sym val)
	  (set sym val)
	  (if (boundp 'jde-jdhelper-singleton)
	      (jde-jdhelper-reload-docsets jde-jdhelper-singleton))))

(defcustom jde-help-use-frames t
   "A non-nil value makes the functions:`jde-help-symbol',
 `jde-help-class-member' and `jde-help-class' use frames when displaying
 the html javadocs."
   :group 'jde-help
   :type 'boolean)

(defcustom jde-help-remote-file-exists-function (list "wget")
  "Specifies the function the JDEE uses to retrieve remote documents.
wget is a Unix utility available on Windows as part of the Cygwin
package. `url-file-exists' is part of the url Emacs Lisp library,
which is included in the Emacs w3 package."
  :group 'jde-help
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Function:"
	   (const "wget")
	   (const "url-file-exists")
	   (const "beanshell")))
  :set '(lambda (sym val)
	  (if (and
	       (string= (car val) "url-file-exists")
	       (locate-library "url"))
	       (autoload 'url-file-exists "url" nil nil nil))
	  (set-default sym val)
	  (if (boundp 'jde-jdhelper-singleton)
	      (jde-jdhelper-reload-resolvers jde-jdhelper-singleton))))

(defcustom jde-help-wget-tries nil
  "Specifies the number of times that the JDEE should try getting
documentation from a remote site This option applies only if wget is
the`jde-help-remote-file-exists-function'. It sets wget's --tries
command-line option. It is helpful in situations where
one of the the sites in `jde-help-docsets' is not always reachable."
  :group 'jde-help
  :type '(choice (const :tag "Try once" :value nil)
		 (const :tag "Never stop trying" :value 0)
		 (integer :tag "Number of retries" :value 1)))

(defcustom jde-help-wget-timeout nil
  "Specifies the length of time that the JDEE should wait for a remote site
to respond to a request for a document. This option applies only if wget is
the`jde-help-remote-file-exists-function'. It sets wget's --timeout
command-line option. It is helpful in situations where
one of the the sites in `jde-help-docsets' is not always reachable."
  :group 'jde-help
  :type '(choice (const :tag "Default (900 s.)" :value nil)
		 (const :tag "Disable timeout checking" :value 0)
		 (integer :tag "Timeout (seconds)" :value 900)))

(defcustom jde-help-wget-command-line-options "--quiet"
  "Specifies additional options (beyond --spider, --tries and
--timeout) to pass to wget, if wget is used for
`jde-help-remote-file-exists-function'."
  :group 'jde-help
  :type 'string)

(defcustom jde-help-browser-function "browse-url"
  "Specifies the function that the JDEE uses to display
HTML pages."
  :group 'jde-help
  :type '(choice
	  (const :tag "browse-url" :value "browse-url")
	  (const :tag "w3m-browse-url" :value "w3m-browse-url")
	  (function :tag "Other" :value identity)))




(defclass jde-url ()
  ((protocol :initarg :protocol
	     :initform nil
	     :protection protected
	     )
   (host :initarg :host
	 :initform nil
	 :protection protected
	 )
   (file :initarg file
	 :initform nil
	 :protection protected)
   ))

(defmethod jde-url-parse ((this jde-url) &optional field)
  (with-slots (protocol host file) this
    (let ((name (object-name-string this)))
      (if (null name) (error "No URL given in `jde-url' instance"))
      (when (null protocol)
	(if (string-match "^\\(.+\\)://\\(.*?\\)\\(\\/.*\\)?$" name)
	    (setq protocol (match-string 1 name)
		  host (match-string 2 name)
		  file (match-string 3 name))))))
  (if field (slot-value this field)))

(defmethod object-print ((this jde-url) &optional strings)
  (jde-url-parse this)
  (apply 'call-next-method this
	 (cons (concat " " (mapconcat #'(lambda (slot)
					  (format "%S=%s" slot (slot-value this slot)))
				      '(protocol host file)
				      " "))
	       strings)))

(defmethod jde-url-name ((this jde-url)) (object-name-string this))
(defmethod jde-url-protocol ((this jde-url)) (jde-url-parse this 'protocol))
(defmethod jde-url-host ((this jde-url)) (jde-url-parse this 'host))
(defmethod jde-url-file ((this jde-url)) (jde-url-parse this 'file))


(defclass jde-jddocset ()
  ((description :initarg :description
		:type (or null string)
		:initform nil
		:documetation "The description of the docset."
		)
   (url :initarg :url
	:type jde-url
	:documentation "The \(base) URL to the doc set."
	)
   (jdkp :initarg :jdkp
	 :initform nil
	 :type boolean
	 :documetation "Whether or not this is a JDK doc set.
This defaults to false."
	 )
   (version :initarg :version
	    :type (or null string)
	    :documentation "The version (i.e. 1.5 for 1.5 JDK docs)."
	    )
   ))

(defmethod initialize-instance ((this jde-jddocset) &rest rest)
  (apply 'call-next-method this rest)
  (unless (oref this :description)
    (oset this :description
	  (if (oref this :jdkp)
	      (format "JDK %s Javadoc" (oref this :version))
	    (let ((file (jde-url-file (oref this :url))))
	      (if (string-match ".*\\/\\(.*?\\)\\/doc\\/api" file)
		  (match-string 1 file)
		(jde-url-name (oref this :url))))))))


(defclass jde-jdurl (jde-url)
  ((class :initarg :class
	  :initform nil
	  :type (or null string)
	  :documentation "Java class name."
	  )
   (member :initarg :member
	   :initform nil
	   :type (or null string)
	   :documentation "Java member name."
	   )
   (docset :initarg :docset
	   :initform nil
	   :type (or null jde-jddocset)
	   :documentation "The docset from which this url was created."
	   )
   ))

(defmethod jde-url-name ((this jde-jdurl))
  (jde-url-member-url-name this))

(defmethod jde-url-file ((this jde-jdurl))
  (let ((file (jde-url-parse this 'file)))
    (concat file
	    (if (string-equal "/" (substring file -1)) "" "/")
	    (concat (substitute ?/ ?. (oref this :class)) ".html"))))

(defmethod jde-url-docset-url-name ((this jde-jdurl))
  (object-name-string this))

(defmethod jde-url-append-file-name ((this jde-jdurl) filename)
  (let ((urlname (jde-url-docset-url-name this)))
    (format "%s%s%s"
	    urlname
	    (if (string-equal "/" (substring urlname -1))
		"" "/")
	    filename)))

(defmethod jde-url-class-url-name ((this jde-jdurl))
  (jde-url-append-file-name
   this
   (concat (substitute ?/ ?. (oref this :class)) ".html")))

(defmethod jde-url-member-url-name ((this jde-jdurl))
  (if (oref this :member)
      (format "%s#%s"
	      (jde-url-class-url-name this)
	      (oref this :member))
    (jde-url-class-url-name this)))
  

(defclass jde-jdurl-resolver () ()
  :abstract true)

(defmethod jde-jdurl-resolver-urls ((this jde-jdurl-resolver) class docset)
  (let ((urlname (jde-url-name (oref docset :url))))
    (list (jde-jdurl urlname
		     :class class
		     :docset docset))))

(defmethod jde-jdurl-resolver-exists ((this jde-jdurl-resolver) class docset)
  (let ((urls (jde-jdurl-resolver-urls this class docset)))
    (car (memq t (mapcar #'(lambda (url)
			     (jde-jdurl-resolver-url-exists this url))
			 urls)))))


(defclass jde-jdurl-fs-resolver (jde-jdurl-resolver) ())  

(defmethod jde-jdurl-resolver-url-exists ((this jde-jdurl-fs-resolver) url)
  (and (equal "file" (jde-url-protocol url))
       (file-exists-p (jde-url-file url))))


(defclass jde-jdurl-w3-resolver (jde-jdurl-resolver) ())  

(defmethod jde-jdurl-resolver-url-exists ((this jde-jdurl-w3-resolver) url)
  (if (fboundp 'url-file-exists)
      (url-file-exists url)
    (error "Cannot find url-file-exists function")))


(defclass jde-jdurl-wget-resolver (jde-jdurl-resolver)
  ((executable :initarg :executable
	       :type string
	       :documentation "The wget program."
	       )
   (tries :initarg :tries
	  :type (or null number)
	  )
   (timeout :initarg :timeout
	    :type (or null number)
	    )
   (options :initarg :options
	    :type (or null string)
	    )
   ))

(defmethod initialize-instance ((this jde-jdurl-wget-resolver) &rest rest)
  (apply 'call-next-method this rest)
  (unless (slot-boundp this :executable)
    (let ((exec (executable-find
		 (if (eq system-type 'windows-nt) "wget.exe" "wget"))))
      (unless exec
	(error "Cannot find wget. You might want to use the beanshell resolver instead."))
      (oset this :executable exec)))
  (dolist (elt '((:tries . jde-help-wget-tries)
		 (:timeout . jde-help-wget-timeout)
		 (:options . jde-help-wget-command-line-options)))
    (unless (slot-boundp this (car elt))
      (set-slot-value this (car elt) (symbol-value (cdr elt))))))

(defmethod jde-jdurl-resolver-url-exists ((this jde-jdurl-wget-resolver) url)
  (with-slots (tries timeout options) this
    (let ((cmd (concat "wget --spider "
		       (if tries (concat "--tries=" tries))
		       (if timeout (concat "--timeout=" timeout))
		       options " " (jde-url-name url))))
      (and (= 0 (shell-command cmd)) t))))

(defclass jde-jdurl-beanshell-resolver (jde-jdurl-resolver) ())

(defmethod jde-jdurl-resolver-url-exists ((this jde-jdurl-beanshell-resolver) url)
  (require 'jde-bsh)
  (let ((cmd (format "\
java.net.URLConnection conn = null;
String urlStr = \"%s\";
try {
  URL url = new URL(urlStr);
  conn = url.openConnection();
  conn.getInputStream().close();
  print(\"t\");
} catch(java.net.MalformedURLException e) {
  print(\"(error \\\"Bad URL: \" + urlStr + \"\\\")\");
} catch(java.io.IOException e) {
  String msg = e.toString().replace(\"\\\"\", \"\\\\\\\"\");
  print(\"nil\");
} finally {
  if (conn instanceof HttpURLConnection) conn.disconnect();
}"
		     (jde-url-name url))))
    (eval (read (jde-jeval cmd)))))


(defclass jde-jdurl-stack-resolver (jde-jdurl-resolver)
  ((resolvers :initarg :resolvers
	      :type list)))

(defmethod jde-jdurl-resolver-urls ((this jde-jdurl-stack-resolver)
				    class docset)
  ;; we inherit from a class we know always gets only one url
  (let* ((url (car (call-next-method this class docset)))
	 (resolvers (jde-jdurl-resolver-matching-resolvers this url))
	 urls)
    (dolist (resolver resolvers)
      (setq urls (nconc urls (jde-jdurl-resolver-urls resolver class docset))))
    urls))

(defmethod jde-jdurl-resolver-matching-resolvers ((this jde-jdurl-stack-resolver) url)
  (let (resolvers)
    (dolist (resolver (oref this :resolvers))
      (if (jde-jdurl-resolver-url-exists resolver url)
	  (setq resolvers (nconc resolvers (list resolver)))))
    resolvers))

(defmethod jde-jdurl-resolver-url-exists ((this jde-jdurl-stack-resolver) url)
  (> (length (jde-jdurl-resolver-matching-resolvers this url)) 0))

(defclass jde-jdhelper ()
  ((docsets :initarg :docsets
	    :initform nil
	    ;; this is a list of `jde-jddocset' instances
	    :type list
	    )
   (resolver :initarg :resolver
	     :initform nil
	     :type (or null jde-jdurl-resolver)
	     )
   ))

(defmethod initialize-instance ((this jde-jdhelper) &rest rest)
  (apply 'call-next-method this rest)
  (unless (oref this :docsets)
    (jde-jdhelper-reload-docsets this))
  (unless (oref this :resolver)
    (jde-jdhelper-reload-resolvers this)))

(defmethod jde-jdhelper-reload-resolvers ((this jde-jdhelper))
  (let* ((func (intern (car jde-help-remote-file-exists-function)))
	 (resolver (case func
		     (wget 'jde-jdurl-wget-resolver)
		     (w3 'jde-jdurl-w3-resolver)
		     (beanshell 'jde-jdurl-beanshell-resolver)
		     (t (error "No such remote function: %S" func)))))
    (oset this :resolver
	  (jde-jdurl-stack-resolver nil :resolvers
				    (list (jde-jdurl-fs-resolver nil)
					  (funcall resolver nil))))))

(defmethod jde-jdhelper-reload-docsets ((this jde-jdhelper))
  (oset this :docsets
	(mapcar #'(lambda (elt)
		    (let* ((version (third elt))
			   (jdkp (and version t))
			   (urlstr (second elt)))
		      (if (null urlstr)
			  (error "No url for docset in `jde-help-docs': %S"
				 jde-help-docsets))
		      (jde-jddocset nil
				    :description (first elt)
				    :url (jde-url urlstr)
				    :version version
				    :jdkp jdkp)))
		jde-help-docsets)))

(defmethod jde-jdhelper-urls-for-class ((this jde-jdhelper) class)
  (with-slots (docsets resolver) this  
    (mapcan #'(lambda (docset)
		(let ((ver (oref docset :version)))
		  (if (or (not ver) (equal ver (car jde-jdk)))
		      (jde-jdurl-resolver-urls resolver class docset))))
	    docsets)))

(defmethod jde-jdhelper-jdk-url ((this jde-jdhelper))
  (with-slots (docsets resolver) this  
    (dolist (docset docsets)
      (if (equal (oref docset :version) (car jde-jdk))
	  (return (oref docset :url))))))

(defmethod jde-jdhelper-read-url ((this jde-jdhelper) class)
  (let ((urls (jde-jdhelper-urls-for-class this class)))
    (cond ((null urls)
	   (error "Cannot find documentation for %s" class))
	  ((= 1 (length urls))
	   (car urls))
	  (t (let ((def (car jde-help-read-url-history))
		   (alist (mapcar
			   #'(lambda (url)
			       (cons (oref (oref url :docset) :description)
				     url))
			   urls)))
	       (cdr (assoc (completing-read
			    (jde-create-default-prompt "Docset" def)
			    (mapcar 'car alist)
			    nil t nil 'jde-help-read-url-history def)
			   alist)))))))

(defmethod jde-jdhelper-show-class ((this jde-jdhelper) class &optional member)
  (let ((url (jde-jdhelper-read-url this class)))
    (oset url :member member)
    (jde-jdhelper-show-url this url)))

(defmethod jde-jdhelper-show-url ((this jde-jdhelper) url)
  (let ((doc-url (jde-url-name url)))
    (unless (string= jde-help-browser-function "w3m-browse-url")
      (let* ((temp-directory (jde-temp-directory))
	     (csfile "jde_meta.html")
	     (metafile (expand-file-name csfile temp-directory))
	     (class-url (jde-url-class-url-name url))
	     (pos (string-match "[^/]*$" class-url))
	     (package-file (substring class-url 0 pos))
	     buf standard-output)
	(if (null metafile)
	    (error "Unable to create a temporary file in %s directory."
		   temporary-file-directory))
	(setq buf (find-file-noselect metafile nil t)
	      standard-output buf)
	(with-current-buffer buf
	  (erase-buffer)
	  (if jde-help-use-frames
	      (progn
		(princ "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">\n")
		(princ "<html>\n<head>\n")
		(princ
		 (format "<title>%s (%s)</title>\n</head>\n"
			 (oref (oref url :docset) :description)
			 (oref url :class)))
		(princ "<frameset cols=\"20%,80%\" onload=\"window.focus()\">\n")
		(princ "<frameset rows=\"30%,70%\">\n")
		(princ
		 (format "<frame src=\"%s\" name=\"packageListFrame\">\n"
			 (jde-url-append-file-name url "overview-frame.html")))
		(princ
		 (format "<frame src=\"%spackage-frame.html\" name=\"packageFrame\">\n"
			 package-file))
		(princ
		 (format "<frame src=\"%s\" name=\"packageFrame\">\n"
			 (jde-url-append-file-name url "allclasses-frame.html")))
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
	  (save-buffer))
	(kill-buffer buf)
	(setq doc-url metafile)))
    (message "Displaying %s from %s"
	     (oref url :class)
	     (oref (oref url :docset) :description))
    (jde-jdhelper-show-document this doc-url)))

(defmethod jde-jdhelper-show-document ((this jde-jdhelper) doc-url &rest args)
  "Displays DOC-URL in the browser specified by `jde-help-browser-function'."
  (let ((browser-function
	 (cond
	  ((functionp jde-help-browser-function)
	   jde-help-browser-function)
	  ((string= jde-help-browser-function "w3m-browse-url")
	   'w3m-browse-url)
	  (t 'browse-url))))
    (apply browser-function doc-url args)))

(defmethod jde-jdhelper-describe-docsets ((this jde-jdhelper))
  (let* ((cols (mapcar #'(lambda (docset)
			   (list (oref docset :description)
				 (jde-url-name (oref docset :url))
				 (if (oref docset :jdkp)
				     (oref docset :version))))
		       (oref this :docsets)))
	 (max-len (apply 'max (mapcar #'(lambda (col)
					  (length (first col)))
				      cols))))
    (with-current-buffer (get-buffer-create "*JDEE Docsets*")
      (erase-buffer)
      (dolist (col cols)
	(insert (format "%s: %s%s%s\n"
			(first col)
			(make-string (- max-len (length (first col))) ? )
			(second col)
			(if (third col)
			    (format " (ver %s)" (third col))
			  ""))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defvar jde-jdhelper-singleton (jde-jdhelper nil)
  "The JDHelper singleton instance.")


;; interactive



(defvar jde-help-read-url-history nil)

(defun jde-help-describe-docsets ()
  (interactive)
  (let ((this jde-jdhelper-singleton))
    (jde-jdhelper-describe-docsets this)))

(defun jde-help-class (class)
  "Return help for fully qualified CLASS."
  (interactive (list (jde-read-class)))
  (jde-jdhelper-show-class jde-jdhelper-singleton class))

(defun jde-help-class-member (class member)
  "Pop up a menu of the fields and methods of CLASS.
Then search `jde-help-docsets' for javadoc for CLASS. If found,
point the browser to the doc for the selected method or
field. Note: this command does not check whether the doc
for CLASS actually documents the selected class member."
  (interactive
   (let* ((class (jde-read-class))
	  (member (jde-help-popup-class-member-menu class)))
     (list class member)))
  (jde-jdhelper-show-class jde-jdhelper-singleton class member))

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
  (flet ((show-symbol
	  (class method-name)
	  (let ((urls (jde-jdhelper-urls-for-class jde-jdhelper-singleton class)))
	    (if (null urls)
		(message "Error: cannot find documentation for class %s " class)
	      (let ((classinfo (jde-complete-get-classinfo class))
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
		(jde-jdhelper-show-class jde-jdhelper-singleton class member))))))
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
	      (show-symbol class-name (cdr parse-result))
	    (if (not (string= (car pair) ""))
		(progn
		  (setq class-name (jde-parse-get-qualified-name (car pair) t))
		  (show-symbol class-name unqualified-name))
	      (message "Error: cannot find class '%s' on the current classpath." unqualified-name))))
      (error
       (message "%s" (error-message-string err))))))

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

;;;###autoload
(defun jde-help-beanshell ()
  "Displays the BeanShell documentation."
  (interactive)
  (browse-url "http://www.beanshell.org/manual/contents.html"))

;;;###autoload
(defun jde-help-browse-jdk-doc ()
  "Displays the JDK doc in a web browser."
  (interactive)
  (let ((url (jde-jdhelper-jdk-url jde-jdhelper-singleton))
	urlstr)
    (setq url (jde-jdurl (jde-url-name url)))
    (setq urlstr (jde-url-append-file-name url "index.html"))
    (browse-url urlstr)))

(provide 'jde-help)

;; End of jde-help.el
