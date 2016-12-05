;;; jdee-help.el --- Functions for showing docs

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

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'jdee-widgets)
(require 'jdee-util)

;; FIXME: refactor
(defvar jdee-jdk)
(defvar jdee-complete-function)
(declare-function jdee-complete-get-classinfo "jdee-complete" (name &optional access-level))
(declare-function jdee-complete-find-all-completions "jdee-complete" (pair lst &optional exact-match))
(declare-function jdee-cursor-posn-as-event "jdee-complete" ())

(defgroup jdee-help nil
  "Java Development Environment"
  :group 'jdee-project
  :prefix "jdee-help-")

(defcustom jdee-help-docsets
  '((nil "http://docs.oracle.com/javase/8/docs/api" "1.8")
    (nil "http://docs.oracle.com/javase/7/docs/api" "1.7")
    (nil "http://docs.oracle.com/javase/6/docs/api" "1.6")
    (nil "http://docs.oracle.com/j2se/1.5.0/docs/api" "1.5"))
  "*Lists collections of HTML files documenting Java classes.
This list is used by the `jdee-help-class' command to find help for
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
  :group 'jdee-help
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
	  (if (boundp 'jdee-jdhelper-singleton)
	      (jdee-jdhelper-reload-docsets jdee-jdhelper-singleton))))

(defcustom jdee-help-use-frames t
   "A non-nil value makes the functions:`jdee-help-symbol',
 `jdee-help-class-member' and `jdee-help-class' use frames when displaying
 the html javadocs."
   :group 'jdee-help
   :type 'boolean)

;; FIXME: this default avoids errors from jdee-jdhelper-singleton initialization at byte-compile time.
(defcustom jdee-help-remote-file-exists-function (list "url-http-file-exists-p")
  "Specifies the function the JDEE uses to retrieve remote documents.
wget is a Unix utility available on Windows as part of the Cygwin
package. `url-http-file-exists-p' is part of the url Emacs Lisp library,
which is included in the Emacs w3 package."
  :group 'jdee-help
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Function:"
	   (const "wget")
	   (const "url-http-file-exists-p")
	   (const "beanshell")))
  :set '(lambda (sym val)
	  (if (and
	       (string= (car val) "url-http-file-exists-p")
	       (locate-library "url-http"))
	       (autoload 'url-http-file-exists-p "url-http" nil nil nil))
	  (set-default sym val)
	  (if (boundp 'jdee-jdhelper-singleton)
	      (jdee-jdhelper-reload-resolvers jdee-jdhelper-singleton))))

(defcustom jdee-help-wget-tries nil
  "Specifies the number of times that the JDEE should try getting
documentation from a remote site This option applies only if wget is
the`jdee-help-remote-file-exists-function'. It sets wget's --tries
command-line option. It is helpful in situations where
one of the the sites in `jdee-help-docsets' is not always reachable."
  :group 'jdee-help
  :type '(choice (const :tag "Try once" :value nil)
		 (const :tag "Never stop trying" :value 0)
		 (integer :tag "Number of retries" :value 1)))

(defcustom jdee-help-wget-timeout nil
  "Specifies the length of time that the JDEE should wait for a remote site
to respond to a request for a document. This option applies only if wget is
the`jdee-help-remote-file-exists-function'. It sets wget's --timeout
command-line option. It is helpful in situations where
one of the the sites in `jdee-help-docsets' is not always reachable."
  :group 'jdee-help
  :type '(choice (const :tag "Default (900 s.)" :value nil)
		 (const :tag "Disable timeout checking" :value 0)
		 (integer :tag "Timeout (seconds)" :value 900)))

(defcustom jdee-help-wget-command-line-options "--quiet"
  "Specifies additional options (beyond --spider, --tries and
--timeout) to pass to wget, if wget is used for
`jdee-help-remote-file-exists-function'."
  :group 'jdee-help
  :type 'string)

(defcustom jdee-help-browser-function "browse-url"
  "Specifies the function that the JDEE uses to display
HTML pages."
  :group 'jdee-help
  :type '(choice
	  (const :tag "browse-url" :value "browse-url")
	  (const :tag "w3m-browse-url" :value "w3m-browse-url")
	  (function :tag "Other" :value identity)))




(defclass jdee-url ()
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

(defmethod jdee-url-parse ((this jdee-url) &optional field)
  (with-slots (protocol host file) this
    (let ((name (object-name-string this)))
      (if (null name) (error "No URL given in `jdee-url' instance"))
      (when (null protocol)
	(if (string-match "^\\(.+\\)://\\(.*?\\)\\(\\/.*\\)?$" name)
	    (setq protocol (match-string 1 name)
		  host (match-string 2 name)
		  file (match-string 3 name))))))
  (if field (slot-value this field)))

(defmethod object-print ((this jdee-url) &optional strings)
  (jdee-url-parse this)
  (apply 'call-next-method this
	 (cons (concat " " (mapconcat #'(lambda (slot)
					  (format "%S=%s" slot (slot-value this slot)))
				      '(protocol host file)
				      " "))
	       strings)))

(defmethod jdee-url-name ((this jdee-url)) (object-name-string this))
(defmethod jdee-url-protocol ((this jdee-url)) (jdee-url-parse this 'protocol))
(defmethod jdee-url-host ((this jdee-url)) (jdee-url-parse this 'host))
(defmethod jdee-url-file ((this jdee-url)) (jdee-url-parse this 'file))


(defclass jdee-jddocset ()
  ((description :initarg :description
		:type (or null string)
		:initform nil
		:documetation "The description of the docset."
		)
   (url :initarg :url
	:type jdee-url
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

(defmethod initialize-instance ((this jdee-jddocset) &rest rest)
  (apply 'call-next-method this rest)
  (unless (oref this :description)
    (oset this :description
	  (if (oref this :jdkp)
	      (format "JDK %s Javadoc" (oref this :version))
	    (let ((file (jdee-url-file (oref this :url))))
	      (if (string-match ".*\\/\\(.*?\\)\\/doc\\/api" file)
		  (match-string 1 file)
		(jdee-url-name (oref this :url))))))))


(defclass jdee-jdurl (jdee-url)
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
	   :type (or null jdee-jddocset)
	   :documentation "The docset from which this url was created."
	   )
   ))

(defmethod jdee-url-name ((this jdee-jdurl))
  (jdee-url-member-url-name this))

(defmethod jdee-url-file ((this jdee-jdurl))
  (let ((file (jdee-url-parse this 'file)))
    (concat file
	    (if (string-equal "/" (substring file -1)) "" "/")
	    (concat (cl-substitute ?/ ?. (oref this :class)) ".html"))))

(defmethod jdee-url-docset-url-name ((this jdee-jdurl))
  (object-name-string this))

(defmethod jdee-url-append-file-name ((this jdee-jdurl) filename)
  (let ((urlname (jdee-url-docset-url-name this)))
    (format "%s%s%s"
	    urlname
	    (if (string-equal "/" (substring urlname -1))
		"" "/")
	    filename)))

(defmethod jdee-url-class-url-name ((this jdee-jdurl))
  (jdee-url-append-file-name
   this
   (concat (cl-substitute ?/ ?. (oref this :class)) ".html")))

(defmethod jdee-url-member-url-name ((this jdee-jdurl))
  (if (oref this :member)
      (format "%s#%s"
	      (jdee-url-class-url-name this)
	      (oref this :member))
    (jdee-url-class-url-name this)))

(defclass jdee-jdurl-resolver () ()
  :abstract true)

(defmethod jdee-jdurl-resolver-urls ((this jdee-jdurl-resolver) class docset)
  (let ((urlname (jdee-url-name (oref docset :url))))
    (list (jdee-jdurl urlname
		     :class class
		     :docset docset))))

(defmethod jdee-jdurl-resolver-exists ((this jdee-jdurl-resolver) class docset)
  (let ((urls (jdee-jdurl-resolver-urls this class docset)))
    (car (memq t (mapcar #'(lambda (url)
			     (jdee-jdurl-resolver-url-exists this url))
			 urls)))))


(defclass jdee-jdurl-fs-resolver (jdee-jdurl-resolver) ())

(defmethod jdee-jdurl-resolver-url-exists ((this jdee-jdurl-fs-resolver) url)
  (and (equal "file" (jdee-url-protocol url))
       (file-exists-p (jdee-url-file url))))


(defclass jdee-jdurl-w3-resolver (jdee-jdurl-resolver) ())
;; 'w3' is an old name for a library that provided url functions

(defmethod jdee-jdurl-resolver-url-exists ((this jdee-jdurl-w3-resolver) url)
  (require 'url-http)
  (if (fboundp 'url-http-file-exists-p)
      (condition-case err
          (url-http-file-exists-p (jdee-url-name url))
        (error nil))
    (error "Cannot find url-http-file-exists-p function")))


(defclass jdee-jdurl-wget-resolver (jdee-jdurl-resolver)
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

(defmethod initialize-instance ((this jdee-jdurl-wget-resolver) &rest rest)
  (apply 'call-next-method this rest)
  (unless (slot-boundp this :executable)
    (let ((exec (executable-find
		 (if (eq system-type 'windows-nt) "wget.exe" "wget"))))
      (unless exec
	(error "Cannot find wget. You might want to use the beanshell resolver instead."))
      (oset this :executable exec)))
  (dolist (elt '((:tries . jdee-help-wget-tries)
		 (:timeout . jdee-help-wget-timeout)
		 (:options . jdee-help-wget-command-line-options)))
    (unless (slot-boundp this (car elt))
      (set-slot-value this (car elt) (symbol-value (cdr elt))))))

(defmethod jdee-jdurl-resolver-url-exists ((this jdee-jdurl-wget-resolver) url)
  (with-slots (tries timeout options) this
    (let ((cmd (concat "wget --spider "
		       (if tries (concat "--tries=" tries))
		       (if timeout (concat "--timeout=" timeout))
		       options " " (jdee-url-name url))))
      (and (= 0 (shell-command cmd)) t))))

(defclass jdee-jdurl-beanshell-resolver (jdee-jdurl-resolver) ())

(defmethod jdee-jdurl-resolver-url-exists ((this jdee-jdurl-beanshell-resolver) url)
  (jdee-backend-url-exists-p url))

(defclass jdee-jdurl-stack-resolver (jdee-jdurl-resolver)
  ((resolvers :initarg :resolvers
	      :type list)))

(defmethod jdee-jdurl-resolver-urls ((this jdee-jdurl-stack-resolver)
				    class docset)
  ;; we inherit from a class we know always gets only one url
  (let* ((url (car (call-next-method this class docset)))
	 (resolvers (jdee-jdurl-resolver-matching-resolvers this url))
	 urls)
    (dolist (resolver resolvers)
      (setq urls (nconc urls (jdee-jdurl-resolver-urls resolver class docset))))
    urls))

(defmethod jdee-jdurl-resolver-matching-resolvers ((this jdee-jdurl-stack-resolver) url)
  (let (resolvers)
    (dolist (resolver (oref this :resolvers))
      (if (jdee-jdurl-resolver-url-exists resolver url)
	  (setq resolvers (nconc resolvers (list resolver)))))
    resolvers))

(defmethod jdee-jdurl-resolver-url-exists ((this jdee-jdurl-stack-resolver) url)
  (> (length (jdee-jdurl-resolver-matching-resolvers this url)) 0))

(defclass jdee-jdhelper ()
  ((docsets :initarg :docsets
	    :initform nil
	    ;; this is a list of `jdee-jddocset' instances
	    :type list
	    )
   (resolver :initarg :resolver
	     :initform nil
	     :type (or null jdee-jdurl-resolver)
	     )
   ))

(defmethod initialize-instance ((this jdee-jdhelper) &rest rest)
  (apply 'call-next-method this rest)
  (unless (oref this :docsets)
    (jdee-jdhelper-reload-docsets this))
  (unless (oref this :resolver)
    (jdee-jdhelper-reload-resolvers this)))

(defmethod jdee-jdhelper-reload-resolvers ((this jdee-jdhelper))
  (let* ((func (intern (car jdee-help-remote-file-exists-function)))
	 (resolver (case func
		     (wget 'jdee-jdurl-wget-resolver)
		     (url-http-file-exists-p 'jdee-jdurl-w3-resolver)
		     (beanshell 'jdee-jdurl-beanshell-resolver)
		     (t (error "No such remote function: %S" func)))))
    (oset this :resolver
	  (jdee-jdurl-stack-resolver nil :resolvers
				    (list (jdee-jdurl-fs-resolver nil)
					  (funcall resolver nil))))))

(defmethod jdee-jdhelper-reload-docsets ((this jdee-jdhelper))
  (oset this :docsets
	(mapcar #'(lambda (elt)
		    (let* ((version (third elt))
			   (jdkp (and version t))
			   (urlstr (second elt)))
		      (if (null urlstr)
			  (error "No url for docset in `jdee-help-docs': %S"
				 jdee-help-docsets))
		      (jdee-jddocset nil
				    :description (first elt)
				    :url (jdee-url urlstr)
				    :version version
				    :jdkp jdkp)))
		jdee-help-docsets)))

(defmethod jdee-jdhelper-urls-for-class ((this jdee-jdhelper) class)
  (with-slots (docsets resolver) this
    (cl-mapcan #'(lambda (docset)
		   (let ((ver (oref docset :version)))
		     (if (or (not ver) (equal ver (car jdee-jdk)))
			 (jdee-jdurl-resolver-urls resolver class docset))))
	       docsets)))

(defmethod jdee-jdhelper-jdk-url ((this jdee-jdhelper))
  (with-slots (docsets resolver) this
    (dolist (docset docsets)
      (if (equal (oref docset :version) (car jdee-jdk))
	  (return (oref docset :url))))))

(defvar jdee-help-read-url-history nil)

(defmethod jdee-jdhelper-read-url ((this jdee-jdhelper) class)
  (let ((urls (jdee-jdhelper-urls-for-class this class)))
    (cond ((null urls)
	   (error "Cannot find documentation for %s" class))
	  ((= 1 (length urls))
	   (car urls))
	  (t (let ((def (car jdee-help-read-url-history))
		   (alist (mapcar
			   #'(lambda (url)
			       (cons (oref (oref url :docset) :description)
				     url))
			   urls)))
	       (cdr (assoc (completing-read
			    (jdee-create-default-prompt "Docset" def)
			    (mapcar 'car alist)
			    nil t nil 'jdee-help-read-url-history def)
			   alist)))))))

(defmethod jdee-jdhelper-show-class ((this jdee-jdhelper) class &optional member)
  (let ((url (jdee-jdhelper-read-url this class)))
    (oset url :member member)
    (jdee-jdhelper-show-url this url)))

(defmethod jdee-jdhelper-show-url ((this jdee-jdhelper) url)
  (let ((doc-url (jdee-url-name url)))
    (unless (string= jdee-help-browser-function "w3m-browse-url")
      (let* ((temp-directory (jdee-temp-directory))
	     (csfile "jde_meta.html")
	     (metafile (expand-file-name csfile temp-directory))
	     (class-url (jdee-url-class-url-name url))
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
	  (if jdee-help-use-frames
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
			 (jdee-url-append-file-name url "overview-frame.html")))
		(princ
		 (format "<frame src=\"%spackage-frame.html\" name=\"packageFrame\">\n"
			 package-file))
		(princ
		 (format "<frame src=\"%s\" name=\"packageFrame\">\n"
			 (jdee-url-append-file-name url "allclasses-frame.html")))
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
    (jdee-jdhelper-show-document this doc-url)))

(defmethod jdee-jdhelper-show-document ((this jdee-jdhelper) doc-url &rest args)
  "Displays DOC-URL in the browser specified by `jdee-help-browser-function'."
  (let ((browser-function
	 (cond
	  ((functionp jdee-help-browser-function)
	   jdee-help-browser-function)
	  ((string= jdee-help-browser-function "w3m-browse-url")
	   'w3m-browse-url)
	  (t 'browse-url))))
    (apply browser-function doc-url args)))

(defmethod jdee-jdhelper-describe-docsets ((this jdee-jdhelper))
  (let* ((cols (mapcar #'(lambda (docset)
			   (list (oref docset :description)
				 (jdee-url-name (oref docset :url))
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

;; FIXME: this throws an error at byte-compile time and for new
;; users if the default for jdee-help-remote-file-exists-function is
;; not found.
;;
;; Change jdee-jdhelper-reload-resolvers to use more than just the
;; first value in jdee-help-remote-file-exists-function, and change
;; default value to '("wget" "beanshell" "url-http-file-exists-p").
;;
;; Or just delete the jdee-jdurl-resolver-url-exists method, and always
;; use url-http-file-exists-p
(defvar jdee-jdhelper-singleton (jdee-jdhelper nil)
  "The JDHelper singleton instance.")


;; interactive



(defun jdee-help-describe-docsets ()
  (interactive)
  (let ((this jdee-jdhelper-singleton))
    (jdee-jdhelper-describe-docsets this)))

(defun jdee-help-class (class)
  "Return help for fully qualified CLASS."
  (interactive (list (jdee-read-class)))
  (jdee-jdhelper-show-class jdee-jdhelper-singleton class))

(defun jdee-help-class-member (class member)
  "Pop up a menu of the fields and methods of CLASS.
Then search `jdee-help-docsets' for javadoc for CLASS. If found,
point the browser to the doc for the selected method or
field. Note: this command does not check whether the doc
for CLASS actually documents the selected class member."
  (interactive
   (let* ((class (jdee-read-class))
	  (member (jdee-help-popup-class-member-menu class)))
     (list class member)))
  (jdee-jdhelper-show-class jdee-jdhelper-singleton class member))

(defun jdee-help-symbol ()
  "Displays help for a symbol. The symbol may reference an object, a
class, or a method or field. If the symbol references a class, this
function displays the javadoc for the class. If the symbol references
an object, this method displays the javadoc for the class of the
object. If the symbol references a field or a method, this function
displays the javadoc for the class of the object of which the field or
method is a member at the point where the method of field is
documented."
  (interactive)
  (cl-flet ((show-symbol
	     (class method-name)
	     (let ((urls (jdee-jdhelper-urls-for-class jdee-jdhelper-singleton class)))
	       (if (null urls)
		   (message "Error: cannot find documentation for class %s " class)
		 (let ((classinfo (jdee-complete-get-classinfo class))
		       method-signature member pos)
		   (if (and method-name classinfo)
		       (setq method-signature (jdee-complete-find-all-completions
					       (list class method-name)
					       classinfo)))
		   (if method-signature
		       (progn
			 (setq member (caar method-signature))
			 (setq pos (string-match " : " member))
			 (if pos
			     (setq member (substring member 0 pos)))))
		   (jdee-jdhelper-show-class jdee-jdhelper-singleton class member))))))
    (condition-case err
	(let* ((parse-result (jdee-help-parse-symbol-at-point))
	       (unqualified-name (thing-at-point 'symbol))
	       (class-name (jdee-parse-get-qualified-name unqualified-name t))
	       (pair (jdee-parse-java-variable-at-point)))
	  (if (not class-name)
	      (if parse-result
		  (progn
		    (setq unqualified-name  (car parse-result))
		    (setq class-name (jdee-parse-get-qualified-name unqualified-name t)))))
	  (if class-name
	      (show-symbol class-name (cdr parse-result))
	    (if (not (string= (car pair) ""))
		(progn
		  (setq class-name (jdee-parse-get-qualified-name (car pair) t))
		  (show-symbol class-name unqualified-name))
	      (message "Error: cannot find class '%s' on the current classpath." unqualified-name))))
      (error
       (message "%s" (error-message-string err))))))

(defun jdee-help-parse-symbol-at-point ()
  "Returns (cons TYPE MEMBER) where TYPE is the declared type of
the object referenced by the (qualified) name at point and MEMBER is the
field or method referenced by the name if qualified."
  (let ((parse-result (jdee-parse-qualified-name-at-point)))
    (if parse-result
	(let* ((qualifier (car parse-result))
	       (name (cdr parse-result))
	       (obj (if qualifier qualifier name))
	       (member (if qualifier name)))
	  (if (not
	       (and
		qualifier
		(string-match "[.]" qualifier)))
	      (let ((declared-type (car (jdee-parse-declared-type-of obj))))
		(if declared-type
		    (cons declared-type  member))))))))

(defun jdee-help-popup-class-member-menu (class &optional title)
  "Popup a menu of the fields and methods defined by CLASS.
Return the member selected by the user."
  (let ((classinfo
	  (jdee-complete-get-classinfo class)))
    (if classinfo
	(let (pos
	      (pair
	       (if (= (length classinfo) 1)
		   ;; if only one item match, return it
		   (car classinfo)

		 (if (eq jdee-complete-function 'jdee-complete-menu)
		     ;; delegates menu handling to imenu :-) Popup window at
		     ;; text cursor
		     (imenu--mouse-menu classinfo
					(jdee-cursor-posn-as-event)
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
(defun jdee-help-beanshell ()
  "Displays the BeanShell documentation."
  (interactive)
  (browse-url "http://www.beanshell.org/manual/contents.html"))

;;;###autoload
(defun jdee-help-browse-jdk-doc ()
  "Displays the JDK doc in a web browser."
  (interactive)
  (let ((url (jdee-jdhelper-jdk-url jdee-jdhelper-singleton))
	urlstr)
    (setq url (jdee-jdurl (jdee-url-name url)))
    (setq urlstr (jdee-url-append-file-name url "index.html"))
    (browse-url urlstr)))

;;;###autoload
(defun jdee-help-show-jdee-doc ()
  "Opens the JDEE User's Guide info pages."
  (interactive)
  (info "(jdee)User's Guide"))

(provide 'jdee-help)

;;; jdee-help.el ends here
