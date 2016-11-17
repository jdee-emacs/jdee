;;; -*- lexical-binding: t -*-
;;; jdee-maven.el -- Project maven integration

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Keywords: java, tools

;; Copyright (C) 2106 Matthew O. Smith

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


(require 'dash)
(require 'jdee-open-source)
(require 'cl)

(defgroup jdee-maven nil
  "JDEE Maven Project Options"
  :group 'jdee
  :prefix "jdee-")


(defvar jdee-maven-mode-hook nil
  "*Lists functions to run when a buffer is successfully initialized as being
in a maven project.")

(defvar jdee-maven-project-dir nil
  "When a buffer is in a maven project, the full path to the project directory, the one with the pom.xml.  It will be set buffer local by the `jdee-maven-hook'.")

(defcustom jdee-maven-disabled-p nil
  "When nil (default) add maven support to project startup."
  :group 'jdee-maven
  :type 'boolean)

(defcustom jdee-maven-buildfile "pom.xml"
  "Specify the name of the maven project file."
  :group 'jdee-maven
  :type 'string)

(defcustom jdee-maven-program "mvn"
  "Specifies name of ant program/script."
 :group 'jdee-maven
 :type 'string)

(defcustom jdee-maven-build-phase "package"
  "Specifies maven phase to specify when calling Build."
 :group 'jdee-maven
 :type 'string)

(defcustom jdee-maven-artifacts-excluded-from-sources nil
  "Specifies a list of artifact IDs to exclude from sources.  Should be added
to the prj.el with something like:

(jdee-set-variables
 '(jdee-maven-artifacts-excluded-from-sources '(\"ojdbc16\")))
"
 :group 'jdee-maven
 :type '(repeat (string :tag "Artifact:")))


(defcustom jdee-maven-init-hook '(jdee-maven-from-file-hook)
  "A list of functions to call to try and initialize the maven integeration.  Each function will be passed the directory that contains the pom.xml.  Stop calling functions after the first non-nil return."
  :group 'jdee-maven
  :type 'hook)

(defcustom jdee-maven-dir-scope-map
  (list
   (list  'compile
          "target/compile.cp"
          "target/compile-sources.cp"
         '("src/main/java")
         '("src/main/java")
         '("target/classes"))
   (list  'test
         "target/test.cp"
         "target/test-sources.cp"
         '("src/test/java")
         '("src/test/java" "src/main/java" )
         '("target/test-classes" "target/classes")))

  "Specify a map of directories to maven dependency scope type."
  :group 'jdee-maven
  :type '(alist :key-type (symbol :tag "Scope")
                :value-type (list (string :tag "Relative path to classpath file")
                                  (string :tag "Relative path to classpath source file")
                                  (repeat (regexp :tag "Path regexp to match"))
                                  (repeat (string :tag "Source path"))
                                  (repeat (string :tag "Runtime path")))))

(defun jdee-maven-get-default-directory (&optional path)
  "Gets the default-directory by searching for the `jdee-maven-buildfile' usually pom.xml.
  Find the directory of the closest  maven project file (see
`jdee-maven-buildfile') starting at DIR (default to `default-directory')"
  (let ((pom-path (jdee-find-project-file (or path default-directory) jdee-maven-buildfile)))
    (when pom-path
      (file-name-directory pom-path))))

(defun jdee-maven-scope-file (&optional file-dir)
  "Return which classpath file to use based on the `jdee-maven-dir-scope-map'.

FILE-DIR is the directory containing the source code in question.
Default to `default-directory'.

Return nil if not found or a list of (scope cp-file source-paths runtime-paths sources-cp-file) both
relative the maven project dir."
  (cl-loop for (scope key sources-file paths source-paths runtime-paths) in jdee-maven-dir-scope-map ; by 'cddr
           if (-any-p (lambda (path) (string-match path (or file-dir default-directory))) paths)
           return (list scope key source-paths runtime-paths sources-file)))

(defun jdee-maven-check-classpath-file (scope classpath-file sources-classpath-file pom-dir)
  "Check that the CLASSPATH-FILE and SOURCES-CLASSPATH-FILE for
the given SOURCE exist relative to POM-DIR, creating them if they
don't.  See `jdee-maven-check-classpath-file*' for more info."
  (jdee-maven-check-classpath-file* scope classpath-file pom-dir nil)
  (jdee-maven-check-classpath-file* scope sources-classpath-file pom-dir "sources")
  )

(defun jdee-maven-check-classpath-file* (scope output-file pom-dir classifier)
  "Check that the specified classpath file, OUTPUT-FILE,  exists.  If it doesn't
try to load it by calling mvn dependency:build-classpath directly
with the appropriate arguments.

If there is an error in creating the file, it leaves the maven
buffer open so the error can be seen and diagnosed.  See
`jdee-maven-dir-scope-map' for the various values of scope and
output-file.

Returns nil if it is unable to find or create the file, otherwise
it returns the full path to the file.

SCOPE - the maven scope, probably 'compile or 'test

OUTPUT-FILE - the file, relative to the POM-DIR, that is being
checked or created

POM-DIR - directory contatining the pom.xml

CLASSIFIER - the maven the classifier, usually nil or \"sources\""
  
  (let ((classpath-file-path (expand-file-name output-file pom-dir)))
    (if (file-readable-p classpath-file-path)
        classpath-file-path
      (with-current-buffer (get-buffer-create (format "*%s*"  "jdee-maven-check-classpath-file"))
        (let* ((default-directory pom-dir)
               (args (list "dependency:build-classpath"
                           (format "-DincludeScope=%s" scope)
                           (format "-Dmdep.outputFile=%s" output-file)
                           (if classifier
                               (format "-Dclassifier=%s" classifier)
                             nil)
                           (if (and classifier jdee-maven-artifacts-excluded-from-sources)
                               (format "-DexcludeArtifactIds=%s" (mapconcat 'identity jdee-maven-artifacts-excluded-from-sources ",")) 
                             nil))))
          (erase-buffer)
          (pop-to-buffer (current-buffer))
          (apply 'call-process "mvn" nil t t (cl-remove-if-not 'identity args)))
        (goto-char (point-min))
        (when (search-forward "BUILD SUCCESS" nil t)
          (kill-buffer (current-buffer))
          classpath-file-path)))))
          


(defun jdee-maven-from-file-hook (&optional dir)
  "Run as a hook to setup the classpath based on having the
classpath in a file on disk.  See
`jdee-maven-dir-scope-map' for how the files are chosen. 

DIR is the directory containing the pom.xml.  If nil, hunt for it."

  ;(message "jdee-maven-from-file-hook: %s" (pwd))
  (let ((pom-dir (or dir (jdee-maven-get-default-directory))))
    (when pom-dir
      (let ((scope-info (jdee-maven-scope-file)))
        (when scope-info
          (jdee-maven-check-classpath-file (nth 0 scope-info) (nth 1 scope-info) (nth 4 scope-info) pom-dir)
          (let* ((sources-classpath (jdee-maven-classpath-from-file
                                     (expand-file-name (nth 4 scope-info) pom-dir)))
                 (classpath (jdee-maven-classpath-from-file
                     (expand-file-name (nth 1 scope-info) pom-dir)))
                 (sp (mapcar (lambda(p) (expand-file-name p pom-dir))
                             (nth 2 scope-info)))
                 (rp (mapcar (lambda(p) (expand-file-name p pom-dir))
                             (nth 3 scope-info))))
            
            (jdee-set-variables (list 'jdee-global-classpath  (cons 'list (append rp classpath)))
                                '(jdee-build-function 'jdee-maven-build)
                                '(jdee-test-function 'jdee-maven-unit-test)
                                (list 'jdee-built-class-path (cons 'list rp))
                                (list 'jdee-run-working-directory pom-dir)
                                (list 'jdee-run-option-classpath (cons 'list (append rp classpath)))
                                (list 'jdee-db-option-classpath (cons 'list (append rp sp classpath)))
                                (list 'jdee-compile-option-directory (first rp))
                                (list 'jdee-compile-option-classpath (cons 'list (append sp classpath)))
                                (list 'jdee-sourcepath  (cons 'list (append sp sources-classpath))))
            pom-dir))))))


(defun jdee-maven-classpath-from-file (file-name &optional sep)
  "Read a classpath from a file that contains a classpath.  Useful in conjunction with
a maven plugin to create the classpath like:
	    <plugin>
              <groupId>org.apache.maven.plugins</groupId>
              <artifactId>maven-dependency-plugin</artifactId>
              <version>2.10</version>
              <executions>
		<execution>
		  <id>test-classpath</id>
		  <phase>generate-sources</phase>
		  <goals>
		    <goal>build-classpath</goal>
		  </goals>
		  <configuration>
		    <outputFile>target/test.cp</outputFile>
		    <includeScope>test</includeScope>
		  </configuration>
		</execution>
		<execution>
		  <id>compile-classpath</id>
		  <phase>generate-sources</phase>
		  <goals>
		    <goal>build-classpath</goal>
		  </goals>
		  <configuration>
		    <outputFile>target/compile.cp</outputFile>
		    <includeScope>compile</includeScope>
		  </configuration>
		</execution>
              </executions>
	    </plugin>

It can be used in a prj.el like this in src/test

(jdee-set-variables
 '(jdee-global-classpath (jdee-maven-classpath-from-file \"./../../target/test.cp\")))

and this in src/main

(jdee-set-variables
 '(jdee-global-classpath (jdee-maven-classpath-from-file \"./../../target/compile.cp\")))


"
  (let ((the-file (jdee-normalize-path file-name)))
    ;;(message "loading file %s %s" the-file (file-exists-p the-file))
    (jdee-with-file-contents
     the-file
     (split-string (buffer-string) (or sep path-separator t)))))

;;
;; Unit tests
;;



;; FIXME: Does this belong here?  Seems more like a parsing function.
(defun jdee-maven-annotations ()
  "Get the annotations of the current tag.

It cheats and looks at the face property for c-annotation-face."
  (save-excursion
    (let* ((current-tag (semantic-current-tag))
           (prev-tag (and current-tag
                          (progn
                            (goto-char (semantic-tag-start current-tag))
                            (semantic-find-tag-by-overlay-prev)))))
      (when (and current-tag prev-tag)
        (let* ((start (semantic-tag-end prev-tag))
              (end (semantic-tag-start current-tag))
              (annotation-start (text-property-any start end 'face 'c-annotation-face)))
          (when annotation-start
            (let ((annotation-end (next-single-property-change  annotation-start 'face (current-buffer) end)))
              (buffer-substring-no-properties annotation-start annotation-end))))))))


(defun jdee-maven-unit-test-run-method-args ()
  "Return the arguments needed to pass to maven to run a single unit test method"
  
  (when (and (not current-prefix-arg)
             (string= (jdee-maven-annotations) "@Test"))
    (format "-Dtest=%s#%s test" (buffer-name) (semantic-tag-name (semantic-current-tag)))))


(defun jdee-maven-unit-test-run-class-args ()
  "Return the arguments needed to pass to maven to run class of  unit test method"

  (when (and (> 5 (prefix-numeric-value current-prefix-arg))
             (text-property-any (point-min) (point-max) 'face 'c-annotation-face))
    (format "-Dtest=%s test" (buffer-name))))

(defun jdee-maven-file ()
  "A function for use in `compilation-error-regexp-alist' as the
file name.  

Expects (match-string 2) to return the fully qualified name of
the class.  Also adds the name of the tag to search for as a property called
'method-name for match-string 1, 'fqn, 'class-name, and  'message"
  (let ((rtnval (jdee-stacktrace-file* (match-string 2))))
    (put-text-property (match-beginning 0) (match-end 0) 'method-name (match-string-no-properties 1))
    (put-text-property (match-beginning 0) (match-end 0) 'fqn (match-string-no-properties 2))
    (put-text-property (match-beginning 0) (match-end 0) 'class-name (match-string-no-properties 4))
    (put-text-property (match-beginning 0) (match-end 0) 'message (match-string-no-properties 5))
    rtnval))

(defun jdee-maven-class-tag-p (class-name tag-type)
  "Create a predicate to check the first two elements of a list.

Returns a function that accepts a list and checks the first two elements.

Assumes the 1st element is a string and `string=' to CLASS-NAME and
the second element is  `eq' to TAG-TYPE."
  (lexical-let ((class-name class-name)
                (tag-type tag-type))
    (lambda (tag)
      (when (and  (eq tag-type (cadr tag))
                  (string= (car tag) class-name))
        tag))))

;;
;; There really should be a semantic function to do this but I cannot
;; find it
;;
(defun jdee-maven-find-tag-by-name-and-type (name type tags)
  "Recursively find a tag by name and type.

Descend on the :members element"
  (when (and name type tags)
    (let ((rtnval (cl-find-if (jdee-maven-class-tag-p name type) tags)))
      (if rtnval
          rtnval
        (dolist (tag tags)
          (let ((members (plist-get (nth 2 tag) :members)))
            (dolist (member members)
              (jdee-maven-find-tag-by-name-and-type name type member))))))))
  
  
(defun jdee-maven-unit-test-next-error-function (n &optional reset)
  "This function is a value of `next-error-function' that supports
the results of mvn test. 

Return the tag of the method if found, nil otherwise."
  (let ((next-fn 'compilation-next-error-function)
        (compile-buffer (current-buffer)))
    (funcall next-fn n reset)
    (let* ((method-name   (with-current-buffer compile-buffer
                            (get-text-property (point) 'method-name)))
           (class-name   (with-current-buffer compile-buffer
                           (get-text-property (point) 'class-name)))
           (tags (semantic-something-to-tag-table (current-buffer)))
           (class-tag (jdee-maven-find-tag-by-name-and-type class-name 'type tags))
           (class-members (plist-get (nth 2 class-tag) :members))
           (method-tag (jdee-maven-find-tag-by-name-and-type method-name 'function class-members)))
      (when method-tag
        (semantic-go-to-tag method-tag)
        (semantic-momentary-highlight-tag method-tag)
        method-tag))))
      

(defvar jdee-maven-unit-test-error-regexp
  
  (format "%s(%s):\\(.*\\)" (jdee-parse-java-name-part-re) (jdee-parse-java-fqn-re))
  " Looks for something like 

  testConnectionProxy2CallJava2(jde.juci.ConnectionImplTest): expected:<hello worl[ ]d> but was:<hello worl[]d>

  Match regions are
1 - the test method name
2 - FQN of the unit test
3 - package name of the unit test
4 - class name of the unit test
5 - the error message
")


  
(defvar jdee-maven-unit-test-finish-hook nil)

(defun jdee-maven-unit-test (&optional path)
  
  "Unit test using maven with project based in PATH (default to `default-directory')

Tries to limit the scope of the unit test based on current point.  If in a class that 
is a test class, just run that file.

With a single prefix C-u, it will skip trying to run a single method.  With a double prefix C-u C-u it will skip trying to run a single class as well.

"
  (interactive)
  (let* ((default-directory (jdee-maven-get-default-directory path))
         ;; FIXME: use a hook instead
         (args (or (jdee-maven-unit-test-run-method-args)
                   (jdee-maven-unit-test-run-class-args)
                   "test"))
         (compilation-scroll-output 'first-error)
         (compile-buffer (compilation-start (format "%s %s" jdee-maven-program args))))
    (with-current-buffer compile-buffer
      (setq next-error-function 'jdee-maven-unit-test-next-error-function)
      (setq compilation-finish-functions
          (lambda (buf msg)
            (run-hook-with-args 'jdee-maven-unit-test-finish-hook buf msg)
            (setq compilation-finish-functions nil)))

      (add-to-list 'compilation-error-regexp-alist
                   (list jdee-maven-unit-test-error-regexp
                         'jdee-maven-file nil nil nil
                         2              ;Hyperlink = FQN
                         '(1  compilation-info-face) ;test method name
                         '(2  compilation-error-face) ;FQN
                         '(5  compilation-message-face)))))) ; error message

;;
;; Building
;;

;;;###autoload
(defun jdee-maven-build (&optional path)
  
  "Build using the maven command from PATH (default to `default-directory')"
  (interactive)
  (let ((default-directory (jdee-maven-get-default-directory path)))
    (compilation-start (format "%s %s" jdee-maven-program jdee-maven-build-phase))))

;;;###autoload
(defun jdee-maven-hook ()
  "Initialize the maven integration if available.  Runs all the
functions in `jdee-maven-init-hook' until one returns non-nil.
If all return nil, maven mode is not initialized.  If one of the
functions returns non-nil, set `jdee-maven-project-dir' buffer
local and then run the functions in `jdee-maven-mode-hook'."
  (unless jdee-maven-disabled-p
    (let ((jdee-maven-project-dir* (jdee-maven-get-default-directory)))
      (when (run-hook-with-args-until-success 'jdee-maven-init-hook jdee-maven-project-dir*)
        (setq-local jdee-maven-project-dir jdee-maven-project-dir*)
        (run-hooks 'jdee-maven-mode-hook)))))

(provide 'jdee-maven)

;;; jdee-maven.el ends here
