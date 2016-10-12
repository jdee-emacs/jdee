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


(require 'jdee-open-source)
(require 'cl)

(defgroup jdee-maven nil
  "JDEE Maven Project Options"
  :group 'jdee
  :prefix "jdee-")


(defcustom jdee-maven-disabled-p nil
  "When nil (default) add maven support to project startup."
  :group 'jdee-maven
  :type 'boolean)

(defcustom jdee-maven-file-name "pom.xml"
  "Specify the name of the maven project file."
  :group 'jdee-maven
  :type 'string)

(defcustom jdee-maven-init-hook '(jdee-maven-from-file-hook)
  "A list of functions to call to try and initialize the maven integeration.  Each function will be passed the directory that contains the pom.xml.  Stop calling functions after the first non-nil return."
  :group 'jdee-maven
  :type 'hook)

(defcustom jdee-maven-dir-scope-map-old (list "target/compile.cp" '("src/main/java")
                                                      "target/test.cp" '("src/test/java"))

  "Specify a map of directories to maven dependency scope type."
  :group 'jdee-maven
  :type '(plist :key-type string :value-type (repeat string)))

(defcustom jdee-maven-dir-scope-map '(("target/compile.cp" ("src/main/java")  ("src/main/java"))
                                              ("target/test.cp" ("src/test/java") ("src/test/java" "src/main/java" )))

  "Specify a map of directories to maven dependency scope type."
  :group 'jdee-maven
  :type '(alist :key-type (string :tag "Relative path to classpath file")
                :value-type (list (repeat (regexp :tag "Path regexp to match"))
                                  (repeat (string :tag "Source path")))))


(defun jdee-maven-pom-dir (&optional dir)
  "Find the directory of the closest maven maven project
file (see `jdee-maven-file-name') starting at
DIR (default to `default-directory')"
  (let ((pom-path  (jdee-find-project-file (or dir default-directory)
                                           jdee-maven-file-name)))
    (when pom-path
      (file-name-directory pom-path))))

(defun jdee-maven-scope-file (&optional file-dir)
  "Return which classpath file to use based on the `jdee-maven-dir-scope-map'.

FILE-Dir is the directory containing the source code in question.  Default to `default-directory'. 

Return nil if not found or a list of (cp-file source-paths) both relative the maven project dir."
  (cl-loop for (key paths source-paths) in jdee-maven-dir-scope-map ; by 'cddr
           if (-any-p (lambda (path) (string-match path (or file-dir default-directory))) paths)
           return (list key source-paths)))

;;;###autoload
(defun jdee-maven-hook ()
  "Initialize the maven integration if available."
  (unless jdee-maven-disabled-p 
    (run-hook-with-args-until-success 'jdee-maven-init-hook (jdee-maven-pom-dir))))

(defun jdee-maven-from-file-hook (&optional dir)
  "Run as a hook to setup the classpath based on having the
classpath in a file on disk.  See
`jdee-maven-dir-scope-map' for how the files are chosen. 

DIR is the directory containing the pom.xml.  If nil, hunt for it."
  (let ((pom-dir (or dir (jdee-maven-pom-dir))))
    (when pom-dir
      (let ((scope-info (jdee-maven-scope-file)))
        (when scope-info
          (let ((cp (jdee-maven-classpath-from-file
                     (expand-file-name (nth 0 scope-info) pom-dir)))
                (sp (mapcar (lambda(p) (expand-file-name p pom-dir))
                            (nth 1 scope-info))))
            (jdee-set-variables '(jdee-global-classpath cp)
                                '(jdee-compile-option-classpath (append sp cp))
                                '(jdee-sourcepath  sp))
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

(provide 'jdee-maven)

;;; jdee-maven.el ends here
