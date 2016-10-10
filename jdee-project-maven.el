;;; jdee-project-maven.el -- Project maven integration

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

(require 'jdee)
(require 'jdee-open-source)

(defun jdee-project-maven-classpath-from-file (file-name &optional sep)
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
 '(jdee-global-classpath (jdee-project-classpath-from-file \"./../../target/test.cp\")))

and this in src/main

(jdee-set-variables
 '(jdee-global-classpath (jdee-project-classpath-from-file \"./../../target/compile.cp\")))


"
  (let ((the-file (jdee-normalize-path file-name)))
    (message "loading file %s %s" the-file (file-exists-p the-file))
    (jdee-with-file-contents
     the-file
     (split-string (buffer-string) (or sep path-separator t)))))

(provide 'jdee-project-maven)

;;; jdee-project-maven.el ends here
