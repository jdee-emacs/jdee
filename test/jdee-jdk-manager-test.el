;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)

(require 'jdee-jdk-manager)

;; Known values for `system-type':
;; ‘gnu’          compiled for a GNU Hurd system.
;; ‘gnu/linux’    compiled for a GNU/Linux system.
;; ‘gnu/kfreebsd’ compiled for a GNU system with a FreeBSD kernel.
;; ‘darwin’       compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;; ‘ms-dos’       compiled as an MS-DOS application.
;; ‘windows-nt’   compiled as a native W32 application.
;; ‘cygwin’       compiled using the Cygwin library.

(ert-deftest jdee-jdk-build-default-registry-test ()
  "Should return empty registry for unimplemented OSes."
  (dolist (st (list 'gnu 'gnu/kfreebsd 'ms-dos 'windows-nt 'cygwin))
    (let ((system-type st))
      (should (null (jdee-jdk-build-default-registry))))))

(ert-deftest jdee-jdk-build-default-registry-darwin-test ()
  "Should return default registry for Darwin."
  ;; No JDK found:
  (cl-letf ((system-type 'darwin)
            ((symbol-function 'file-executable-p)
             (lambda (filename)
               (should (string= "/usr/libexec/java_home" filename))
               nil)))
    (should (null (jdee-jdk-build-default-registry))))

  ;; JDK in default path:
  (cl-letf ((system-type 'darwin)
            ((symbol-function 'file-executable-p)
             (lambda (filename)
               (should (string= "/usr/libexec/java_home" filename))))
            ((symbol-function 'shell-command-to-string)
             (lambda (command)
               (should (string= "/usr/libexec/java_home" command))
               "/usr/lib64/jvm/jdk1.7.0_21/")))
    (should (equal '(("1.7" . "/usr/lib64/jvm/jdk1.7.0_21"))
                   (jdee-jdk-build-default-registry)))))

(ert-deftest jdee--jdk-find-dirs-test ()
  "Should return list of directories in given path."
  (cl-letf ((expected-dirs '("/usr/lib/jvm/java-8-oracle"
                             "/usr/lib64/jvm/java-7-openjdk-amd64"))
            ((symbol-function 'directory-files)
             (lambda (path full-match)
               (should full-match)
               (if (string= "/usr/lib/jvm" path)
                   '("/usr/lib/jvm/."
                     "/usr/lib/jvm/.."
                     "/usr/lib/jvm/.java-8-oracle.jinfo"
                     "/usr/lib/jvm/default-java"
                     "/usr/lib/jvm/java-8-oracle")
                 '("/usr/lib64/jvm/."
                   "/usr/lib64/jvm/.."
                   "/usr/lib64/jvm/.java-1.7.0-openjdk-amd64.jinfo"
                   "/usr/lib64/jvm/java-1.7.0-openjdk-amd64"
                   "/usr/lib64/jvm/java-7-openjdk-amd64"))))
            ((symbol-function 'file-directory-p)
             (lambda (filename)
               (member filename expected-dirs)))
            ((symbol-function 'file-symlink-p)
             (lambda (filename)
               (not (file-directory-p filename)))))
    (should (equal (sort (jdee--jdk-find-dirs '("/usr/lib/jvm" "/usr/lib64/jvm")) #'string<)
                   expected-dirs))))

(ert-deftest jdee-jdk-build-default-registry-linux-no-jdks-test ()
  "Should return empty when there are no JDKs in default paths for GNU/Linux."
  ;; 'gnu/linux JDK paths:
  ;; /usr/lib/jvm for Debian based and RedHat
  ;; /usr/lib64/jvm for Open Suse

  (let ((default-jvm-paths '("/usr/lib/jvm" "/usr/lib64/jvm")))
    ;; No JDK found in default paths:
    (cl-letf ((system-type 'gnu/linux)
              ((symbol-function 'file-executable-p)
               (lambda (filename)
                 (should (string= "/usr/bin/javac" filename))
                 nil))
              ((symbol-function 'jdee--jdk-find-dirs)
               (lambda (paths)
                 (should (equal paths default-jvm-paths))
                 nil)))
      (should (null (jdee-jdk-build-default-registry))))))

(ert-deftest jdee-jdk-build-default-registry-linux-default-test ()
  "Should return registry with JDKs loaded from default dirs for GNU/Linux."
  ;; JDKs in default paths:
  (let ((default-jvm-paths '("/usr/lib/jvm" "/usr/lib64/jvm")))
    (cl-letf ((system-type 'gnu/linux)
              ((symbol-function 'jdee--jdk-p)
               (lambda (path)
                 (string-match "oracle" path)))
              ((symbol-function 'jdee--jdk-find-dirs)
               (lambda (paths)
                 (should (equal paths default-jvm-paths))
                 '("/usr/lib/jvm/java-6-oracle"
                   "/usr/lib/jvm/java-8-oracle"
                   "/usr/lib/jvm/java-7-oracle"
                   "/usr/lib/jvm/java-7-openjdk-amd64")))
              ((symbol-function 'file-executable-p)
               (lambda (path) nil)))
      (should (equal '(("1.8" . "/usr/lib/jvm/java-8-oracle")
                       ("1.7" . "/usr/lib/jvm/java-7-oracle")
                       ("1.6" . "/usr/lib/jvm/java-6-oracle"))
                     (jdee-jdk-build-default-registry))))))

(ert-deftest jdee--jdk-get-version-test ()
  "Should return version for given directory."

  ;; `nil' as directory:
  (should-not (jdee--jdk-get-version nil))

  ;; Directory with unreadable version:
  (should-not (jdee--jdk-get-version "/tmp"))

  ;; Common GNU/Linux paths:
  (should (string= "1.6" (jdee--jdk-get-version "/usr/lib64/jvm/java-1.6.0-openjdk-amd64")))
  (should (string= "1.7" (jdee--jdk-get-version "/usr/lib64/jvm/jdk1.7.0_21")))
  (should (string= "1.7" (jdee--jdk-get-version "/usr/lib/jvm/java-7-openjdk-amd64")))
  (should (string= "1.8" (jdee--jdk-get-version "/usr/lib/jvm/java-8-oracle")))
  (should
   (string=
    "1.8"
    (jdee--jdk-get-version
     "/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.51-4.b16.fc22.x86_64")))
    (should
   (string=
    "1.9"
    (jdee--jdk-get-version
     "/usr/lib/jvm/java-1.9.0-openjdk-1.9.0.12-4.b16.fc22.x86_64"))))

(provide 'jdee-jdk-manager-test)
;;; jdee-jdk-manager-test.el ends here
