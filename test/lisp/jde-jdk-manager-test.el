;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)

(require 'jde-jdk-manager)

;; Known values for `system-type':
;; ‘gnu’          compiled for a GNU Hurd system.
;; ‘gnu/linux’    compiled for a GNU/Linux system.
;; ‘gnu/kfreebsd’ compiled for a GNU system with a FreeBSD kernel.
;; ‘darwin’       compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;; ‘ms-dos’       compiled as an MS-DOS application.
;; ‘windows-nt’   compiled as a native W32 application.
;; ‘cygwin’       compiled using the Cygwin library.

(ert-deftest jde-jdk-build-default-registry-test ()
  "Should return empty registry for unimplemented OSes."
  (dolist (st (list 'gnu 'gnu/kfreebsd 'ms-dos 'windows-nt 'cygwin))
    (let ((system-type st))
      (should (null (jde-jdk-build-default-registry))))))

(ert-deftest jde--jdk-get-version-test ()
  "Should return version for given directory."

  ;; `nil' as directory:
  (should-not (jde--jdk-get-version nil))

  ;; Directory with unreadable version:
  (should-not (jde--jdk-get-version "/tmp"))

  ;; Common GNU/Linux paths:
  (should (string= "1.6" (jde--jdk-get-version "/usr/lib64/jvm/java-1.6.0-openjdk-amd64")))
  (should (string= "1.7" (jde--jdk-get-version "/usr/lib64/jvm/jdk1.7.0_21")))
  (should (string= "1.7" (jde--jdk-get-version "/usr/lib/jvm/java-7-openjdk-amd64")))
  (should (string= "1.8" (jde--jdk-get-version "/usr/lib/jvm/java-8-oracle"))))

(provide 'jde-jdk-manager-test)
;;; jde-jdk-manager-test.el ends here
