;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'el-mock)
(require 'jdee-maven)
(require 'cl)

(ert-deftest test-get-default-directory-no-pom-returns-nil ()
  "Check that if a pom.xml is not found, it returns nil."
  (let ((default-directory "/aaaaaaaa/b/c/d/e/f"))
    (with-mock
     (stub directory-files => '("." ".."))
     (should (null (jdee-maven-get-default-directory))))))

(ert-deftest test-get-default-directory-with-pom-returns-dir ()
  "Check that the case where the pom file is found returns the correct directory.
Requires directory-files and file-readable-p to be stubbed so it
doesn't try to hit the file system."
  (let* ((without-pom '("." ".."))
         (with-pom '("." ".." "pom.xml"))
         (dirs (list (list "/a" without-pom)
                     (list "/a/b" without-pom)
                     (list "/a/b/c" with-pom)
                     (list "/a/b/c/src" without-pom)
                     (list "/a/b/c/src/main" without-pom)
                     (list "/a/b/c/src/main/java" without-pom))))


    (let ((default-directory (caar (last dirs))))
      
      (cl-letf (((symbol-function 'directory-files) (lambda (d) (cadr (assoc d dirs)))))
        (with-mock
          (stub file-readable-p => t)
          
          (should (string= (format "%s/" (car (nth 2 dirs)))
                           (jdee-maven-get-default-directory))))))))

(provide 'jdee-maven-test)
;;; jdee-maven-test.el ends here
