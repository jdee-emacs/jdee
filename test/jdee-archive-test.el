;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of jdee-archive.el
;;
;;; Code:

(require 'ert)
(require 'el-mock)
(require 'jdee-maven)
(require 'cl)


(defun find-project-directory (&optional dir)
  (let ((makefile (jdee-find-project-file (or dir default-directory)  "Makefile")))
    (when makefile
      (file-name-directory makefile))))

(defvar classpath-type-alist '((compile ("test/jars/compile"))
                               (test ("test/jars/test" "test/jars/compile"))))
                                        

(defun build-classpath (type &optional dir)
  "Build a classpath."
  (let ((prj-dir (find-project-directory dir))
        (dirs (cadr (assoc type classpath-type-alist)))
        rtnval)
    (dolist (d dirs rtnval)
      (setq rtnval (append rtnval (directory-files (expand-file-name d prj-dir) t ".*[.]jar$"))))))  
    
    

;;
;; Testing: jdee-archive-files-hashtable
;;

(ert-deftest test-jdee-archive-files-hashtable-has-entries ()
  "Check that `jdee-archive-files-hashtable' returns the correct hashtable."
  (let* ((project-dir (find-project-directory))
         (jar (expand-file-name "test/jars/compile/commons-lang-2.6.jar" project-dir)))
    (should (file-exists-p jar))
    (with-current-buffer (find-file-noselect jar)
      (let* ((files archive-files)
             (ht (jdee-archive-files-hashtable files)))
        (kill-buffer (current-buffer))

        (should (eq 'indexed (gethash "org/apache/commons/lang/ArrayUtils.class" ht 'missing)))
        (should (eq 'missing (gethash "org/junit/rules/DisableOnDebug.class" ht 'missing)))))))

;;
;; Testing: jdee-archive-resource-from-ht
;;

(ert-deftest test-jdee-archive-resource-from-ht-with-entries ()
  "Check that `jdee-archive-resource-from-ht' finds the resource as expected."
  (let* ((project-dir (find-project-directory))
         (jar (expand-file-name "test/jars/compile/commons-lang-2.6.jar" project-dir)))
    (should (file-exists-p jar))
    (with-current-buffer (find-file-noselect jar)
      (let* ((files archive-files)
             (ht (jdee-archive-files-hashtable files)))
        (should (equal
                 (list jar "org/apache/commons/lang/ArrayUtils.class")
                 (jdee-archive-resource-from-ht ht jar "org/apache/commons/lang/ArrayUtils.class")))
        (should (equal
                 nil
                 (jdee-archive-resource-from-ht ht jar "hamster.class")))))))

(provide 'jdee-archive-test)
;;; jdee-archive-test.el ends here
