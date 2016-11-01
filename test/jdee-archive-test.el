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


(provide 'jdee-archive-test)
;;; jdee-archive-test.el ends here
