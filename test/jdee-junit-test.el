;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-junit)

(ert-deftest test-jdee-junit ()
  "Test jdee-junit-get-tester-name function"
  (should (string= "DynamicClassLoaderTest"
                   (jdee-junit-get-tester-name "DynamicClassLoader"))))


(provide 'jdee-junit-test)
;;; jdee-junit-test.el ends here
