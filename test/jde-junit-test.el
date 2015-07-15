;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jde-junit)

(ert-deftest test-jde-junit ()
  "Test jde-junit-get-tester-name function"
  (should (string= "TDynamicClassLoader"
                   (jde-junit-get-tester-name "DynamicClassLoader"))))


(provide 'jde-junit-test)
;;; jde-junit-test.el ends here
