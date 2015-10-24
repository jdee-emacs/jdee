;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)

(require 'jdee-util)

(ert-deftest test-jdee-create-default-prompt ()
  (should (string= "a (default b): " (jdee-create-default-prompt "a" "b"))))

(provide 'jdee-util-test)
;;; jdee-util-test.el ends here
