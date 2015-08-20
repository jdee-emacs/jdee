;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)

(require 'jde-util)

(ert-deftest test-jde-create-default-prompt ()
  (should (string= "a (default b): " (jde-create-default-prompt "a" "b"))))

(provide 'jde-util-test)
;;; jde-util-test.el ends here
