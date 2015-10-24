;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)

(require 'beanshell)
(require 'jdee-open-source)

(ert-deftest test-jdee-remove-type ()
  (should (equal '("List") (jdee-remove-type '("List<String>")))))

(provide 'jdee-open-source-test)
;;; jdee-open-source-test.el ends here
