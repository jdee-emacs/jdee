;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)

(require 'jde-autoload)
(require 'beanshell)
(require 'jde-open-source)

(ert-deftest test-jde-remove-type ()
  (should (equal '("List") (jde-remove-type '("List<String>")))))

(provide 'jde-open-source-test)
;;; jde-open-source-test.el ends here
