(require 'ert)

(require 'jde-autoload)
(require 'beanshell)
(require 'jde-open-source)

(ert-deftest jde-remove-type-test ()
  (should (equal '("List") (jde-remove-type '("List<String>")))))
