(require 'ert)

(require 'jde-util)

(ert-deftest jde-create-default-prompt-test ()
  (should (string= "a (default b): " (jde-create-default-prompt "a" "b"))))
