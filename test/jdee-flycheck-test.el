;;; jdee-flycheck-test.el --- JDEE Flycheck unit tests  -*- lexical-binding: t; -*-

;; URL: https://github.com/jdee-emacs/jdee

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'jdee-flycheck)
(require 'ert)

(ert-deftest jdee-flycheck--collect-errors-no-errors-test ()
  "Should return nil when there are no errors."
  (jdee-test-with-temp-buffer
      "CompileServer output:

-classpath /classes -d /classes /src/MyClass.java

Compilation finished at Sun Dec 25 15:15:43
"
    (let ((checker (lambda () (throw error)))
          (orig-buffer (generate-new-buffer-name "flycheck test buffer")))
      (should-not (jdee-flycheck--collect-errors checker orig-buffer)))))

(ert-deftest jdee-flycheck--collect-errors-with-errors-test ()
  "Should return errors when there are no errors."
  (jdee-test-with-temp-buffer
      "CompileServer output:

-classpath /classes -d /classes /src/Poligon.java
/src/Poligon.java:8: error: cannot find symbol
        ++new Poligon().ac();
                       ^
  symbol:   method ac()
  location: class Poligon
/src/Poligon.java:12: error: incompatible types: int cannot be converted to Another
        Another a = 123;
                    ^
2 errors

Compilation exited abnormally with code 1 at Sun Dec 25 16:11:52
"
    (let* ((checker (lambda () (throw e)))
           (orig-buffer (generate-new-buffer-name "flycheck test buffer"))
           (errors (jdee-flycheck--collect-errors checker orig-buffer)))
      (should (= 2 (length errors)))
      (let ((fe (first errors)))
        (should (eq orig-buffer (flycheck-error-buffer fe)))
        (should (eq checker (flycheck-error-checker fe)))
        (should (string= "/src/Poligon.java" (flycheck-error-filename fe)))
        (should (= 12 (flycheck-error-line fe)))
        (should (= 21 (flycheck-error-column fe)))
        (should (string= "incompatible types: int cannot be converted to Another"
                         (flycheck-error-message fe)))
        (should (eq 'error (flycheck-error-level fe))))
      (let ((se (second errors)))
        (should (eq orig-buffer (flycheck-error-buffer se)))
        (should (eq checker (flycheck-error-checker se)))
        (should (string= "/src/Poligon.java" (flycheck-error-filename se)))
        (should (= 8 (flycheck-error-line se)))
        (should (= 24 (flycheck-error-column se)))
        (should (string= "cannot find symbol" (flycheck-error-message se)))
        (should (eq 'error (flycheck-error-level se)))))))

(provide 'jdee-flycheck-test)
;;; jdee-flycheck-test.el ends here
