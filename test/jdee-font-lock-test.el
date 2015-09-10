;;; jdee-font-lock-test.el --- JDEE Font Lock: Unit test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Przemysław Wojnowski <esperanto@cumego.com>

;; Author: Przemysław Wojnowski <esperanto@cumego.com>
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

;; The unit test suite of JDEE Font Lock

;;; Code:

(require 'jdee-font-lock)
(require 'ert)

(defmacro jdee-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENTS."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (jdee-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defun jdee-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.
If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (jdee-test-with-temp-buffer content
                                  (get-text-property pos 'face))
    (get-text-property pos 'face)))

(ert-deftest jdee-font-lock/fontify-package-name ()
  :tags '(fontification)
  (jdee-test-with-temp-buffer
      "package com.example;"
    (should (eq (jdee-test-face-at 1) 'font-lock-keyword-face))
    (should (eq (jdee-test-face-at 9) 'jdee-font-lock-package-face))
    (should (eq (jdee-test-face-at 13) 'jdee-font-lock-package-face))
    ;; Semicolon at the end shold has no face:
    (should (eq (jdee-test-face-at 20) nil))))

(ert-deftest jdee-font-lock/fontify-import-class ()
  :tags '(fontification)
  (jdee-test-with-temp-buffer
      "import java.util.Date;"
    ;; import keyword:
    (should (eq (jdee-test-face-at 1) 'font-lock-keyword-face))
    ;; package name:
    (should (eq (jdee-test-face-at 8) 'jdee-font-lock-package-face))
    ;; class name:
    (should (eq (jdee-test-face-at 18) 'font-lock-type-face))
    ;; semicolon:
    (should (eq (jdee-test-face-at 22) nil))))

(ert-deftest jdee-font-lock/fontify-import-package ()
  :tags '(fontification)
  :expected-result :failed
  (jdee-test-with-temp-buffer
      "import java.io.*;"
    ;; import keyword:
    (should (eq (jdee-test-face-at 1) 'font-lock-keyword-face))
    ;; package name:
    (should (eq (jdee-test-face-at 8) 'jdee-font-lock-package-face))
    ;; *:
    (should (eq (jdee-test-face-at 16) nil))
    ;; semicolon:
    (should (eq (jdee-test-face-at 17) nil))))

(ert-deftest jdee-font-lock/fontify-import-static ()
  :tags '(fontification)
  (jdee-test-with-temp-buffer
      "import static java.lang.System.out;"
    ;; import keyword:
    (should (eq (jdee-test-face-at 1) 'font-lock-keyword-face))
    ;; static keyword:
    (should (eq (jdee-test-face-at 8) 'font-lock-keyword-face))
    ;; package name:
    (should (eq (jdee-test-face-at 15) 'jdee-font-lock-package-face))
    ;; static field/method:
    (should (eq (jdee-test-face-at 35) nil))
    ;; semicolon:
    (should (eq (jdee-test-face-at 36) nil))))

(ert-deftest jdee-font-lock/fontify-import-static-all ()
  :tags '(fontification)
  :expected-result :failed
  (jdee-test-with-temp-buffer
      "import static java.lang.System.*;"
    ;; import keyword:
    (should (eq (jdee-test-face-at 1) 'font-lock-keyword-face))
    ;; static keyword:
    (should (eq (jdee-test-face-at 8) 'font-lock-keyword-face))
    ;; package name:
    (should (eq (jdee-test-face-at 15) 'jdee-font-lock-package-face))
    ;; *:
    (should (eq (jdee-test-face-at 33) nil))
    ;; semicolon:
    (should (eq (jdee-test-face-at 34) nil))))

(ert-deftest jdee-font-lock/fontify-class-declaration ()
  :tags '(fontification)
  :expected-result :failed
  (jdee-test-with-temp-buffer
      "public class Sample { Sample() {} }"
    ;; public modifier:
    (should (eq (jdee-test-face-at 1) 'jdee-font-lock-public-face))
    ;; class keyword:
    (should (eq (jdee-test-face-at 8) 'font-lock-keyword-face))
    ;; class name:
    (should (eq (jdee-test-face-at 14) 'font-lock-type-face))
    ;; class opening brace:
    (should (eq (jdee-test-face-at 21) nil))
    ;; Sample() {}:
    (should (eq (jdee-test-face-at 23) 'jdee-font-lock-constructor-face))
    (should (eq (jdee-test-face-at 29) nil))
    (should (eq (jdee-test-face-at 30) nil))
    (should (eq (jdee-test-face-at 32) nil))
    (should (eq (jdee-test-face-at 33) nil))
    ;; class closing brace:
    (should (eq (jdee-test-face-at 35) nil))))

(ert-deftest jdee-font-lock/fontify-public-modifier ()
  :tags '(fontification)
  (jdee-test-with-temp-buffer
      "public class Sample { public int field; }"
    ;; public before class:
    (should (eq (jdee-test-face-at 1) 'jdee-font-lock-public-face))
    ;; class keyword:
    (should (eq (jdee-test-face-at 8) 'font-lock-keyword-face))
    ;; class name:
    (should (eq (jdee-test-face-at 14) 'font-lock-type-face))
    ;; public before field:
    (should (eq (jdee-test-face-at 23) 'jdee-font-lock-public-face))))

(ert-deftest jdee-font-lock/fontify-protected-modifier ()
  :tags '(fontification)
  (jdee-test-with-temp-buffer
      "protected class Sample { protected int field; }"
    ;; protected before class:
    (should (eq (jdee-test-face-at 1) 'jdee-font-lock-protected-face))
    ;; class keyword:
    (should (eq (jdee-test-face-at 11) 'font-lock-keyword-face))
    ;; class name:
    (should (eq (jdee-test-face-at 17) 'font-lock-type-face))
    ;; protected before field:
    (should (eq (jdee-test-face-at 26) 'jdee-font-lock-protected-face))))

(ert-deftest jdee-font-lock/fontify-private-modifier ()
  :tags '(fontification)
  (jdee-test-with-temp-buffer
      "private class Sample { private int field; }"
    ;; private before class:
    (should (eq (jdee-test-face-at 1) 'jdee-font-lock-private-face))
    ;; class keyword:
    (should (eq (jdee-test-face-at 9) 'font-lock-keyword-face))
    ;; class name:
    (should (eq (jdee-test-face-at 15) 'font-lock-type-face))
    ;; private before field:
    (should (eq (jdee-test-face-at 24) 'jdee-font-lock-private-face))))

(provide 'jdee-font-lock-test)
;;; jdee-font-lock-test.el ends here
