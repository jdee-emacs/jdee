;;; jdee-compile-test.el --- JDEE Compile: Unit test suite  -*- lexical-binding: t; -*-

;; Author: Przemysław Wojnowski
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

;; The unit test suite of JDEE Compile

;;; Code:

(require 'jdee-compile)
(require 'ert)

(ert-deftest test-jdee-compile--successful-compilation-p ()
  (should (jdee-compile--successful-compilation-p "" ""))
  (should (jdee-compile--successful-compilation-p "msg" "buf content"))
  (should-not (jdee-compile--successful-compilation-p "exited abnormally" "buf content"))
  (should-not (jdee-compile--successful-compilation-p "msg" "BUILD FAILED"))
  (should-not (jdee-compile--successful-compilation-p "exited abnormally" "BUILD FAILED")))

(provide 'jdee-compile-test)
;;; jdee-compile-test.el ends here
