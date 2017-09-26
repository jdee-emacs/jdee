;;; jdee-ant-test.el --- JDEE Ant: Unit test suite  -*- lexical-binding: t; -*-

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

;; The unit test suite of JDEE Ant

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'jdee-ant)

(ert-deftest test-jdee-ant--build-command ()
  (let ((jdee-ant-buildfile "bf.xml")
        (jdee-ant-program "ant-sh")
        (jdee-ant-invocation-method '("Script"))
        (system-type 'windows-nt))
    (with-mock
      (stub jdee-ant-get-ant-home => "/ant home")
      (mock (jdee-normalize-path jdee-ant-buildfile) => "/path/bf.xml")
      (should (string= "ant-sh -Dant.home=\"/ant home\" -buildfile \"/path/bf.xml\" -emacs compile "
                       (jdee-build-ant-command "compile" nil))))))

(provide 'jdee-ant-test)
;;; jdee-ant-test.el ends here
