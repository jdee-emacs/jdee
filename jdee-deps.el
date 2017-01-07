;;; jdee-deps.el -- Checks dependencies of JDEE

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cedet)
(require 'jdee-util)

(defconst jdee-cedet-min-version "1.0beta2"
  "Cedet minimum version.")

(defconst jdee-cedet-max-version "2.0"
  "Cedet maximum version.")

(defcustom jdee-check-version-flag t
  "*Non-nil means to check versions of semantic, eieio, and speedbar.
That is if they meet the requirements for this version of the JDE.
If nil only check if semantic, eieio, and speedbar are available.
See also the function `jdee-check-versions'."
  :group 'jdee
  :type 'boolean)

(defconst jdee-check-versions-message
  "JDEE requires a version of CEDET between %s and %s (found %s)")

(defun jdee-check-versions ()
  "Check for correct versions of CEDET provided packages.
Signal an error if CEDET is not installed.
When `jdee-check-version-flag' is non-nil, signal an error if the
version of CEDET currently installed doesn't meet the requirements for
this version of the JDEE."
  ;; Check that CEDET is installed.
  (or (boundp 'cedet-version)
      (error jdee-check-versions-message
             jdee-cedet-min-version
             jdee-cedet-max-version
             "none"))
  ;; Check version requirement when requested.
  (or (not jdee-check-version-flag)
      (jdee-check-version cedet-version
                          jdee-cedet-min-version
                          jdee-cedet-max-version)
      (error jdee-check-versions-message
             jdee-cedet-min-version
             jdee-cedet-max-version
             cedet-version)))


(defun jdee-check-version (current-version min-version max-version)
  "Return non-nil if CURRENT-VERSION >= MIN-VERSION or <= MAX-VERSION."
  (and (or (jdee-earlier-versionp current-version
                                  max-version)
           (string= current-version
                    max-version))
       (or (jdee-earlier-versionp min-version
                                  current-version)
           (string= current-version
                    min-version))))

(defun jdee-earlier-versionp (ver1 ver2)
  "Return non-nil if `VER1' is earlier than `VER2'."
  (let ((ver1n (jdee-replace-in-string ver1 "beta" "zb"))
        (ver2n (jdee-replace-in-string ver2 "beta" "zb")))
    (setq ver1n (jdee-replace-in-string ver1n "pre" "zp"))
    (setq ver2n (jdee-replace-in-string ver2n "pre" "zp"))
    (if (string-match "z" ver1n)
        (unless (string-match "z" ver2n)
          (setq ver2n (concat ver2n "zz")))
      (if (string-match "z" ver2n)
          (setq ver1n (concat ver1n "zz"))))
    (string< ver1n ver2n)))

(provide 'jdee-deps)

;;; jdee-deps.el ends here
