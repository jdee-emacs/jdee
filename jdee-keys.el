;;; jdee-keys.el -- JDEE Keymap handling

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

(defcustom jdee-key-bindings
  (list
   (cons "[?\C-c ?\C-v ?\C-a]" 'jdee-run-menu-run-applet)
   (cons "[?\C-c ?\C-v ?\C-b]" 'jdee-build)
   (cons "[?\C-c ?\C-v ?\C-c]" 'jdee-compile)
   (cons "[?\C-c ?\C-v ?\C-u]" 'jdee-test-unittest)
   (cons "[?\C-c ?\C-v ?\C-d]" 'jdee-debug)
   (cons "[?\C-c ?\C-v ?\C-f]" 'jdee-find)
   (cons "[?\C-c ?\C-v ?\C-g]" 'jdee-open-class-at-point)
   (cons "[?\C-c ?\C-v ?*]"    'jdee-parse-fqn-to-kill-ring)
   (cons "[?\C-c ?\C-v ?#]"    'jdee-stacktrace-buffer)
   (cons "[?\C-c ?\C-v ?w]"    'jdee-archive-which)
   (cons "[?\C-c ?\C-v ?\C-k]" 'jdee-backend-run)
   (cons "[?\C-c ?\C-v ?\C-l]" 'jdee-gen-println)
   (cons "[?\C-c ?\C-v ?\C-n]" 'jdee-help-browse-jdk-doc)
   (cons "[?\C-c ?\C-v ?\C-p]" 'jdee-save-project)
   (cons "[?\C-c ?\C-v ?\C-q]" 'jdee-project-update-class-list)
   (cons "[?\C-c ?\C-v ?\C-r]" 'jdee-run)
   (cons "[?\C-c ?\C-v ?\C-s]" 'speedbar-frame-mode)
   (cons "[?\C-c ?\C-v ?\C-t]" 'jdee-jdb-menu-debug-applet)
   (cons "[?\C-c ?\C-v ?\C-w]" 'jdee-help-symbol)
   (cons "[?\C-c ?\C-v ?\C-x]" 'jdee-show-superclass-source)
   (cons "[?\C-c ?\C-v ?\C-y]" 'jdee-open-class-at-point)
   (cons "[?\C-c ?\C-v ?\C-z]" 'jdee-import-find-and-import)
   (cons "[?\C-c ?\C-v ?e]"    'jdee-wiz-extend-abstract-class)
   (cons "[?\C-c ?\C-v ?f]"    'jdee-gen-try-finally-wrapper)
   (cons "[?\C-c ?\C-v ?i]"    'jdee-wiz-implement-interface)
   (cons "[?\C-c ?\C-v ?j]"    'jdee-javadoc-autodoc-at-line)
   (cons "[?\C-c ?\C-v ?o]"    'jdee-wiz-override-method)
   (cons "[?\C-c ?\C-v ?t]"    'jdee-gen-try-catch-wrapper)
   (cons "[?\C-c ?\C-v ?z]"    'jdee-import-all)
   (cons "[?\C-c ?\C-v ?\C-[]" 'jdee-run-etrace-prev)
   (cons "[?\C-c ?\C-v ?\C-]]" 'jdee-run-etrace-next)
   (cons "[(control c) (control v) (control ?.)]" 'jdee-complete)
   (cons "[(control c) (control v) ?.]" 'jdee-complete-in-line)
   )
  "*Specifies key bindings for the JDE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jdee-project
  :type '(repeat
          (cons :tag "Key binding"
                (string :tag "Key")
                (function :tag "Command")))
  :set '(lambda (sym val)
          (mapc
           (lambda (buf)
             (save-excursion
               (set-buffer buf)
               (when (boundp 'jdee-mode-map)
                 ;; Unmap existing key bindings
                 (if (and (boundp 'jdee-key-bindings)
                          jdee-key-bindings)
                     (mapc
                      (lambda (binding)
                        (let ((key (car binding)))
                          (if (string-match "\\[.+]" key)
                              (setq key (car (read-from-string key))))
                          (local-unset-key key)))
                      jdee-key-bindings))
                 ;; Map new key bindings.
                 (mapc
                  (lambda (binding)
                    (let ((key (car binding))
                          (fcn (cdr binding)))
                      (if (string-match "\\[.+]" key)
                          (setq key (car (read-from-string key))))
                      (define-key (current-local-map) key fcn)))
                  val))))
           (jdee-get-java-source-buffers))
          (set-default sym val)))

;; jdee-describe-map is Ehud Karni's describe map with jdee prepended.
(defun jdee-keymap-test (var)
  "Internal function for keymap checking."
  (and (boundp var)
       (keymapp (symbol-value var))))

(defun jdee-describe-map (map)
  "Display binding for MAP which must be a quoted keymap variable."
  (interactive
   (let ((map (intern (completing-read "Key map: " obarray 'jdee-keymap-test 1))))
     (list map)))
  (let ((val (symbol-value map)))
    (or (keymapp val)
        (error "%s is not a keymap !" (symbol-name map)))
    (with-output-to-temp-buffer "*Help*"
      (princ (format "Binding for keymap %s is:\n" (symbol-name map)))
      (princ (substitute-command-keys "\\{val}" ))
      (help-print-return-message))))

(defun jdee-keys ()
  "Displays JDEE key bindings.
Use `jdee-bug-keys' to display JDEbug keybindings."
  (interactive)
  (jdee-describe-map 'jdee-mode-map))

(provide 'jdee-keys)

;;; jdee-keys.el ends here
