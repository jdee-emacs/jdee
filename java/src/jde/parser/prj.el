













(jde-set-project-name "parser")
(jde-set-variables 
 '(jde-run-option-properties nil)
 '(jde-run-option-stack-size (quote ((128 . "kilobytes") (400 . "kilobytes"))))
 '(jde-gen-buffer-templates (quote (("Class" . jde-gen-class) ("Console" . jde-gen-console) ("Swing App" . jde-gen-jfc-app))))
 '(jde-compile-option-command-line-args "+E" t)
 '(jde-gen-action-listener-template (quote ("'& (P \"Component name: \")" "\".addActionListener(new ActionListener() {\" 'n>" "\"public void actionPerformed(ActionEvent e) {\" 'n>" "\"}});\" 'n>")))
 '(jde-compile-option-depend nil)
 '(jde-compile-option-optimize nil)
 '(jde-run-option-verify (quote (nil t)))
 '(jde-gen-inner-class-template (quote ("'& \"class \" (P \"Class name: \" class)" "(P \"Superclass: \" super t)" "(let ((parent (jde-gen-lookup-named 'super)))" "(if (not (string= parent \"\"))" "(concat \" extends \" parent))) \" {\" 'n>" "\"public \" (s class) \"() {\" 'n> \"}\" 'n> \"}\" 'n>")))
 '(jde-run-read-vm-args nil)
 '(jde-entering-java-buffer-hooks (quote (jde-reload-project-file)))
 '(jde-run-applet-viewer "")
 '(jde-compile-option-debug (quote ("selected" (t nil nil))) t)
 '(jde-project-file-name "prj.el")
 '(jde-run-option-verbose (quote (nil nil nil)))
 '(jde-run-application-class "" t)
 '(jde-db-option-vm-args nil)
 '(jde-run-option-heap-size (quote ((1 . "megabytes") (16 . "megabytes"))))
 '(jde-compile-option-target (quote ("1.1")))
 '(jde-appletviewer-option-encoding "")
 '(jde-db-read-vm-args nil)
 '(jde-db-option-heap-profile (quote (nil "./java.hprof" 5 20 "Allocation objects")))
 '(jde-db-mode-hook nil)
 '(jde-run-option-garbage-collection (quote (t t)))
 '(jde-compile-option-vm-args nil)
 '(jde-run-applet-doc "")
 '(jde-compile-option-depend-switch (quote ("-Xdepend")))
 '(jde-db-option-java-profile (quote (nil . "./java.prof")))
 '(jde-compile-option-sourcepath nil)
 '(jde-debug-breakpoint-marker-colors (quote ("red" . "yellow")))
 '(jde-gen-get-set-var-template (quote ("'n>" "(P \"Variable type: \" type) \" \"" "(P \"Variable name: \" name) \";\" 'n> 'n>" "\"/**\" 'n>" "\"* Get the value of \" (s name) \".\" 'n>" "\"* @return Value of \" (s name) \".\" 'n>" "\"*/\" 'n>" "\"public \" (s type) \" get\" (jde-gen-init-cap (jde-gen-lookup-named 'name))" "\"() {return \" (s name) \";}\" 'n> 'n>" "\"/**\" 'n>" "\"* Set the value of \" (s name) \".\" 'n>" "\"* @param v  Value to assign to \" (s name) \".\" 'n>" "\"*/\" 'n>" "\"public void set\" (jde-gen-init-cap (jde-gen-lookup-named 'name))" "\"(\" (s type) \"  v) {this.\" (s name) \" = v;}\" 'n>")))
 '(jde-compile-option-extdirs nil)
 '(jde-db-option-verify (quote (nil t)))
 '(jde-run-mode-hook nil)
 '(jde-db-option-classpath nil)
 '(jde-compile-option-deprecation nil)
 '(jde-db-startup-commands nil)
 '(jde-gen-boilerplate-function (quote jde-gen-create-buffer-boilerplate))
 '(jde-compile-option-classpath nil t)
 '(jde-build-use-make nil t)
 '(jde-quote-classpath t)
 '(jde-gen-to-string-method-template (quote ("'&" "\"public String toString() {\" 'n>" "\"return super.toString();\" 'n>" "\"}\" 'n>")))
 '(jde-run-read-app-args nil)
 '(jde-db-source-directories (quote ("d:/jmath/src/" "e:/jdk1.2/src/")) t)
 '(jde-db-option-properties nil)
 '(jde-db-option-stack-size (quote ((128 . "kilobytes") (400 . "kilobytes"))))
 '(jde-db-set-initial-breakpoint t)
 '(jde-run-option-application-args nil t)
 '(jde-gen-mouse-listener-template (quote ("'& (P \"Component name: \")" "\".addMouseListener(new MouseAdapter() {\" 'n>" "\"public void mouseClicked(MouseEvent e) {}\" 'n>" "\"public void mouseEntered(MouseEvent e) {}\" 'n>" "\"public void mouseExited(MouseEvent e) {}\" 'n>" "\"public void mousePressed(MouseEvent e) {}\" 'n>" "\"public void mouseReleased(MouseEvent e) {}});\" 'n>")))
 '(jde-gen-console-buffer-template (quote ("(funcall jde-gen-boilerplate-function) 'n" "\"/**\" 'n" "\" * \"" "(file-name-nondirectory buffer-file-name) 'n" "\" *\" 'n" "\" *\" 'n" "\" * Created: \" (current-time-string) 'n" "\" *\" 'n" "\" * @author \" (user-full-name) 'n" "\" * @version\" 'n" "\" */\" 'n>" "'n>" "\"public class \"" "(file-name-sans-extension (file-name-nondirectory buffer-file-name))" "\" {\" 'n> 'n>" "\"public \"" "(file-name-sans-extension (file-name-nondirectory buffer-file-name))" "\"() {\" 'n>" "'n>" "\"}\" 'n>" "'n>" "\"public static void main(String[] args) {\" 'n>" "'p 'n>" "\"}\" 'n> 'n>" "\"} // \"" "(file-name-sans-extension (file-name-nondirectory buffer-file-name))" "'n>")))
 '(jde-compile-option-directory "d:/jde-dev/java/classes" t)
 '(jde-run-option-vm-args nil)
 '(jde-make-program "make")
 '(jde-compile-option-bootclasspath nil)
 '(jde-use-font-lock t)
 '(jde-db-option-garbage-collection (quote (t t)))
 '(jde-appletviewer-option-vm-args nil)
 '(jde-gen-class-buffer-template (quote ("(funcall jde-gen-boilerplate-function) 'n" "\"/**\" 'n" "\" * \"" "(file-name-nondirectory buffer-file-name) 'n" "\" *\" 'n" "\" *\" 'n" "\" * Created: \" (current-time-string) 'n" "\" *\" 'n" "\" * @author \" (user-full-name) 'n" "\" * @version\" 'n" "\" */\" 'n>" "'n>" "\"public class \"" "(file-name-sans-extension (file-name-nondirectory buffer-file-name))" "\" \" (jde-gen-get-super-class) \" {\" 'n> 'n>" "\"public \"" "(file-name-sans-extension (file-name-nondirectory buffer-file-name))" "\"() {\" 'n>" "'p 'n>" "\"}\" 'n>" "'n>" "\"} // \"" "(file-name-sans-extension (file-name-nondirectory buffer-file-name))" "'n>")))
 '(jde-compiler "//d/jikes/jikes" t)
 '(jde-debug-breakpoint-cursor-colors (quote ("cyan" . "brown")))
 '(jde-jdk-doc-url "e:/jdk1.2/docs/index.html" t)
 '(jde-db-debugger (quote ("jdb" . "Executable")))
 '(jde-compile-option-verbose-path nil)
 '(jde-run-option-classpath nil)
 '(jde-key-bindings (quote (("" . jde-compile) ("" . jde-run) ("" . jde-db) ("" . jde-build) ("" . jde-run-menu-run-applet) ("" . jde-db-menu-debug-applet) ("
" . bsh) ("" . speedbar-frame-mode) ("" . jde-wiz-implement-interface) ("