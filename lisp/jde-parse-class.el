;; JDE-PARSE-CLASS.EL --- Parse a Java class file.
;; $Revision: 1.9 $ $Date: 2005/01/20 04:53:21 $
;;
;; Copyright (C) 2002, 2004, 2005 Andrew Hyatt
;;
;; Author: Andrew Hyatt <andy_jde@thehyatts.net>
;; Maintainers: Andrew Hyatt and Paul Kinnucan
;;
;; Keywords: java, tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; ) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; jde-parse-class|Andrew Hyatt|
;; |Parse a Java class file to get xref info for the JDEE.
;; |$Date: 2005/01/20 04:53:21 $|$Revision: 1.9 $|~/packages/jde-parse-class.el

;;; Commentary:

;; This file parses Java .class files to get information about what
;; methods call what other methods.  On the way there, we happen to
;; pick up all sorts of useful information. If someone needs the
;; functionality, contact me and I could easiliy put in more
;; functionality.
;;
;; Also, we could return a semantic-compatible structure, but at the
;; moment there is no point in doing this.
;;
;; There are two public functions, `jde-parse-class', which returns
;; everything we have parsed about the class. The resultant structures
;; can be passed to `jde-parse-class-extract-method-calls' which
;; returns the method calls a particular class uses, and
;; `jde-parse-class-extract-interfaces' to retrieve the interfaces a
;; class uses, and `jde-parse-class-extract-classname' to get the
;; fully qualified classname the class represents.  This is used in
;; jde-xref.el
;;
;; There is one other functions that is generally useful, and one
;; macro.  The function is `get-bit-flags-for-byte', which, given a
;; single byte in decimal (0 - 255), and a vector of flags
;; corresponding to bytes, will return a list of which flags are on.
;; The macro `do-and-advance-chars' wraps code, and after executing the
;; code, advances the current point a certain number of chars (here
;; equivalent to bytes).  This is useful for the kind of parsing we are
;; doing.

;; for XEmacs compatibilty
(unless (fboundp 'char-int)
  (defalias 'char-int 'identity))
(when jde-xemacsp
    (require 'jde-xemacs))

(defmacro do-and-advance-chars (num &rest body)
  "Execute BODY, and then advance the point NUM bytes, called like
      (do-and-advance-chars 2 ...)"
  (let ((result-sym (gensym)))
  `(let ((,result-sym (progn ,@body)))
     (goto-char (+ (point) ,num))
     ,result-sym)))

(defsubst jde-parse-class-get-next-const-val (constants)
  (cadr (jde-parse-class-lookup-constant
         (jde-parse-class-get-next-2-bytes) constants)))

;; xemacs without mule can't do utf-8
(setq jde-parse-class-encoding
      (if (member 'utf-8 (coding-system-list))
          'utf-8 'raw-text))

(defun jde-parse-class-slash-to-dot (string)
  (subst-char-in-string ?/ ?. string))

(defun jde-parse-class (class-file)
  "Parse the class in CLASS-FILE, and return an alist, with the following
keys: version, this-class, interfaces, fields, and methods."
  ;; we must visit the file in such a way as to not use any encoding,
  ;; else our parsing could be severely messed up
  (unless (file-exists-p class-file)
    (error (concat "Class file " class-file " does not exist")))
  (let ((buf (find-file-noselect class-file nil t)))
    (set-buffer buf)
    (let* ((constants (jde-parse-class-get-constants))
           (version (jde-parse-class-get-version))
           (access (jde-parse-class-get-access-flags))
           (this-class
             (subst-char-in-string
              ?/ ?. (cadr (jde-parse-class-lookup-constant
                             (jde-parse-class-get-next-const-val constants)
                              constants))))
           (superclass (jde-parse-class-slash-to-dot
                        (do-and-advance-chars 2
                          (let ((val (jde-parse-class-get-const-ref
                                      (point) constants)))
                            (if (eq (cadr val) 0)
                              nil ;; only Object can have no superclass
                              (cadr (jde-parse-class-lookup-constant
                                     (cadr val) constants)))))))
           (interfaces (jde-parse-class-get-interfaces constants))
           (fields (jde-parse-class-get-fields constants))
           (methods (jde-parse-class-get-methods constants))
           (attributes (jde-parse-class-get-attributes constants)))
      (kill-buffer buf)
      `((version . ,version) (access . ,access)
        (this-class . ,this-class) (superclass . ,superclass)
        (interfaces . ,interfaces)
        (fields . ,fields)
        (methods . ,methods)
        (attributes . ,attributes)))))

(defun jde-parse-class-extract-caught-exception-types (info)
  "Returns a list of a two-element of list of method signatures to
  caught exception types for each method"
  (mapcar (lambda (method)
            (list `(,(cdr (nth 2 info))
                      ,(cdr (assoc 'name method))
                      ,@(cdr (assoc 'descriptor method)))
                  (mapcar (lambda (class) (when class
                                            (jde-parse-class-slash-to-dot class)))
                          (cdr (assoc
                                'catch-types
                                (cdr
                                 (assoc 'code
                                        (cdr
                                         (assoc 'attributes
                                                method)))))))))
          (cdr (assoc 'methods info))))

(defun jde-parse-class-extract-thrown-exception-types (info)
  "Returns a two element list of method signatures to thrown exception
  types for each method"
  (mapcar (lambda (method)
            (list `(,(cdr (nth 2 info))
                      ,(cdr (assoc 'name method))
                      ,@(cdr (assoc 'descriptor method)))
                  (cdr
                   (assoc 'exceptions
                          (cdr
                           (assoc 'attributes
                                  method))))))
          (cdr (assoc 'methods info))))

(defun jde-parse-class-extract-method-calls (info)
  "Return a cons of a method signature, and a list of the methods it
calls.  Each method in the list is a list of the calling method or
line number if available, the Class, method, and return value, and
arguments.  INFO is the result of `jde-parse-class'"
  (mapcan (lambda (method) (mapcar (lambda (attr)
                                     (list 
                                      `(,(cdr (nth 2 info))
                                         ,(cdr (assoc 'name method))
                                         ,@(cdr (assoc 'descriptor method)) 
                                         ,(car attr)) (cdr attr))) 
                                   (cdr (assoc
                                         'calls
                                         (cdr
                                          (assoc 'code
                                                 (cdr
                                                  (assoc 'attributes
                                                         method))))))))
          (cdr (assoc 'methods info))))

(defun jde-parse-class-extract-interfaces (info)
  "Returns a list of fully qualified interface names that the class
  implements.  INFO is the result of `jde-parse-class'"
  (mapcar 'jde-parse-class-slash-to-dot
          (cdr (assoc 'interfaces info))))

(defun jde-parse-class-extract-superclass (info)
  "Returns a list of fully qualified class names that are superclasses
  of the parsed class"
  (jde-parse-class-slash-to-dot (cdr (assoc 'superclass info))))

(defun jde-parse-class-extract-method-signatures (info)
  "Returns a list of method names that the class implements"
  (mapcar (lambda (method-info) (cons (cdr (assoc 'name method-info))
                                      (cdr (assoc 'descriptor method-info))))
          (cdr (assoc 'methods info))))

(defun jde-parse-class-extract-field-signatures (info)
  "Return a list of field names that the class defines"
  (mapcar (lambda (field-info)
            (list (cdr (assoc 'name field-info))
                  (cdr (assoc 'descriptor field-info))))
          (cdr (assoc 'fields info))))

(defun jde-parse-class-extract-classname (info)
  "Returns the fully qualified classname that the class implements.
INFO is the result of `jde-parse-class'"
  (cdr (assoc 'this-class info)))

(defun jde-parse-class-extract-sourcefile (info)
  (cdr (assoc 'source-file (cdr (assoc 'attributes info)))))

(defun jde-parse-class-get-const-ref (point constants)
  "Look at point in the class file and read it as a reference to the array.
Returns the constant information contained at the reference"
  ;; we have to subtract one since the array index starts at 1
  ;; (according to the class file, not us
  (jde-parse-class-lookup-constant (jde-parse-class-get-2byte point)
                                   constants))

(defun jde-parse-class-lookup-constant (num constants)
  "From an index value, get the constant for that index"
  (aref constants (- num 1)))
  
(defsubst jde-parse-class-get-next-length-val ()
  (jde-parse-class-get-next-2-bytes))

(defun jde-parse-class-get-interfaces (constants)
  (let ((num (jde-parse-class-get-next-length-val))
        (interfaces '()))
    (dotimes (i num interfaces)
      (add-to-list 'interfaces (cadr (jde-parse-class-lookup-constant
                                     (jde-parse-class-get-next-const-val
                                      constants) constants))))))

(defun jde-parse-class-get-methods (constants)
  (let ((num (jde-parse-class-get-next-length-val))
        (methods '()))
    (dotimes (i num (nreverse methods))
      (add-to-list 'methods (jde-parse-class-get-method constants)))))

(defun jde-parse-class-get-method (constants)
  (list (cons 'access-flags (jde-parse-class-get-access-flags))
        (cons 'name (jde-parse-class-get-next-const-val constants))
        (cons 'descriptor (jde-parse-class-parse-complete-arg-signature
                           (jde-parse-class-get-next-const-val constants)))
        (cons 'attributes (jde-parse-class-get-attributes constants))))

(defun jde-parse-class-get-fields (constants)
  (let ((num (jde-parse-class-get-next-length-val))
        (fields '()))
    (dotimes (i num (nreverse fields))
      (add-to-list 'fields (jde-parse-class-get-field constants)))))

(defun jde-parse-class-get-field (constants)
  (list (cons 'access-flags (jde-parse-class-get-access-flags))
        (cons 'name (jde-parse-class-get-next-const-val constants))
        (cons 'descriptor (car (jde-parse-class-parse-first-arg
                           (jde-parse-class-get-next-const-val constants))))
        (cons 'attributes (jde-parse-class-get-attributes constants))))

(defun jde-parse-class-get-attributes (constants)
  (let ((num (jde-parse-class-get-next-length-val))
        (attributes '()))
    (dotimes (i num attributes)
      (add-to-list 'attributes (jde-parse-class-get-attribute constants)))))

(defun jde-parse-class-get-attribute (constants)
  (let ((attribute-type (jde-parse-class-get-next-const-val constants))
        (numbytes (jde-parse-class-get-next-4-bytes (point))))
    ;; TODO: implement the rest of the common attribute types
    (cond ((equal attribute-type "Code")
            (cons 'code
                  (jde-parse-class-get-code-attribute numbytes constants)))
          ((equal attribute-type "LineNumberTable")
            (cons 'line-number-table
                  (jde-parse-class-get-line-number-attribute)))
          ((equal attribute-type "Exceptions")
            (cons 'throws
                  (jde-parse-class-get-exception-attribute constants)))
          ((equal attribute-type "SourceFile")
            (cons 'source-file
                  (jde-parse-class-get-source-file-attribute constants)))
          (t (forward-char numbytes)))))

(defun jde-parse-class-get-source-file-attribute (constants)
  (jde-parse-class-get-next-const-val constants))

(defun jde-parse-class-get-exception-attribute (constants)
  (let ((num (jde-parse-class-get-next-length-val))
        (classes '()))
    (dotimes (i num classes)
      (add-to-list 'classes
                   (cadr (jde-parse-class-lookup-constant
                          (jde-parse-class-get-next-const-val constants)
                          constants))))))

(defun jde-parse-class-get-line-number-attribute ()
  (let ((num (jde-parse-class-get-next-length-val))
        (line-number-infos '()))
    (dotimes (i num (nreverse line-number-infos))
      (add-to-list
       'line-number-infos
       (cons (jde-parse-class-get-next-2-bytes)
             (jde-parse-class-get-next-2-bytes))))))

(defun jde-parse-class-get-code-attribute (numbytes constants)
  (goto-char (+ (point) 4)) ;; no one cares about max_stack and max_locals, right?
  (let* ((code-numbytes (jde-parse-class-get-next-4-bytes (point)))
         (end-point (+ (point) code-numbytes))
         (begin-point (point))
         (calls '())
         (catch-types '()))
    (while (< (point) end-point)
      (let* ((opcode-info (aref jde-parse-class-opcode-vec
                                (char-int (char-after))))
             (opcode-val (car opcode-info))
             (opcode-length (cdr opcode-info)))
        (forward-char)
        (cond ((member opcode-val '(invokevirtual invokestatic invokespecial
                                    invokeinterface getstatic putstatic
                                    getfield putfield))
                (do-and-advance-chars (- opcode-length 1)
                  (add-to-list
                   'calls
                   (cons (- (point) begin-point)
                         (jde-parse-class-parse-method-signature
                          (jde-parse-class-lookup-method
                           (jde-parse-class-get-const-ref (point) constants)
                           constants))))))
              ((eq opcode-val 'tableswitch)
                ;; skip padding to go to a multiple of 4 from the begin-point.
                ;; The second mod is to make sure on an offset of 4 we really don't skip anything
                (forward-char (mod (- 4 (mod (- (point) begin-point) 4)) 4))
                (forward-char 4)  ;; we don't care about default
                (let ((low (jde-parse-class-get-next-4-bytes-as-signed))
                      (high (jde-parse-class-get-next-4-bytes-as-signed)))
                  (forward-char (* 4 (+ (- high low) 1)))))
              ((eq opcode-val 'lookupswitch)
                (forward-char (mod (- 4 (mod (- (point) begin-point) 4)) 4))
                (forward-char 4)
                (forward-char (* 8 (jde-parse-class-get-next-4-bytes-as-signed))))
              ((eq opcode-val 'wide)
                (let ((opcode2 (char-int (char-after))))
                  (if (eq opcode2 'iinc)
                    (forward-char 5)
                    (forward-char 2))))
              (t (forward-char (- opcode-length 1))))))
    (let ((num-exceptions (jde-parse-class-get-next-length-val)))
      (dotimes (i num-exceptions)
        (let ((type (cdr (assoc 'catch-type
                                 (jde-parse-class-get-exception-handler
                                  constants)))))
          (when type
            (add-to-list 'catch-types type)))))
    (let ((attributes (jde-parse-class-get-attributes constants)))
      ;; Get the line numbers if there, if not use -1's to iondicate no line number
      (let ((table (when (assoc 'line-number-table attributes)
                     (cdr (assoc 'line-number-table attributes)))))
        (list (cons
               'calls
               (mapcar (lambda (call)
                  (if table
                    ;; The line numbers describe the instruction at
                    ;; the start of the line, not every instruction

                    ;; Advance in the table when the next byte
                    ;; described is higher than the current one
                    (progn (while (and (cadr table)
                                       (>= (car call) (caadr table)))
                             (setq table (cdr table)))
                           (cons (cdar table) (cdr call)))
                    (cons -1 (cdr call)))) (nreverse calls)))
              (cons 'catch-types catch-types))))))

(defun jde-parse-class-get-exception-handler (constants)
  (list (cons 'start-pc (jde-parse-class-get-next-2-bytes))
    (cons 'end-pc (jde-parse-class-get-next-2-bytes))
    (cons 'handler-pc (jde-parse-class-get-next-2-bytes))
    (cons 'catch-type (let ((val (jde-parse-class-get-next-2-bytes)))
                        (when (> val 0)
                          (cadr (jde-parse-class-lookup-constant
                                 (cadr (jde-parse-class-lookup-constant val constants))
                              constants)))))))

(defun jde-parse-class-parse-first-arg (sig)
  (let ((char (char-to-string (string-to-char sig))))
    (cond ((equal char "B") `("byte" . 1))
          ((equal char "C") `("char" . 1))
          ((equal char "D") `("double" . 1))
          ((equal char "F") `("float" . 1))
          ((equal char "I") `("int" . 1))
          ((equal char "J") `("long" . 1))
          ((equal char "L") (let ((endpos (string-match ";" sig)))
                              (cons (jde-parse-class-slash-to-dot
                                     (substring sig 1
                                                endpos))
                                    (+ 1 endpos))))
          ((equal char "S") `("short" . 1))
          ((equal char "Z") `("boolean" . 1))
          ((equal char "[") (let ((rest (jde-parse-class-parse-first-arg
                                         (substring sig 1))))
                              (cons (concat (car rest) "[]") (+ (cdr rest) 1))))
            (t (error (concat "Could not find char " char))))))

(defun jde-parse-class-parse-arg-signature (sig)
  (when (> (length sig) 0)
    (let ((arg (jde-parse-class-parse-first-arg sig)))
      (if (> (cdr arg) (length sig))
        (list (car arg))
        (cons (car arg) (jde-parse-class-parse-arg-signature
                         (substring sig (cdr arg))))))))
                                    

(defun jde-parse-class-parse-complete-arg-signature (sig)
  (let* ((match (string-match "(\\(.*\\))\\(.*\\)" sig)))
    (if match
      (let ((args (match-string 1 sig))
            (return-val (match-string 2 sig)))
        (list (unless (equal "V" return-val)
                (car (jde-parse-class-parse-arg-signature return-val)))
              (jde-parse-class-parse-arg-signature args)))
      (list nil (jde-parse-class-parse-arg-signature sig)))))
         

(defun jde-parse-class-parse-method-signature (siglist)
  `(,(jde-parse-class-slash-to-dot (car siglist))
    ,(cadr siglist) ,@(jde-parse-class-parse-complete-arg-signature
                      (nth 2 siglist))))

(defun jde-parse-class-lookup-method (method constants)
  (list (cadr (jde-parse-class-lookup-constant
               (cadr (jde-parse-class-lookup-constant
                       (cadr method) constants))
               constants))
        (cadr (jde-parse-class-lookup-constant
               (cadr 
                (jde-parse-class-lookup-constant (nth 2 method) constants))
               constants))
        (cadr (jde-parse-class-lookup-constant
               (nth 2
                (jde-parse-class-lookup-constant (nth 2 method) constants))
               constants))))

(defun get-bit-flags-for-byte (byte flag-vec)
  "Gets the bit flags for BYTE, given the flags that apply to each bit,
a vector of length 8 (one for each bit).  Nulls in the FLAG-VEC are 
taken to mean there is no flag for that byte, which causes the byte to be 
ignored.

For example: (get-bit-flags-for-byte 6 ['a 'b 'c 'd 'e 'f 'g 'h])
returns ('f 'g)"
  (let ((flags '()))
    (dotimes (i 8 flags)
      (when (and (aref flag-vec (- 7 i))
                 (> (logand (expt 2 i)  byte) 0))
        (add-to-list 'flags (aref flag-vec (- 7 i)))))))
          
(defun jde-parse-class-get-access-flags ()
  (do-and-advance-chars 2
    (let ((raw0 (char-int (char-after (point))))
          (raw1 (char-int (char-after (+ (point) 1)))))
      (append
       (get-bit-flags-for-byte raw0
                               [nil nil nil nil 'string 'abstract
                                    'interface 'native])
       (get-bit-flags-for-byte raw1
                               ['transient 'volatile 'synchronized 'final
                                           'static 'protected
                                           'private 'public])))))

(defun jde-parse-class-get-version ()
  "Return a list - (major-version minor-version)"
  (list (jde-parse-class-get-2byte 5)
                  (jde-parse-class-get-2byte 7)))

(defun jde-parse-class-get-2byte (point)
  "Gets the value of two bytes (0 - 65535) as an int" 
  (let ((b1 (char-int (char-after point)))
        (b2 (char-int (char-after (+ 1 point)))))
    (+ (* b1 256) b2)))

(defun jde-parse-class-get-next-2-bytes ()
  (do-and-advance-chars 2
    (jde-parse-class-get-2byte (point))))

(defun jde-parse-class-get-4byte (point &optional ignore-large-val)
  (let ((db1 (jde-parse-class-get-2byte point))
        (db2 (jde-parse-class-get-2byte (+ 2 point))))
    (if (< db1 2047)
      ;; don't go over the maxint in elisp (2047, since we have 1 more db1 could be 65536)
      (+ (* 65536 db1) db2)
      (if ignore-large-val
        0
        (error "Class file has a larger 4 byte value then emacs can handle")))))

(defun jde-parse-class-get-next-4-bytes-as-signed (&optional ignore-large-val)
  (let ((db1 (jde-parse-class-get-next-2-bytes))
        (db2 (jde-parse-class-get-next-2-bytes)))
    (if (> (logand 32768 db1) 0)  ;; if it's high-bit is set, then it's negative.
      (if (> db1 63488)
        (- (+ 1 (+ (* (- 65535 db1) 65536) (- 65535 db2))))
        (if ignore-large-val
          0
          (error "Class file has an unsigned int who is smaller than emacs can handle")))
      (jde-parse-class-get-4byte (- (point) 4)))))

(defun jde-parse-class-get-next-4-bytes (&optional ignore-large-val)
  (do-and-advance-chars 4
    (jde-parse-class-get-4byte (point) ignore-large-val)))

(defun jde-parse-class-get-constants ()
  "Returns a list of the constant ending location (not inclusive of
of the constants) and a vector of all constants"
  (let* ((count (- (jde-parse-class-get-2byte 9) 1))
         (const-vec (make-vector count '())))
    (goto-char 11) ;; start of constants
    (dotimes (i count const-vec)
      (let ((const (jde-parse-class-get-next-constant)))
        (aset const-vec i const)
        ;; doubles and longs take up two places on the const table. 
        (when (or (eq (car const) 'double)
                  (eq (car const) 'long))
          (aset const-vec (+ 1 i) nil)
          (setq i (+ 1 i)))))))

(defsubst jde-parse-class-get-long-constant (&optional ignore-large-val)
  (let ((qb1 (jde-parse-class-get-next-4-bytes ignore-large-val))
        (qb2 (jde-parse-class-get-next-4-bytes ignore-large-val)))
    (if (> qb2 0)
      (if ignore-large-val
        0
        (error "Class file has a large 8 byte value than emacs can handle"))
      qb1)))

(defsubst jde-parse-class-get-double-constant ()
  "NOT IMPLEMENTED YET"
  (do-and-advance-chars 8
    "0.0"))

(defsubst jde-parse-class-get-ref-constant ()
  (list (jde-parse-class-get-next-2-bytes) (jde-parse-class-get-next-2-bytes)))

(defsubst jde-parse-class-get-float-constant ()
  "NOT IMPLEMENTED YET"
  (do-and-advance-chars 4
    "0.0"))

(defsubst jde-parse-class-get-nameandtype-constant ()
  (list (jde-parse-class-get-next-2-bytes) (jde-parse-class-get-next-2-bytes)))

(defsubst jde-parse-class-get-utf8-constant ()
  (let* ((len (jde-parse-class-get-next-2-bytes))
         (result (encode-coding-string (buffer-substring (point)
                                                        (+ len (point)))
                                       jde-parse-class-encoding)))
    (goto-char (+ len (point)))
    result))

(defun jde-parse-class-get-next-constant ()
  (let ((const-type (char-int (char-after (point)))))
    (forward-char)
    (cond ((eq const-type 7)
            `(class ,(jde-parse-class-get-next-2-bytes)))
          ((eq const-type 9)
            `(field ,@(jde-parse-class-get-ref-constant)))
          ((eq const-type 10)
            `(method ,@(jde-parse-class-get-ref-constant)))
          ((eq const-type 11)
            `(interface-method ,@(jde-parse-class-get-ref-constant)))
          ((eq const-type 8)
            `(string ,(jde-parse-class-get-next-2-bytes)))
          ((eq const-type 3)
            `(integer ,(jde-parse-class-get-next-4-bytes t)))
          ((eq const-type 4)
            `(float ,(jde-parse-class-get-float-constant)))
          ((eq const-type 5)
            `(long ,(jde-parse-class-get-long-constant t)))
          ((eq const-type 6)
            `(double ,(jde-parse-class-get-double-constant)))
          ((eq const-type 12)
            `(name-and-type ,@(jde-parse-class-get-nameandtype-constant)))
          ((eq const-type 1)
            `(utf8 ,(jde-parse-class-get-utf8-constant))))))

(defconst jde-parse-class-opcode-vec
  [(nop . 1)  ;; 0
   (aconst_null . 1) ;; 1
   (iconst_m1 . 1)  ;; 2
   (iconst_0 . 1)  ;; 3
   (iconst_1 . 1)  ;; 4
   (iconst_2 . 1)  ;; 5
   (iconst_3 . 1)  ;; 6
   (iconst_4 . 1)  ;; 7
   (iconst_5 . 1)  ;; 8
   (lconst_0 . 1)  ;; 9
   (lconst_1 . 1)  ;; 10
   (fconst_0 . 1)  ;; 11
   (fconst_1 . 1)  ;; 12
   (fconst_2 . 1)  ;; 13
   (dconst_0 . 1)  ;; 14
   (dconst_1 . 1)  ;; 15
   (bipush . 2)  ;; 16
   (sipush . 3)  ;; 17
   (ldc . 2)  ;; 18
   (ldc_w . 3)  ;; 19
   (ldc2_w . 3)  ;; 20
   (iload . 2)  ;; 21
   (lload . 2)  ;; 22
   (fload . 2)  ;; 23
   (dload . 2)  ;; 24
   (aload . 2)  ;; 25
   (iload_0 . 1)  ;; 26
   (iload_1 . 1)  ;; 27
   (iload_2 . 1)  ;; 28
   (iload_3 . 1)  ;; 29
   (lload_0 . 1)  ;; 30
   (lload_1 . 1)  ;; 31
   (lload_2 . 1)  ;; 32
   (lload_3 . 1)  ;; 33
   (fload_0 . 1)  ;; 34
   (fload_1 . 1)  ;; 35
   (fload_2 . 1)  ;; 36
   (fload_3 . 1)  ;; 37
   (dload_0 . 1)  ;; 38
   (dload_1 . 1)  ;; 39
   (dload_2 . 1)  ;; 40
   (dload_3 . 1)  ;; 41
   (aload_0 . 1)  ;; 42
   (aload_1 . 1)  ;; 43
   (aload_2 . 1)  ;; 44
   (aload_3 . 1)  ;; 45
   (iaload . 1)  ;; 46
   (laload . 1)  ;; 47
   (faload . 1)  ;; 48
   (daload . 1)  ;; 49
   (aaload . 1)  ;; 50
   (baload . 1)  ;; 51
   (caload . 1)  ;; 52
   (saload . 1)  ;; 53
   (istore . 2)  ;; 54
   (lstore . 2)  ;; 55
   (fstore . 2)  ;; 56
   (dstore . 2)  ;; 57
   (astore . 2)  ;; 58
   (istore_0 . 1)  ;; 59
   (istore_1 . 1)  ;; 60
   (istore_2 . 1)  ;; 61
   (istore_3 . 1)  ;; 62
   (lstore_0 . 1)  ;; 63
   (lstore_1 . 1)  ;; 64
   (lstore_2 . 1)  ;; 65
   (lstore_3 . 1)  ;; 66
   (fstore_0 . 1)  ;; 67
   (fstore_1 . 1)  ;; 68
   (fstore_2 . 1)  ;; 69
   (fstore_3 . 1)  ;; 70
   (dstore_0 . 1)  ;; 71
   (dstore_1 . 1)  ;; 72
   (dstore_2 . 1)  ;; 73
   (dstore_3 . 1)  ;; 74
   (astore_0 . 1)  ;; 75
   (astore_1 . 1)  ;; 76
   (astore_2 . 1)  ;; 77
   (astore_3 . 1)  ;; 78
   (iastore . 1)  ;; 79
   (lastore . 1)  ;; 80
   (fastore . 1)  ;; 81
   (dastore . 1)  ;; 82
   (aastore . 1)  ;; 83
   (bastore . 1)  ;; 84
   (castore . 1)  ;; 85
   (sastore . 1)  ;; 86
   (pop . 1)  ;; 87
   (pop2 . 1)  ;; 88
   (dup . 1)  ;; 89
   (dup_x1 . 1)  ;; 90
   (dup_x2 . 1)  ;; 91
   (dup2 . 1)  ;; 92
   (dup2_x1 . 1)  ;; 93
   (dup2_x2 . 1)  ;; 94
   (swap . 1)  ;; 95
   (iadd . 1)  ;; 96
   (ladd . 1)  ;; 97
   (fadd . 1)  ;; 98
   (dadd . 1)  ;; 99
   (isub . 1)  ;; 100
   (lsub . 1)  ;; 101
   (fsub . 1)  ;; 102
   (dsub . 1)  ;; 103
   (imul . 1)  ;; 104
   (lmul . 1)  ;; 105
   (fmul . 1)  ;; 106
   (dmul . 1)  ;; 107
   (idiv . 1)  ;; 108
   (ldiv . 1)  ;; 109
   (fdiv . 1)  ;; 110
   (ddiv . 1)  ;; 111
   (irem . 1)  ;; 112
   (lrem . 1)  ;; 113
   (frem . 1)  ;; 114
   (drem . 1)  ;; 115
   (ineg . 1)  ;; 116
   (lneg . 1)  ;; 117
   (fneg . 1)  ;; 118
   (dneg . 1)  ;; 119
   (ishl . 1)  ;; 120
   (lshl . 1)  ;; 121
   (ishr . 1)  ;; 122
   (lshr . 1)  ;; 123
   (iushr . 1)  ;; 124
   (lushr . 1)  ;; 125
   (iand . 1)  ;; 126
   (land . 1)  ;; 127
   (ior . 1)  ;; 128
   (lor . 1)  ;; 129
   (ixor . 1)  ;; 130
   (lxor . 1)  ;; 131
   (iinc . 3)  ;; 132
   (i2l . 1)  ;; 133
   (i2f . 1)  ;; 134
   (i2d . 1)  ;; 135
   (l2i . 1)  ;; 136
   (l2f . 1)  ;; 137
   (l2d . 1)  ;; 138
   (f2i . 1)  ;; 139
   (f2l . 1)  ;; 140
   (f2d . 1)  ;; 141
   (d2i . 1)  ;; 142
   (d2l . 1)  ;; 143
   (d2f . 1)  ;; 144
   (i2b . 1)  ;; 145
   (i2c . 1)  ;; 146
   (i2s . 1)  ;; 147
   (lcmp . 1)  ;; 148
   (fcmpl . 1)  ;; 149
   (fcmpg . 1)  ;; 150
   (dcmpl . 1)  ;; 151
   (dcmpg . 1)  ;; 152
   (ifeq . 3)  ;; 153
   (ifne . 3)  ;; 154
   (iflt . 3)  ;; 155
   (ifge . 3)  ;; 156
   (ifgt . 3)  ;; 157
   (ifle . 3)  ;; 158
   (if_icmpeq . 3)  ;; 159
   (if_icmpne . 3)  ;; 160
   (if_icmplt . 3)  ;; 161
   (if_icmpge . 3)  ;; 162
   (if_icmpgt . 3)  ;; 163
   (if_icmple . 3)  ;; 164
   (if_acmpeq . 3)  ;; 165
   (if_acmpne . 3)  ;; 166
   (goto . 3)  ;; 167
   (jsr . 3)  ;; 168
   (ret . 2)  ;; 169
   (tableswitch . -1)  ;; 170  - variable length instruction
   (lookupswitch . -1)  ;; 171   - variable length instruction
   (ireturn . 1)  ;; 172
   (lreturn . 1)  ;; 173
   (freturn . 1)  ;; 174
   (dreturn . 1)  ;; 175
   (areturn . 1)  ;; 176
   (return . 1)  ;; 177
   (getstatic . 3)  ;; 178
   (putstatic . 3)  ;; 179
   (getfield . 3)  ;; 180
   (putfield . 3)  ;; 181
   (invokevirtual . 3)  ;; 182
   (invokespecial . 3)  ;; 183
   (invokestatic . 3)  ;; 184
   (invokeinterface . 5)  ;; 185
   (unused . -1) ;; 186
   (new . 3)  ;; 187
   (newarray . 2)  ;; 188
   (anewarray . 3)  ;; 189
   (arraylength . 1)  ;; 190
   (athrow . 1) ;; 191
   (checkcast . 3) ;; 192
   (instanceof . 3)  ;; 193
   (monitorcenter . 1)  ;; 194
   (monitorexit . 1)  ;; 195
   (wide . -1)  ;; 196   - variable length instruction
   (multianewarray . 4)  ;; 197
   (ifnull . 3)  ;; 198
   (ifnonnull . 3)  ;; 199
   (goto_w . 5)  ;; 200
   (jsr_w . 5)  ;; 201
   ]
  "A vector storing the java opcodes.  Each position in the vector is
in the position of it's bytecode number.  For example, if, when
reading a class file we come across bytecode 0, we can just look at
this vector to see both the name of the instruction, and the size of
the operation in bytes.  A few opcodes have variable length, so those
must be calculated at runtime.")
    
(provide 'jde-parse-class)

;; $Log: jde-parse-class.el,v $
;; Revision 1.9  2005/01/20 04:53:21  paulk
;; Fix bug in jde-parse-class-get-code-attribute that caused it to compute the length of iinc JVM instructions
;; as 5 bytes when they are in fact 6 bytes long. Thanks to Jack Klebanoff.
;;
;; Revision 1.8  2004/07/09 05:17:20  paulk
;; Eliminated initialization of loop variable i to 0 in
;; jde-parse-class-get-constants. It does not seem necessssary as dotimes
;; initializes the variable itself to 0. Further, the initialization
;; turned i into a free variable, causing a compiler warning. I tested
;; jde-parse-class-get-constants before and after this change and the
;; change made no difference.
;;
;; Revision 1.7  2003/06/19 17:01:39  ahyatt
;; Now compatible with emacs and xemacs versions without MULE
;;
;; Revision 1.5  2003/03/08 06:32:45  ahyatt
;; Now parses exception information
;;
;; Revision 1.4  2002/12/01 02:39:58  ahyatt
;; Support for jde-xref's latest improvements, which require more
;; information from each class.
;;
;; Revision 1.3  2002/11/21 04:26:33  ahyatt
;; Changed my e-mail address to andy_jde@thehyatts.net
;;
;; Revision 1.2  2002/11/21 04:03:47  ahyatt
;; Fixed a bug in jde-parse-class where two functions had the same
;; definition.  The fix involved renamed a few classes to keep
;; consistency.  jde-xref had to change as well.
;;
;; Removed a (newline) function call in jde-xref and replaced it with a "\n"
;;
;; In jde-xref, rewrote the parts of jde-xref-make-db-from-path which
;; dealt with keeping track of the already parsed classes. Previous
;; solution was really kludgey, this solution is only somewhat kludgey.
;; I'm still thinking about this problem.
;;
;; Revision 1.1  2002/11/18 07:02:18  paulk
;; Initial release.
;;

;; end of jde-xref.el  