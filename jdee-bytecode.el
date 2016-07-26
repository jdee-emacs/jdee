;; jdee-bytecode.el --- Parse bytecode from Java class file.
;;
;; Copyright (C) 2002, 2004, 2005 Andrew Hyatt
;; Copyright (C) 2009 by Paul Landes
;;
;; Author: Andrew Hyatt <andy_jde@thehyatts.net>
;; Maintainer: Andrew Hyatt, Paul Landes <landes <at> mailc dt net>
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

;;; Commentary:

;; This file parses Java .class files to get information about what
;; methods call what other methods.  On the way there, we happen to
;; pick up all sorts of useful information.
;;
;; Also, we could return a semantic-compatible structure, but at the
;; moment there is no point in doing this.
;;
;; There are two public functions, `jdee-bytecode', which returns
;; everything we have parsed about the class.  The resultant structures
;; can be passed to `jdee-bytecode-extract-method-calls' which
;; returns the method calls a particular class uses, and
;; `jdee-bytecode-extract-interfaces' to retrieve the interfaces a
;; class uses, and `jdee-bytecode-extract-classname' to get the
;; fully qualified classname the class represents.  This is used in
;; jdee-xref.el
;;
;; There is one other functions that is generally useful, and one
;; macro.  The function is `get-bit-flags-for-byte', which, given a
;; single byte in decimal (0 - 255), and a vector of flags
;; corresponding to bytes, will return a list of which flags are on.
;; The macro `do-and-advance-chars' wraps code, and after executing the
;; code, advances the current point a certain number of chars (here
;; equivalent to bytes).  This is useful for the kind of parsing we are
;; doing.

;;; Code:

;; for XEmacs compatibilty
(unless (fboundp 'char-int)
  (defalias 'char-int 'identity))

(defmacro do-and-advance-chars (num &rest body)
  "Execute BODY, and then advance the point NUM bytes, called like
      (do-and-advance-chars 2 ...)"
  (let ((result-sym (cl-gensym)))
  `(let ((,result-sym (progn ,@body)))
     (goto-char (+ (point) ,num))
     ,result-sym)))

(defsubst jdee-bytecode-get-next-const-val (constants)
  (cadr (jdee-bytecode-lookup-constant
	 (jdee-bytecode-get-next-2-bytes) constants)))

(setq jdee-bytecode-encoding 'utf-8)

(defun jdee-bytecode-slash-to-dot (string)
  (subst-char-in-string ?/ ?. string))

(defun jdee-bytecode (class-file)
  "Parse the class in CLASS-FILE, and return an alist, with the following
keys: version, this-class, interfaces, fields, and methods."
  ;; we must visit the file in such a way as to not use any encoding,
  ;; else our parsing could be severely messed up
  (unless (file-exists-p class-file)
    (error (concat "Class file " class-file " does not exist")))
  (let ((buf (find-file-noselect class-file nil t)))
    (set-buffer buf)
    (let* ((version (jdee-bytecode-get-version)) ;do first to validate class file version early
	   (constants (jdee-bytecode-get-constants))
	   (access (jdee-bytecode-get-class-access-flags))
	   (this-class
	     (subst-char-in-string
	      ?/ ?. (cadr (jdee-bytecode-lookup-constant
			     (jdee-bytecode-get-next-const-val constants)
			      constants))))
	   (superclass (jdee-bytecode-slash-to-dot
			(do-and-advance-chars 2
			  (let ((val (jdee-bytecode-get-const-ref
				      (point) constants)))
			    (if (eq (cadr val) 0)
			      nil ;; only Object can have no superclass
			      (cadr (jdee-bytecode-lookup-constant
				     (cadr val) constants)))))))
	   (interfaces (jdee-bytecode-get-interfaces constants))
	   (fields (jdee-bytecode-get-fields constants))
	   (methods (jdee-bytecode-get-methods constants))
	   (attributes (jdee-bytecode-get-attributes constants)))
      (kill-buffer buf)
      `((version . ,version) (access . ,access)
	(this-class . ,this-class) (superclass . ,superclass)
	(interfaces . ,interfaces)
	(fields . ,fields)
	(methods . ,methods)
	(attributes . ,attributes)))))

(defun jdee-bytecode-extract-caught-exception-types (info)
  "Returns a list of a two-element of list of method signatures to
  caught exception types for each method"
  (mapcar (lambda (method)
	    (list `(,(cdr (nth 2 info))
		      ,(cdr (assoc 'name method))
		      ,@(cdr (assoc 'descriptor method)))
		  (mapcar (lambda (class) (when class
					    (jdee-bytecode-slash-to-dot class)))
			  (cdr (assoc
				'catch-types
				(cdr
				 (assoc 'code
					(cdr
					 (assoc 'attributes
						method)))))))))
	  (cdr (assoc 'methods info))))

(defun jdee-bytecode-extract-thrown-exception-types (info)
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

(defun jdee-bytecode-extract-method-calls (info)
  "Return a cons of a method signature, and a list of the methods it
calls.  Each method in the list is a list of the calling method or
line number if available, the Class, method, and return value, and
arguments.  INFO is the result of `jdee-bytecode'"
  (cl-mapcan
   (lambda (method)
     (mapcar (lambda (attr)
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

(defun jdee-bytecode-extract-interfaces (info)
  "Returns a list of fully qualified interface names that the class
  implements.  INFO is the result of `jdee-bytecode'"
  (mapcar 'jdee-bytecode-slash-to-dot
	  (cdr (assoc 'interfaces info))))

(defun jdee-bytecode-extract-superclass (info)
  "Returns a list of fully qualified class names that are superclasses
  of the parsed class"
  (jdee-bytecode-slash-to-dot (cdr (assoc 'superclass info))))

(defun jdee-bytecode-extract-method-signatures (info)
  "Returns a list of method names that the class implements"
  (mapcar (lambda (method-info) (cons (cdr (assoc 'name method-info))
				      (cdr (assoc 'descriptor method-info))))
	  (cdr (assoc 'methods info))))

(defun jdee-bytecode-extract-field-signatures (info)
  "Return a list of field names that the class defines"
  (mapcar (lambda (field-info)
	    (list (cdr (assoc 'name field-info))
		  (cdr (assoc 'descriptor field-info))))
	  (cdr (assoc 'fields info))))

(defun jdee-bytecode-extract-classname (info)
  "Returns the fully qualified classname that the class implements.
INFO is the result of `jdee-bytecode'"
  (cdr (assoc 'this-class info)))

(defun jdee-bytecode-extract-sourcefile (info)
  (cdr (assoc 'source-file (cdr (assoc 'attributes info)))))

(defun jdee-bytecode-get-const-ref (point constants)
  "Look at point in the class file and read it as a reference to the array.
Returns the constant information contained at the reference"
  ;; we have to subtract one since the array index starts at 1
  ;; (according to the class file, not us
  (jdee-bytecode-lookup-constant (jdee-bytecode-get-2byte point)
				   constants))

(defun jdee-bytecode-lookup-constant (num constants)
  "From an index value, get the constant for that index"
  (aref constants (- num 1)))

(defsubst jdee-bytecode-get-next-length-val ()
  (jdee-bytecode-get-next-2-bytes))

(defun jdee-bytecode-get-interfaces (constants)
  (let ((num (jdee-bytecode-get-next-length-val))
	(interfaces '()))
    (dotimes (i num interfaces)
      (add-to-list 'interfaces (cadr (jdee-bytecode-lookup-constant
				     (jdee-bytecode-get-next-const-val
				      constants) constants))))))

(defun jdee-bytecode-get-methods (constants)
  (let ((num (jdee-bytecode-get-next-length-val))
	(methods '()))
    (dotimes (i num (nreverse methods))
      (add-to-list 'methods (jdee-bytecode-get-method constants)))))

(defun jdee-bytecode-get-method (constants)
  (list (cons 'access-flags (jdee-bytecode-get-method-access-flags))
        (cons 'name (jdee-bytecode-get-next-const-val constants))
	(cons 'descriptor (jdee-bytecode-parse-complete-arg-signature
			   (jdee-bytecode-get-next-const-val constants)))
	(cons 'attributes (jdee-bytecode-get-attributes constants))))

(defun jdee-bytecode-get-fields (constants)
  (let ((num (jdee-bytecode-get-next-length-val))
	(fields '()))
    (dotimes (i num (nreverse fields))
      (add-to-list 'fields (jdee-bytecode-get-field constants)))))

(defun jdee-bytecode-get-field (constants)
  (list (cons 'access-flags (jdee-bytecode-get-field-access-flags))
        (cons 'name (jdee-bytecode-get-next-const-val constants))
	(cons 'descriptor (car (jdee-bytecode-parse-first-arg
			   (jdee-bytecode-get-next-const-val constants))))
	(cons 'attributes (jdee-bytecode-get-attributes constants))))

(defun jdee-bytecode-get-attributes (constants)
  (let ((num (jdee-bytecode-get-next-length-val))
	(attributes '()))
    (dotimes (i num attributes)
      (add-to-list 'attributes (jdee-bytecode-get-attribute constants)))))

(defun jdee-bytecode-get-attribute (constants)
  (let ((attribute-type (jdee-bytecode-get-next-const-val constants))
	(numbytes (jdee-bytecode-get-next-4-bytes (point))))
    ;; TODO: implement the rest of the common attribute types
    (cond ((equal attribute-type "Code")
	    (cons 'code
		  (jdee-bytecode-get-code-attribute numbytes constants)))
	  ((equal attribute-type "LineNumberTable")
	    (cons 'line-number-table
		  (jdee-bytecode-get-line-number-attribute)))
	  ((equal attribute-type "Exceptions")
	    (cons 'throws
		  (jdee-bytecode-get-exception-attribute constants)))
	  ((equal attribute-type "SourceFile")
	    (cons 'source-file
		  (jdee-bytecode-get-source-file-attribute constants)))
	  (t (forward-char numbytes)))))

(defun jdee-bytecode-get-source-file-attribute (constants)
  (jdee-bytecode-get-next-const-val constants))

(defun jdee-bytecode-get-exception-attribute (constants)
  (let ((num (jdee-bytecode-get-next-length-val))
	(classes '()))
    (dotimes (i num classes)
      (add-to-list 'classes
		   (cadr (jdee-bytecode-lookup-constant
			  (jdee-bytecode-get-next-const-val constants)
			  constants))))))

(defun jdee-bytecode-get-line-number-attribute ()
  (let ((num (jdee-bytecode-get-next-length-val))
	(line-number-infos '()))
    (dotimes (i num (nreverse line-number-infos))
      (add-to-list
       'line-number-infos
       (cons (jdee-bytecode-get-next-2-bytes)
	     (jdee-bytecode-get-next-2-bytes))))))

(defun jdee-bytecode-get-code-attribute (numbytes constants)
  (goto-char (+ (point) 4)) ;; no one cares about max_stack and max_locals, right?
  (let* ((code-numbytes (jdee-bytecode-get-next-4-bytes (point)))
	 (end-point (+ (point) code-numbytes))
	 (begin-point (point))
	 (calls '())
	 (catch-types '()))
    (while (< (point) end-point)
      (let* ((opcode-info (aref jdee-bytecode-opcode-vec
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
			 (jdee-bytecode-parse-method-signature
			  (jdee-bytecode-lookup-method
			   (jdee-bytecode-get-const-ref (point) constants)
			   constants))))))
              ((eq opcode-val 'invokedynamic)
               ;; TODO -- possible to parse out a call?
               (forward-char (- opcode-length 1)))
	      ((eq opcode-val 'tableswitch)
		;; skip padding to go to a multiple of 4 from the begin-point.
		;; The second mod is to make sure on an offset of 4 we really don't skip anything
		(forward-char (mod (- 4 (mod (- (point) begin-point) 4)) 4))
		(forward-char 4)  ;; we don't care about default
		(let ((diff (jdee-bytecode-diff-next-two-4-bytes-as-signed)))
		  (forward-char (* 4 (+ diff 1)))))
	      ((eq opcode-val 'lookupswitch)
		(forward-char (mod (- 4 (mod (- (point) begin-point) 4)) 4))
		(forward-char 4)
		(forward-char (* 8 (jdee-bytecode-get-next-4-bytes-as-signed))))
	      ((eq opcode-val 'wide)
		(let ((opcode2 (char-int (char-after))))
		  (if (eq opcode2 'iinc)
		    (forward-char 5)
		    (forward-char 2))))
              ((< opcode-length 0)
               (error "Invalid opcode-length %s" opcode-info))
	      (t (forward-char (- opcode-length 1))))))
    (let ((num-exceptions (jdee-bytecode-get-next-length-val)))
      (dotimes (i num-exceptions)
	(let ((type (cdr (assoc 'catch-type
				 (jdee-bytecode-get-exception-handler
				  constants)))))
	  (when type
	    (add-to-list 'catch-types type)))))
    (let ((attributes (jdee-bytecode-get-attributes constants)))
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

(defun jdee-bytecode-get-exception-handler (constants)
  (list (cons 'start-pc (jdee-bytecode-get-next-2-bytes))
    (cons 'end-pc (jdee-bytecode-get-next-2-bytes))
    (cons 'handler-pc (jdee-bytecode-get-next-2-bytes))
    (cons 'catch-type (let ((val (jdee-bytecode-get-next-2-bytes)))
			(when (> val 0)
			  (cadr (jdee-bytecode-lookup-constant
				 (cadr (jdee-bytecode-lookup-constant val constants))
			      constants)))))))

(defun jdee-bytecode-parse-first-arg (sig)
  (let ((char (char-to-string (string-to-char sig))))
    (cond ((equal char "B") `("byte" . 1))
	  ((equal char "C") `("char" . 1))
	  ((equal char "D") `("double" . 1))
	  ((equal char "F") `("float" . 1))
	  ((equal char "I") `("int" . 1))
	  ((equal char "J") `("long" . 1))
	  ((equal char "L") (let ((endpos (string-match ";" sig)))
			      (cons (jdee-bytecode-slash-to-dot
				     (substring sig 1
						endpos))
				    (+ 1 endpos))))
	  ((equal char "S") `("short" . 1))
	  ((equal char "Z") `("boolean" . 1))
	  ((equal char "[") (let ((rest (jdee-bytecode-parse-first-arg
					 (substring sig 1))))
			      (cons (concat (car rest) "[]") (+ (cdr rest) 1))))
          (t (error (concat "Could not find char " char))))))

(defun jdee-bytecode-parse-arg-signature (sig)
  (when (> (length sig) 0)
    (let ((arg (jdee-bytecode-parse-first-arg sig)))
      (if (> (cdr arg) (length sig))
	(list (car arg))
	(cons (car arg) (jdee-bytecode-parse-arg-signature
			 (substring sig (cdr arg))))))))


(defun jdee-bytecode-parse-complete-arg-signature (sig)
  (let* ((match (string-match "(\\(.*\\))\\(.*\\)" sig)))
    (if match
      (let ((args (match-string 1 sig))
	    (return-val (match-string 2 sig)))
	(list (unless (equal "V" return-val)
		(car (jdee-bytecode-parse-arg-signature return-val)))
	      (jdee-bytecode-parse-arg-signature args)))
      (list nil (jdee-bytecode-parse-arg-signature sig)))))


(defun jdee-bytecode-parse-method-signature (siglist)
  `(,(jdee-bytecode-slash-to-dot (car siglist))
    ,(cadr siglist) ,@(jdee-bytecode-parse-complete-arg-signature
		      (nth 2 siglist))))

(defun jdee-bytecode-lookup-method (method constants)
  (list (cadr (jdee-bytecode-lookup-constant
	       (cadr (jdee-bytecode-lookup-constant
		       (cadr method) constants))
	       constants))
	(cadr (jdee-bytecode-lookup-constant
	       (cadr
		(jdee-bytecode-lookup-constant (nth 2 method) constants))
	       constants))
	(cadr (jdee-bytecode-lookup-constant
	       (nth 2
		(jdee-bytecode-lookup-constant (nth 2 method) constants))
	       constants))))

(defun get-bit-flags-for-byte (byte flag-vec)
  "Gets the bit flags for BYTE, given the flags that apply to each bit,
a vector of length 8 (one for each bit).  Nulls in the FLAG-VEC are
taken to mean there is no flag for that bit, which causes the bit to be
ignored.

For example: (get-bit-flags-for-byte 6 ['a 'b 'c 'd 'e 'f 'g 'h])
returns ('f 'g)"
  (let ((flags '()))
    (dotimes (i 8 flags)
      (when (and (aref flag-vec (- 7 i))
		 (> (logand (expt 2 i)  byte) 0))
	(add-to-list 'flags (aref flag-vec (- 7 i)))))
    flags))

(defun jdee-bytecode-get-access-flags (bits0 bits1)
  (do-and-advance-chars 2
    (let ((raw0 (char-int (char-after (point))))
	  (raw1 (char-int (char-after (+ (point) 1)))))
      (append
       (get-bit-flags-for-byte raw0 bits0)
       (get-bit-flags-for-byte raw1 bits1)))))

(defun jdee-bytecode-get-class-access-flags ()
  (jdee-bytecode-get-access-flags
   [nil enum annotation synthetic nil abstract interface nil]
   [nil nil super final nil nil nil public]))

(defun jdee-bytecode-get-method-access-flags ()
  (jdee-bytecode-get-access-flags
   [nil nil nil synthetic strict abstract nil native]
   [varargs bridge synchronized final static protected private public]))

(defun jdee-bytecode-get-field-access-flags ()
  (jdee-bytecode-get-access-flags
   [nil enum nil synthetic nil nil nil nil]
   [transient volatile nil final static protected private public]))

(defun jdee-bytecode-get-version ()
  "Return a list - (major-version minor-version)"
  (let ((minor (jdee-bytecode-get-2byte 5))
        (major (jdee-bytecode-get-2byte 7)))
    (if (or
         (and (eq major 45) (eq minor 3)) ;Java v 1.1
         (and (eq major 46) (eq minor 0)) ;Java v 1.2 -- strictfp
         (and (eq major 47) (eq minor 0)) ;Java v 1.3
         (and (eq major 48) (eq minor 0)) ;Java v 1.4
         (and (eq major 49) (eq minor 0)) ;Java v 1.5 -- new attributes for generics etc
         (and (eq major 50) (eq minor 0)) ;Java v 1.6 -- StackMaps
         (and (eq major 51) (eq minor 0)) ;Java v 1.7 -- InvokeDynamic
         (and (eq major 52) (eq minor 0)) ;Java v 1.8 -- RuntimeVisibleTypeAnnotations, MethodParameters etc
         )
        (list major minor)
      (error "Unsupported class file version: %s.%s" major minor))))

(defun jdee-bytecode-get-2byte (point)
  "Gets the value of two bytes (0 - 65535) as an int"
  (let ((b1 (char-int (char-after point)))
	(b2 (char-int (char-after (+ 1 point)))))
    (+ (* b1 256) b2)))

(defun jdee-bytecode-get-next-2-bytes ()
  (do-and-advance-chars 2
    (jdee-bytecode-get-2byte (point))))

(defun jdee-bytecode-get-4byte (point &optional ignore-large-val)
  (let ((db1 (jdee-bytecode-get-2byte point))
	(db2 (jdee-bytecode-get-2byte (+ 2 point))))
    (if (< db1 2047)
      ;; don't go over the maxint in elisp (2047, since we have 1 more db1 could be 65536)
      (+ (* 65536 db1) db2)
      (if ignore-large-val
	0
	(error "Class file has a larger 4 byte value then emacs can handle")))))

(defun jdee-bytecode-get-next-4-bytes-as-signed (&optional ignore-large-val)
  (let ((db1 (jdee-bytecode-get-next-2-bytes))
	(db2 (jdee-bytecode-get-next-2-bytes)))
    (if (> (logand 32768 db1) 0)  ;; if its high-bit is set, then it's negative.
      (if (> db1 63488)
	(- (+ 1 (+ (* (- 65535 db1) 65536) (- 65535 db2))))
	(if ignore-large-val
	  0
	  (error "Class file has an unsigned int who is smaller than emacs can handle")))
      (jdee-bytecode-get-4byte (- (point) 4)))))

(defun jdee-bytecode-diff-next-two-4-bytes-as-signed ()
  (let ((low1 (jdee-bytecode-get-next-2-bytes))
        (low2 (jdee-bytecode-get-next-2-bytes))
        (high1 (jdee-bytecode-get-next-2-bytes))
        (high2 (jdee-bytecode-get-next-2-bytes)))
    (if (and (> (logand 32768 low1) 0)
             (<= (logand 32768 high1) 0))
        (+ (* 65536 (- (+ high1 65535) low1)) (- (+ high2 65536) low2))
      (+ (* 65535 (- high1 low1)) (- high2 low2)))))

(defun jdee-bytecode-get-next-4-bytes (&optional ignore-large-val)
  (do-and-advance-chars 4
    (jdee-bytecode-get-4byte (point) ignore-large-val)))

(defun jdee-bytecode-get-constants ()
  "Returns a list of the constant ending location (not inclusive of
of the constants) and a vector of all constants"
  (let* ((count (- (jdee-bytecode-get-2byte 9) 1))
	 (const-vec (make-vector count '())))
    (goto-char 11) ;; start of constants
    (dotimes (i count const-vec)
      (let ((const (jdee-bytecode-get-next-constant)))
	(aset const-vec i const)
	;; doubles and longs take up two places on the const table.
	(when (or (eq (car const) 'double)
		  (eq (car const) 'long))
	  (aset const-vec (+ 1 i) nil)
	  (setq i (+ 1 i)))))))

(defsubst jdee-bytecode-get-long-constant (&optional ignore-large-val)
  (let ((qb1 (jdee-bytecode-get-next-4-bytes ignore-large-val))
	(qb2 (jdee-bytecode-get-next-4-bytes ignore-large-val)))
    (if (> qb2 0)
      (if ignore-large-val
	0
	(error "Class file has a large 8 byte value than emacs can handle"))
      qb1)))

(defsubst jdee-bytecode-get-double-constant ()
  "NOT IMPLEMENTED YET"
  (do-and-advance-chars 8
    "0.0"))

(defsubst jdee-bytecode-get-ref-constant ()
  (list (jdee-bytecode-get-next-2-bytes) (jdee-bytecode-get-next-2-bytes)))

(defsubst jdee-bytecode-get-float-constant ()
  "NOT IMPLEMENTED YET"
  (do-and-advance-chars 4
    "0.0"))

(defsubst jdee-bytecode-get-nameandtype-constant ()
  (list (jdee-bytecode-get-next-2-bytes) (jdee-bytecode-get-next-2-bytes)))

(defsubst jdee-bytecode-get-utf8-constant ()
  (let* ((len (jdee-bytecode-get-next-2-bytes))
	 (result (encode-coding-string (buffer-substring (point)
							(+ len (point)))
				       jdee-bytecode-encoding)))
    (goto-char (+ len (point)))
    result))

(defsubst jdee-bytecode-get-method-handle ()
  (let ((kind (do-and-advance-chars 1 (char-int (char-after (point)))))
        (index (jdee-bytecode-get-next-2-bytes)))
    (list
     (cond ((eq kind 1) 'get-field)
           ((eq kind 2) 'get-static)
           ((eq kind 3) 'put-field)
           ((eq kind 4) 'put-static)
           ((eq kind 5) 'invoke-virtual)
           ((eq kind 6) 'invoke-static)
           ((eq kind 7) 'invoke-special)
           ((eq kind 8) 'new-invoke-special)
           ((eq kind 9) 'invoke-interface)
           (t (error "Unrecognized MethodHandle_info kind: %s" kind)))
     index)))

(defsubst jdee-bytecode-get-method-type ()
  (list (jdee-bytecode-get-next-2-bytes)))

(defsubst jdee-bytecode-get-invoke-dynamic ()
  (list (jdee-bytecode-get-next-2-bytes)
        (jdee-bytecode-get-next-2-bytes)))

(defun jdee-bytecode-get-next-constant ()
  (let ((const-type (char-int (char-after (point)))))
    (forward-char)
    (cond ((eq const-type 7)
           `(class ,(jdee-bytecode-get-next-2-bytes)))
	  ((eq const-type 9)
           `(field ,@(jdee-bytecode-get-ref-constant)))
	  ((eq const-type 10)
           `(method ,@(jdee-bytecode-get-ref-constant)))
	  ((eq const-type 11)
           `(interface-method ,@(jdee-bytecode-get-ref-constant)))
	  ((eq const-type 8)
           `(string ,(jdee-bytecode-get-next-2-bytes)))
	  ((eq const-type 3)
           `(integer ,(jdee-bytecode-get-next-4-bytes t)))
	  ((eq const-type 4)
           `(float ,(jdee-bytecode-get-float-constant)))
	  ((eq const-type 5)
           `(long ,(jdee-bytecode-get-long-constant t)))
	  ((eq const-type 6)
           `(double ,(jdee-bytecode-get-double-constant)))
	  ((eq const-type 12)
           `(name-and-type ,@(jdee-bytecode-get-nameandtype-constant)))
	  ((eq const-type 1)
           `(utf8 ,(jdee-bytecode-get-utf8-constant)))
          ((eq const-type 15)
           `(method-handle ,@(jdee-bytecode-get-method-handle)))
          ((eq const-type 16)
           `(method-type ,@(jdee-bytecode-get-method-type)))
          ((eq const-type 18)
           `(invoke-dynamic ,@(jdee-bytecode-get-invoke-dynamic)))
          (t
           (error "Unrecognized constant type: %s" const-type)))))

(defconst jdee-bytecode-opcode-vec
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
   (invokedynamic . 4) ;; 186
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

(provide 'jdee-bytecode)

;;; jdee-bytecode.el ends here
