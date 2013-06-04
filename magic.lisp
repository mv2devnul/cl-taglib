;;; magic.lisp
(in-package :cl-user)
(defpackage :magic
  (:export :init-magic :get-mime-string :finish-magic)
  (:use :cl :ccl))
(in-package :magic)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (lib '("/usr/lib/x86_64-linux-gnu/libmagic.so.1"))
    (ccl:open-shared-library lib))
  (dolist (f '("libmagic"))
	(ccl:use-interface-dir f)))


(defparameter *magic-handle* nil)

(defun string+ (&rest args)
  (format nil "~{~a~}" args))

(defun init-magic-files ()
  (let* ((home (osicat-posix:getenv "HOME"))
		 (files "")
		 (magic-env (osicat-posix:getenv "MAGIC")))

	;; if MAGIC environment variable is set, then just return that
	(when magic-env 
	  (if (osicat:file-exists-p magic-env)
		  (return-from init-magic-files magic-env)))

	;; so long as the HOME env variable is set, look for files there.
	;; if found, just return
	(when home
	  (let ((home-files (list (string+ home "/.magic.mgc")
							  (string+ home "/.magic"))))
		(dolist (f home-files)
		  (when (osicat:file-exists-p f)
			(return-from init-magic-files f)))))

	;; if no MAGIC/HOME files found, use the defaults
	(if (osicat:file-exists-p "/etc/magic") (setf files "/etc/magic"))
	(if (osicat:file-exists-p "/usr/share/misc/magic.mgc")
		(setf files (string+ files ":/usr/share/misc/magic.mgc"))
		(if (osicat:file-exists-p "/usr/share/misc/magic")
			(setf files (string+ files ":/usr/share/misc/magic"))))
	(if files (return-from init-magic-files files))

	;; last ditch effort.  search the /usr/share/misc dir for files
	(if (osicat:directory-exists-p "/usr/share/misc/magic")
		(osicat:walk-directory "/usr/share/misc/magic" 
							   (lambda (fn) (setf files (string+ files (if files ":") fn)))))

	files))

;;; use libmagic to get mime type
(defparameter *default-magic-flags* (logior #$MAGIC_SYMLINK #$MAGIC_MIME_TYPE))
(defparameter *default-magic-files* nil)

(defclass magic ()
  ((magic-cookie :initform (%null-ptr))
   (magic-flags  :initform *default-magic-flags* :initarg :magic-flags)
   (magic-files  :initform *default-magic-files* :initarg :magic-files)))

(defmethod print-object ((me magic) stream)
  (with-slots (magic-cookie magic-files magic-flags) me
	(format stream "magic-cookie: <~a>; magic-flags: <~8,'0x>; magic-files: <~a>~%"
			magic-cookie magic-flags magic-files)))

(define-condition bad-magic () 
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream) 
			 (format stream "problem with libmagic in  <~a> with object <~a>: message<~a>"
					 (location condition) (object condition) (message condition)))))

(defmethod initialize-instance :after ((me magic) &key)
  (with-slots (magic-cookie magic-files magic-flags) me
	(setf magic-cookie (#_magic_open magic-flags))
	(if (%null-ptr-p magic-cookie)
		(error 'bad-magic :location "magic initialize method"
						  :object magic-cookie
						  :message (format nil "could not get magic cookie")))

	(let ((file-arg (if magic-files
						(ccl::make-cstring magic-files) 
						(%null-ptr))))
	  (if (= -1 (#_magic_load magic-cookie file-arg))
		  (error 'bad-magic :location "magic initialize method"
							:object magic-cookie
							:message (format nil "magic error: ~a" (#_magic_error magic-cookie)))))))

(defmethod close-magic ((me magic))
  (with-slots (magic-cookie) me
	(when (not (%null-ptr-p magic-cookie))
	  (#_magic_close magic-cookie))))

(defmethod get-magic ((me magic) file-name)
  (with-slots (magic-cookie) me
	(let ((ret-type (with-cstrs ((p file-name)) (#_magic_file magic-cookie p))))
	  (if (not (%null-ptr-p ret-type))
		  (%get-cstring ret-type)
		  ""))))

(defun init-magic (&key (magic-flags *default-magic-flags*) (magic-files *default-magic-files*))
  (if *magic-handle*
	  (warn "trying to initialize magic when already initalized")
	  (setf *magic-handle* (make-instance 'magic :magic-flags magic-flags :magic-files magic-files))))


(defun finish-magic ()
  (if (not *magic-handle*)
	  (warn "trying to finish magic when it hasn't been initialized")
	  (progn 
		(close-magic *magic-handle*)
		(setf *magic-handle* nil))))

(defun get-mime-string (fn)
  (if (not *magic-handle*)
	  (init-magic))
  (get-magic *magic-handle* fn))
