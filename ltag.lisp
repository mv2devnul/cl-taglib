;;; ltag.lisp
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package :cl-user)
(defpackage :ltag
  (:use :cl :ccl)
  (:export :open-audio-file :close-audio-file :save-audio-file :with-open-audio-file
		   :bad-path :bad-file :bad-tag :bad-audioproperties
		   :af-path :af-type :af-title :af-artist :af-album :af-comment :af-genre :af-year :af-track
		   :af-length :af-bitrate :af-samplerate :af-channels))
(in-package :ltag)

;;; load up any shared libraries and interfaces we need
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (lib '("/usr/local/lib/libtag_c.so"))
	(format t "Loading shared library: ~a~%" lib)
    (ccl:open-shared-library lib))
  (dolist (f '("libtag"))
	(format t "Loading interface directory: ~a~%" f)
	(ccl:use-interface-dir f)))

(defmacro ensure-writeable (af)
  "throw an error if someone tries to write to a read-only audio file"
  `(when (read-only ,af)
	 (error "Trying to write to read-only file")))

;;; build accessor methods for strings:
;;; 	lib:   one of tag|audioproperties. used to build "taglib_~a_field"
;;; 	field: the subfield in tag|audio properties
(defmacro define-cstring-accessors (lib field)
  "build read/writer methods for taglib funtions using strings"
  `(progn
	 (defmethod ,(alexandria:symbolicate 'af- field) ((me audio-file))
	   ;;(ensure-data-loaded ,lib me)
	   (%get-cstring (external-call ,(string-downcase (format nil "taglib_~a_~a" lib field)) :address (,lib me) :address)))
	 (defmethod (setf ,(alexandria:symbolicate 'af- field)) (,field (me audio-file))
	   ;;(ensure-data-loaded ,lib me)
	   (ensure-writeable me)
	   (with-cstrs ((cs ,field))
		 (setf (modified me) t)
		 (external-call ,(string-downcase (format nil "taglib_~a_set_~a" lib field)) :address (,lib me) :address cs :void)))))

;;; build accessor methods for longs:
;;; 	lib:   one of tag|audioproperties. used to build "taglib_~a_field"
;;; 	field: the subfield in tag|audio properties
(defmacro define-long-accessors (lib field)
  "build read/writer methods for taglib funtions using longs"
  `(progn
	 (defmethod ,(alexandria:symbolicate 'af- field) ((me audio-file))
	   ;;(ensure-data-loaded ,lib me)
	   (external-call ,(string-downcase (format nil "taglib_~a_~a" lib field)) :address (,lib me) :unsigned-long))
	 (defmethod (setf ,(alexandria:symbolicate 'af- field)) (,field (me audio-file))
	   ;;(ensure-data-loaded ,lib me)
	   (ensure-writeable me)
	   (setf (modified me) t)
	   (external-call ,(string-downcase (format nil "taglib_~a_set_~a" lib field)) :address (,lib me) :unsigned-long ,field :void))))

#+IN-DEVELOPMENT (progn

;;; taglib uses enums for mime types; map these to constants
;;; NB: highly fragile, since not autogenerated from taglib
(defconstant +taglib_file_mpeg+      0)
(defconstant +taglib_file_oggvorbis+ 1)
(defconstant +taglib_file_flac+      2)
(defconstant +taglib_file_mpc+       3)
(defconstant +taglib_file_oggflac+   4)
(defconstant +taglib_file_wavpack+   5)
(defconstant +taglib_file_speex+     6)
(defconstant +taglib_file_trueaudio+ 7)
(defconstant +taglib_file_mp4+       8)
(defconstant +taglib_file_asf+       9)

(defun map-mime-string-to-taglib-type (ms)
  "map a mime string returned from magic to a taglib enum")

;;; Seems more trouble than it is worth, honestly..
)

;;; some error conditions
(define-condition bad-path () 
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream) 
			 (format stream "Bad path at location <~a> with object <~a>: message<~a>"
					 (location condition) (object condition) (message condition)))))

(define-condition bad-file () 
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream) 
			 (format stream "Bad file at location <~a> with object <~a>: message<~a>"
					 (location condition) (object condition) (message condition)))))


(define-condition bad-tag () 
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream) 
			 (format stream "Bad tagdata at location <~a> with object <~a>: message<~a>"
					 (location condition) (object condition) (message condition)))))

(define-condition bad-audioproperties () 
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream) 
			 (format stream "Bad audioproperties at location <~a> with object <~a>: message<~a>"
					 (location condition) (object condition) (message condition)))))

;;; now, the class that represents an audio file
(defclass audio-file ()
  ((af-path :accessor af-path :initform nil :initarg :af-path)
   (af-type :accessor af-type :initform nil)
   (file :accessor file :initform nil)
   (tag :initform nil)
   (audioproperties :initform nil)
   (modified :accessor modified :initform nil)
   (read-only :accessor read-only :initarg :read-only :initform t)))

;;; paths are *hard* in Lisp... make sure we get a non-escaped pathname to pass to FFI
;;; NB: this still doesn't work all the time correctly...
(defun clean-path (p)
  (cl-ppcre:regex-replace-all "\\" (namestring p) ""))

;;; initializer for ltag:audio-file
(defmethod initialize-instance :after ((me audio-file) &key)
  (with-slots (af-path af-type file tag audioproperties) me

	;; make sure we have a clean name string for FFI calls (to libtag)
	(setf af-path (clean-path af-path))
	(setf af-type (magic:get-mime-string af-path))
	(let ((tru-name (osicat:file-exists-p af-path)))
	  (when (not tru-name)
		(error 'bad-path :location "class audio-file initializer"
						 :object af-path 
						 :message (format nil "class audio-file initializer: no such file ~a" af-path))))
	(with-cstrs ((p af-path))
	  (setf file (#_taglib_file_new p)))
	(when (%null-ptr-p file)
	  ;; make sure we don't try to access later by setting to nil. also, inform caller via error
	  (setf file nil)
	  (error 'bad-file :location "class audio-file initializer"
					   :object af-path 
					   :message (format nil "class audio-file initializer: could not open the file ~a" af-path)))))

;;; convenience function to open an audio file
(defun open-audio-file (path &key (read-only t))
  (make-instance 'audio-file :af-path path :read-only read-only))

;;; save audio file (if it is modified)
(defmethod save-audio-file ((me audio-file))
  "save a modified audiofile"
  (with-slots (file modified) me
	(if (not modified)
		(warn "will not save an unmodified file")
		(progn
		  (#_taglib_file_save file)
		  (setf modified nil)))))
	  
;;; close an audio file, releasing memory allocated by the taglib library
(defmethod close-audio-file ((me audio-file))
  (with-slots (af-path af-type file tag audioproperties modified read-only) me
	(when (and file (not (%null-ptr-p file)))
	  (when modified
		(warn "at some point, I will add code to auto-write modified audio-files?"))
	  (#_taglib_tag_free_strings)
	  (#_taglib_file_free file))
	(setf af-path nil
		  af-type nil
		  file nil
		  tag nil
		  audioproperties nil
		  modified nil
		  read-only t)))

;;; convenience macro for opening, operating, and ensuring close of an audio file
;;; "args" is any valid open-audio-file parameter
(defmacro with-open-audio-file ((var filename . args) &body body)
  (alexandria:with-gensyms (stream)
	`(let (,stream)
	   (unwind-protect
			(let ((,var (setq ,stream (open-audio-file ,filename ,@args))))
			  (progn
				,@body))
		 (when ,stream (close-audio-file ,stream))))))
  
(defmethod print-object ((me audio-file) stream)
  (with-slots (af-path af-type file tag audioproperties modified read-only) me
	(format stream "audio-file: af-path = ~a~%~taf-type = ~a~%~tfile = ~a~%~ttag = ~a~%~taudioproperties = ~a~%~tmodified = ~a~%~tread-only = ~a~%"
			af-path af-type file tag audioproperties modified read-only)))

;;; locate the tag data for the audio file.  this is used for things like title, track, etc
(defmethod tag ((me audio-file))
  "gets the pointer to the tag information in an audio file.  gets set on first access, so no need for user to call directly"
  (with-slots (file tag af-path) me
	(when (not tag)
	  (setf tag (#_taglib_file_tag file))
	  (when (%null-ptr-p tag)
		;; make sure we don't try to access later by setting to nil. also, inform caller via error
		(setf tag nil)
		(error 'bad-tag :location "method tagdata"
						:object af-path 
						:message (format nil "method tagdata: could not read the tag in file ~a" af-path))))
	tag))

;;; the following functions operate on tag data in an audiofile
(define-cstring-accessors tag title)
(define-cstring-accessors tag artist)
(define-cstring-accessors tag album)
(define-cstring-accessors tag comment)
(define-cstring-accessors tag genre)
(define-long-accessors tag year)
(define-long-accessors tag track)

;;; get the audio properties of the audio file.  This is needed for things like bitrate, length, etc
(defmethod audioproperties ((me audio-file))
  "gets the pointer to the audioproperties information in an audio file.  gets set on first access, so no need for user to call directly"
  (with-slots (af-path file audioproperties) me
	(when (not audioproperties)
	  (setf audioproperties (#_taglib_file_audioproperties file))
	  (when (%null-ptr-p audioproperties)
		;; make sure we don't try to access later by setting to nil. also, inform caller via error
		(setf audioproperties nil)
		(error 'bad-audioproperties :location "method audioproperties"
									 :object af-path
									 :message (format nil "method audioproperties: could not read audioproperties in file ~a" af-path))))
	audioproperties))

;;; the following functions operate on audioproperties data in an audiofile
(define-long-accessors audioproperties length)
(define-long-accessors audioproperties bitrate)
(define-long-accessors audioproperties samplerate)
(define-long-accessors audioproperties channels)
