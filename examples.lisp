;;; test calls...
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package :cl-user)

(defmacro redirect-output (filename &rest body)
  "if a lot of output is expected, use this macro to redirect *standard-output* to a file"
  `(if ,filename 
	   (let ((*standard-output* (open ,filename :direction :output :if-does-not-exist :create :if-exists :overwrite)))
		 ,@body)
	   ,@body))
	   
(defun read-tag-file (fn)
  "read & print all simple tag/audio data from a file"
  (handler-case (ltag:with-open-audio-file (f fn)
				  (format t "got here~%")
				  (format t "File: ~a~%" (ltag:af-path f))
				  (format t "~tTitle: ~a~%~tArtist: ~a~%~tAlbum: ~a~%~tComment: ~a~%~tGenre: ~a~%~tYear: ~a~%~tTrack: ~a~%~tType: ~a~%"
						  (ltag:af-title f) (ltag:af-artist f) (ltag:af-album f) (ltag:af-comment f) (ltag:af-genre f)
						  (ltag:af-year f) (ltag:af-track f) (ltag:af-type f))
				  (format t "~tLength: ~a~%~tBitrate: ~a~%~tSamplerate: ~a~%~tChannels: ~a~%"
										 (ltag:af-length f) (ltag:af-bitrate f) (ltag:af-samplerate f) (ltag:af-channels f)))
	(ltag:bad-file (c)
	  (format t "Problem opening ~a: ~a~%" fn c))
	(condition (c)
	  (format t "Unknown problem with ~a: ~a~%" fn c))))

(defun read-tags-from-dir (dir &key (output nil))
  "walk a diretory and read all audio files, printing all tag/audio data"
  (let ((count 0))
	(redirect-output output (progn 
							  (osicat:walk-directory dir (lambda (file) (incf count) (read-tag-file file)))
							  (format t "Processed ~,d files~%" count)))))
	))
