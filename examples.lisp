(in-package :cl-user)
(use-package :ltag)

;;; test calls...
(defmacro redirect-output (filename &rest body)
  "if a lot of output is expected, use this macro to redirect *standard-output* to a file"
  `(if ,filename 
	   (let ((*standard-output* (open ,filename :direction :output :if-does-not-exist :create :if-exists :overwrite)))
		 ,@body)
	   ,@body))
	   

(defun read-tag-file (fn)
  "read & print all simple tag/audio data from a file"
  (handler-case (with-open-audio-file (f fn)
				  (format t "File: ~a~%" (af-path f))
				  (format t "~tTitle: ~a~%~tArtist: ~a~%~tAlbum: ~a~%~tComment: ~a~%~tGenre: ~a~%~tYear: ~a~%~tTrack: ~a~%~tType: ~a~%"
						  (af-title f) (af-artist f) (af-album f) (af-comment f) (af-genre f) (af-year f) (af-track f) (af-type f))
				  (format t "~tLength: ~a~%~tBitrate: ~a~%~tSamplerate: ~a~%~tChannels: ~a~%"
										 (af-length f) (af-bitrate f) (af-samplerate f) (af-channels f)))
	(bad-file (c) (declare (ignore c))
	  (format t "File <~a> not recognized as an audio file~%" fn))))

(defun read-tags-from-dir (dir &key (output nil))
  "walk a diretory and read all audio files, printing all tag/audio data"
  (redirect-output output (osicat:walk-directory dir #'read-tag-file)))
