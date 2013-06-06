;;; ltag.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(asdf:defsystem #:ltag
  :description "Simple interface to the libtag library"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:log5 #:osicat #:alexandria #:cl-ppcre)
  :components ((:file "examples" :depends-on ("ltag"))
			   (:file "magic")
               (:file "ltag" :depends-on ("magic"))))

