;;;; ltag.asd

(asdf:defsystem #:ltag
  :description "Simple interface to the libtag library"
  :author "Mark VandenBrink"
  :license "LLGPL"
  :depends-on (#:log5 #:osicat #:alexandria #:cl-ppcre)
  :components ((:file "examples" :depends-on ("ltag"))
			   (:file "magic")
               (:file "ltag" :depends-on ("magic"))))

