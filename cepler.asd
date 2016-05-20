;;;; cepler.asd

(asdf:defsystem #:cepler
  :description "Cepler"
  :author "<github@setq.net>"
  :license "GPL V3"
  #+asdf-unicode :encoding
  #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl
               #:cepl.sdl2
               #:dendrite
               #:skitter
               #:cepl.skitter.sdl2
               #:livesupport
               #:cepl.devil
               #:split-sequence
               #:cl-freetype2
               #:bordeaux-threads
               #:cl-openal
               #:cl-alut
               #:temporal-functions)
  :components ((:file "package")

               (:file "camera" :depends-on ("package"))
               (:file "sky" :depends-on ("package"))

               (:file "vsop87")
               (:file "planets" :depends-on ("vsop87" "camera"))

               (:file "ft")
               (:file "console" :depends-on ("ft" "planets"))

               (:file "textures" :depends-on ("package"))
               (:file "fps" :depends-on ("package"))
               (:file "sound" :depends-on ("package"))
               (:file "game" :depends-on ("fps" "sky" "textures" "sound" "console"))))
