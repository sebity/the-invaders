;;;; the-invaders.asd

(asdf:defsystem #:the-invaders
  :description "Describe the-invaders here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-mixer)
  :serial t
  :components ((:file "package")
               (:file "the-invaders")))

