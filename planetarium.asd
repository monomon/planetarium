;;;; planetarium.asd

(asdf:defsystem #:planetarium
  :description "Solar system model"
  :author "Mois Moshev <mois@monomon.me>"
  :license  "GPL-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:cl-opengl #:cl-glu #:cl-jpeg #:3d-vectors #:3d-matrices #:alexandria)
  :components ((:file "package")
               (:file "planetarium")))
