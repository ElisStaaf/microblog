;;;; ublog.asd

(defsystem #:ublog
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas #:local-time)
  :pathname "core/"
  :serial t
  :components ((:file "defmodules")
               (:closure-template "feed")
               (:file "ublog")
               (:file "public")
               (:file "admin")
               (:file "static")))
