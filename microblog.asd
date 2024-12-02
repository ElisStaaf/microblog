;;;; microblog.asd

(require :ql)

(ql:quickload "closure-template")
(ql:quickload "restas")
(ql:quickload "local-time")

(defsystem #:microblog
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas #:local-time)
  :pathname "core/"
  :serial t
  :components ((:file "defmodules")
               (:closure-template "feed")
               (:file "arblog")
               (:file "public")
               (:file "admin")
               (:file "static")))
