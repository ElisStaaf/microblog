;;;; ublog-policies.asd

(defsystem #:ublog-systems)

;;;; datastores

(defsystem #:ublog-datastore-mongodb
  :depends-on (#:ublog #:mongo-cl-driver.usocket #:ironclad)
  :pathname "policies/datastore/"
  :components ((:file "mongodb")))

;;;; markups

(defsystem #:ublog-markup-rst
  :depends-on (#:ublog #:docutils #:colorize #:cl-libxml2)
  :pathname "policies/markup/"
  :components ((:file "rst")))

(defsystem #:ublog-markup-markdown
  :depends-on (#:ublog #:cl-markdown)
  :pathname "policies/markup/"
  :components ((:file "markdown")))

;;;; themes

(defsystem #:ublog-theme-mirev
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:ublog)
  :pathname "policies/theme/mirev/"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "page")
                                     (:closure-template "entries")
                                     (:closure-template "tags")
                                     (:closure-template "archive")
                                     (:closure-template "admin")))
               (:file "mirev")))

(defsystem #:ublog-theme-isimple
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:ublog)
  :pathname "policies/theme/isimple/"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "isimple")))
               (:file "isimple")))
