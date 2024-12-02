;;;; microblog-policies.asd

(defsystem #:microblog-systems)

;;;; datastores

(defsystem #:microblog-datastore-mongodb
  :depends-on (#:microblog #:mongo-cl-driver.usocket #:ironclad)
  :pathname "policies/datastore/"
  :components ((:file "mongodb")))

;;;; markups

(defsystem #:microblog-markup-rst
  :depends-on (#:microblog #:docutils #:colorize #:cl-libxml2)
  :pathname "policies/markup/"
  :components ((:file "rst")))

(defsystem #:microblog-markup-markdown
  :depends-on (#:microblog #:cl-markdown)
  :pathname "policies/markup/"
  :components ((:file "markdown")))

;;;; themes

(defsystem #:microblog-theme-mirev
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:microblog)
  :pathname "policies/theme/mirev/"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "page")
                                     (:closure-template "entries")
                                     (:closure-template "tags")
                                     (:closure-template "archive")
                                     (:closure-template "admin")))
               (:file "mirev")))

(defsystem #:microblog-theme-isimple
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:microblog)
  :pathname "policies/theme/isimple/"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "isimple")))
               (:file "isimple")))
