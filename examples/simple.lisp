;;;; simple.lisp

(asdf:operate 'asdf:load-op '#:ublog)
(asdf:operate 'asdf:load-op '#:ublog-systems)

(asdf:operate 'asdf:load-op '#:ublog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:ublog-markup-rst)
;; (asdf:operate 'asdf:load-op '#:ublog-markup-markdown)

(asdf:operate 'asdf:load-op '#:ublog-theme-mirev)

(restas:define-module #:myblog
  (:use #:cl))

(in-package #:myblog)

(restas:mount-module -ublog- (#:ublog)
  (ublog:*blog-name* "My blog")
  (ublog:*posts-on-page* 10)
  
  (ublog.internal.datastore:*datastore* (make-instance 'ublog.datastore.mongodb:arblog-mongo-datastore))
  (ublog.internal.markup:*markup* (make-instance 'ublog.markup.rst:arblog-rst-markup))
  ;;(ublog.policy.markup:*markup* (make-instance 'ublog.markup.markdown:arblog-markdown-markup))
  (ublog.internal.theme:*theme* (make-instance 'ublog.theme.mirev:arblog-mirev-theme))
  
  (ublog:*disqus-enabled* nil))

(restas:start '#:myblog :port 8080)
