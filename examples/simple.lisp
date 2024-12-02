;;;; simple.lisp

(require :asdf)

(asdf:operate 'asdf:load-op '#:microblog)
(asdf:operate 'asdf:load-op '#:microblog-systems)

(asdf:operate 'asdf:load-op '#:microblog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:microblog-markup-rst)
;; (asdf:operate 'asdf:load-op '#:microblog-markup-markdown)

(asdf:operate 'asdf:load-op '#:microblog-theme-mirev)

(restas:define-module #:myblog
  (:use #:cl))

(in-package #:myblog)

(restas:mount-module -microblog- (#:microblog)
  (microblog:*blog-name* "My blog")
  (microblog:*posts-on-page* 10)
  
  (microblog.internal.datastore:*datastore* (make-instance '#:microblog.datastore.mongodb:microblog-mongo-datastore))
  (microblog.internal.markup:*markup* (make-instance '#:microblog.markup.rst:microblog-rst-markup))
  ;;(microblog.policy.markup:*markup* (make-instance '#:microblog.markup.markdown:microblog-markdown-markup))
  (microblog.internal.theme:*theme* (make-instance '#:microblog.theme.mirev:microblog-mirev-theme))
  
  (microblog:*disqus-enabled* nil))

(restas:start '#:myblog :port 8080)
