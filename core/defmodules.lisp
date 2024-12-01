;;;; defmodule.lisp

(restas:define-module #:ublog
  (:use #:cl #:iter)
  (:export #:*disqus-enabled*
           #:*disqus-shortname*
           #:*disqus-developer-mode*

           #:*posts-on-page*
           #:*blog-name*
           #:*blog-author*

           #:register-theme-static-dir

           #:parse-skip-param
           #:navigation
           #:title-to-urlname

           #:@admin))

(restas:define-policy #:datastore
  (:interface-package #:ublog.policy.datastore)
  (:interface-method-template "DATASTORE-~A")
  (:internal-package #:ublog.internal.datastore)
  (:internal-function-template "DS.~A")
  
  (define-method count-posts (&optional tag)
    "Return a count of the posts that are published")
  
  (define-method list-recent-posts (skip limit &key tag fields)
    "Retrieve the recent posts.")
  
  (define-method find-single-post (year month day urlname)
    "Retrieve a single post, based on date and post urlname")

  (define-method get-single-post (id &key fields)
    "Retrieve a single post, based  on post ID")

  (define-method list-archive-posts (min max &optional fields)
    "Retrieve archive posts")

  (define-method all-tags ()
    "Retrieve an array of tags")

  (define-method insert-post (title tags content &key markup published updated)
    "Insert post in the datastore and return the post ID of the created post")

  (define-method update-post (id title tags content &key markup)
    "Update post in the datastore")

  (define-method set-admin (name password)
    "Set administrator name and password")

  (define-method check-admin (name password)
    "Check for administrator rights"))

(restas:define-policy #:markup
  (:interface-package #:ublog.policy.markup)
  (:interface-method-template "MARKUP-~A")
  (:internal-package #:ublog.internal.markup)
  (:internal-function-template "MARKUP.~A")

  (define-method render-content (content)
    "Generate HTML from markup"))

(restas:define-policy #:theme
  (:interface-package #:ublog.policy.theme)
  (:interface-method-template "THEME-~A")
  (:internal-package #:ublog.internal.theme)
  (:internal-function-template "RENDER.~A")

  (define-method list-recent-posts (posts navigation))
  
  (define-method archive-for-year (year months))
  (define-method archive-for-month (year month posts))
  (define-method archive-for-day (year month day posts))
  (define-method one-post (post))

  (define-method all-tags (tags))
  (define-method posts-with-tag (tag posts navigation))

  (define-method admin-posts (posts navigation))
  (define-method admin-edit-post (&key title markup tags preview)))


(restas:define-module #:ublog.public
  (:use #:cl #:iter #:ublog
        #:ublog.internal.datastore
        #:ublog.internal.theme
        #:ublog.internal.markup))

(restas:define-module #:ublog.admin
  (:use #:cl #:iter #:ublog
        #:ublog.internal.datastore
        #:ublog.internal.theme
        #:ublog.internal.markup)
  (:export #:*post-permalink-route*))

(restas:define-module #:ublog.static
  (:use #:cl #:iter #:ublog))
