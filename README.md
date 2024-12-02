# MicroBlog
MicroBlog is a miniscule blogging engine which allows you to create your very own
blog in CLisp (Common Lisp)! It has been made to be as asthetically pleasing as
possible while still being easy to use (if you know CLisp, no way to avoid that).
MicroBlog was mostly written for me to learn CLisp, but it's actually a pretty
useful tool if you want to write a blog.

## Requirements
* A CLisp compiler (SBCL recommended)
* ASDF
* [Others](/microblog.asd)

## Install
```sh
git clone https://github.com/ElisStaaf/microblog
./install # Assuming that your ASDF directory is "$HOME/common-lisp"
```

## Examples
```lisp
(require :asdf)

(asdf:operate 'asdf:load-op '#:microblog)
(asdf:operate 'asdf:load-op '#:microblog-systems)

(asdf:operate 'asdf:load-op '#:microblog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:microblog-markup-rst)

(asdf:operate 'asdf:load-op '#:microblog-theme-mirev)

(restas:define-module #:myblog
  (:use #:cl))

(in-package #:myblog)

(restas:mount-module -microblog- (#:microblog)
  (microblog:*blog-name* "My blog")
  (microblog:*posts-on-page* 10)
  
  (microblog.internal.datastore:*datastore* (make-instance 'microblog.datastore.mongodb:microblog-mongo-datastore))
  (microblog.internal.markup:*markup* (make-instance 'microblog.markup.rst:microblog-rst-markup))
  (microblog.internal.theme:*theme* (make-instance 'microblog.theme.mirev:microblog-mirev-theme))
  
  (microblog:*disqus-enabled* nil))

(restas:start '#:myblog :port 8080)
```
For more, see the [examples](/examples) folder.
