;;;; markdown.lisp

(defpackage #:microblog.markup.markdown
  (:use #:cl #:microblog.policy.markup)
  (:export #:microblog-markdown-markup))

(in-package #:microblog.markup.markdown)

(defclass microblog-markdown-markup () ())

(defmethod markup-render-content ((markup microblog-markdown-markup) content)
  (with-output-to-string (str)
    (cl-markdown:markdown 
     (reduce (lambda (x y) (concatenate 'string x y))
	     (with-input-from-string (in content)
	       (loop for line = (read-line in nil :eof)
		  while (not (eq line :eof))
		  collect line)))
     :format :html
     :stream str)))

