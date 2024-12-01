;;;; markdown.lisp

(defpackage #:ublog.markup.markdown
  (:use #:cl #:ublog.policy.markup)
  (:export #:ublog-markdown-markup))

(in-package #:ublog.markup.markdown)

(defclass ublog-markdown-markup () ())

(defmethod markup-render-content ((markup ublog-markdown-markup) content)
  (with-output-to-string (str)
    (cl-markdown:markdown 
     (reduce (lambda (x y) (concatenate 'string x y))
	     (with-input-from-string (in content)
	       (loop for line = (read-line in nil :eof)
		  while (not (eq line :eof))
		  collect line)))
     :format :html
     :stream str)))

