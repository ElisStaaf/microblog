;;;; demo.lisp

(asdf:operate 'asdf:load-op '#:ublog)
(asdf:operate 'asdf:load-op '#:ublog-systems)

(asdf:operate 'asdf:load-op '#:ublog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:ublog-markup-rst)
;; (asdf:operate 'asdf:load-op '#:ublog-markup-markdown)

(asdf:operate 'asdf:load-op '#:ublog-theme-mirev)

(restas:define-module #:my-multi-authors-blog
  (:use #:cl))

(in-package #:my-multi-authors-blog)

;;;; datastore

(defclass multi-authors-datastore (ublog.datastore.mongodb:arblog-mongo-datastore)
  ((author :initarg :author :reader blog-author))
  (:default-initargs
   :dbspec '(:name "multi-authors-blog")))

(defmethod ublog.datastore.mongodb:make-query ((datastore multi-authors-datastore) &rest args)
  (declare (ignore args))
  (let ((query (call-next-method)))
    (setf (gethash "author" query)
          (blog-author datastore))
    query))

(defun add-author (author password)
  (ublog.policy.datastore:datastore-set-admin
   (make-instance 'multi-authors-datastore :author author)
   author
   password))

(add-author "ivanov" "111")
(add-author "petrov" "222")

;;;; settings

(defun author-settings (author)
  (restas:make-context
   `((ublog:*blog-name* . ,author)
     (ublog.internal.datastore:*datastore* . ,(make-instance 'multi-authors-datastore :author author))
     (ublog.internal.markup:*markup* . ,(make-instance 'arblog.markup.rst:arblog-rst-markup))
     (ublog.internal.theme:*theme* . ,(make-instance 'arblog.theme.mirev:arblog-mirev-theme)))))

;;;; multi-authors-ublog-route

(defclass multi-authors-ublog-route (routes:proxy-route) ())

(defun @multi-authors (route)
  (make-instance 'multi-authors-ublog-route :target route))

(defmethod routes:route-template ((route multi-authors-ublog-route))
  (append (routes:parse-template ":author")
          (call-next-method)))

(defmethod restas:process-route :around ((route multi-authors-ublog-route) bindings)
  (let ((author (cdr (assoc :author bindings :test #'string=))))
    (restas:with-context (author-settings author)
      (call-next-method))))

(defmethod routes:route-check-conditions ((route multi-authors-ublog-route) bindings)
  (let ((author (cdr (assoc :author bindings :test #'string=))))
    (restas:with-context (author-settings author)
      (call-next-method))))
  

(defmethod restas:make-route-url ((route multi-authors-ublog-route) bindings)
  (restas:make-route-url (routes:route-template route)
                         (list* :author (blog-author ublog.internal.datastore:*datastore*)
                                bindings)))

;;;; mount modules

(restas:mount-module -public- (#:ublog.public)
  (:decorators '@multi-authors))

(restas:mount-module -admin- (#:ublog.admin)
  (:url "/admin/")
  (:decorators '@multi-authors 'ublog:@admin))

(restas:mount-module -static- (#:ublog.static)
  (:url "/static/"))

;;;; start

(restas:start '#:my-multi-authors-blog :port 8080)
