;;;; demo.lisp

(asdf:operate 'asdf:load-op '#:microblog)
(asdf:operate 'asdf:load-op '#:microblog-systems)

(asdf:operate 'asdf:load-op '#:microblog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:microblog-markup-rst)
;; (asdf:operate 'asdf:load-op '#:microblog-markup-markdown)

(asdf:operate 'asdf:load-op '#:microblog-theme-mirev)

(restas:define-module #:my-multi-authors-blog
  (:use #:cl))

(in-package #:my-multi-authors-blog)

;;;; datastore

(defclass multi-authors-datastore (microblog.datastore.mongodb:arblog-mongo-datastore)
  ((author :initarg :author :reader blog-author))
  (:default-initargs
   :dbspec '(:name "multi-authors-blog")))

(defmethod microblog.datastore.mongodb:make-query ((datastore multi-authors-datastore) &rest args)
  (declare (ignore args))
  (let ((query (call-next-method)))
    (setf (gethash "author" query)
          (blog-author datastore))
    query))

(defun add-author (author password)
  (microblog.policy.datastore:datastore-set-admin
   (make-instance 'multi-authors-datastore :author author)
   author
   password))

(add-author "ivanov" "111")
(add-author "petrov" "222")

;;;; settings

(defun author-settings (author)
  (restas:make-context
   `((microblog:*blog-name* . ,author)
     (microblog.internal.datastore:*datastore* . ,(make-instance 'multi-authors-datastore :author author))
     (microblog.internal.markup:*markup* . ,(make-instance 'arblog.markup.rst:arblog-rst-markup))
     (microblog.internal.theme:*theme* . ,(make-instance 'arblog.theme.mirev:arblog-mirev-theme)))))

;;;; multi-authors-microblog-route

(defclass multi-authors-microblog-route (routes:proxy-route) ())

(defun @multi-authors (route)
  (make-instance 'multi-authors-microblog-route :target route))

(defmethod routes:route-template ((route multi-authors-microblog-route))
  (append (routes:parse-template ":author")
          (call-next-method)))

(defmethod restas:process-route :around ((route multi-authors-microblog-route) bindings)
  (let ((author (cdr (assoc :author bindings :test #'string=))))
    (restas:with-context (author-settings author)
      (call-next-method))))

(defmethod routes:route-check-conditions ((route multi-authors-microblog-route) bindings)
  (let ((author (cdr (assoc :author bindings :test #'string=))))
    (restas:with-context (author-settings author)
      (call-next-method))))
  

(defmethod restas:make-route-url ((route multi-authors-microblog-route) bindings)
  (restas:make-route-url (routes:route-template route)
                         (list* :author (blog-author microblog.internal.datastore:*datastore*)
                                bindings)))

;;;; mount modules

(restas:mount-module -public- (#:microblog.public)
  (:decorators '@multi-authors))

(restas:mount-module -admin- (#:microblog.admin)
  (:url "/admin/")
  (:decorators '@multi-authors 'microblog:@admin))

(restas:mount-module -static- (#:microblog.static)
  (:url "/static/"))

;;;; start

(restas:start '#:my-multi-authors-blog :port 8080)
