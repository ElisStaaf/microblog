;;;; isimple.lisp

(defpackage #:ublog.theme.isimple
  (:use #:cl #:iter #:ublog.policy.theme)
  (:export #:ublog-isimple-theme
           #:theme-templates-package))

(in-package #:ublog.theme.isimple)

(defclass ublog-isimple-theme ()
  ((templates-package :initarg :templates-package
                      :initform '#:ublog.theme.isimple.tmpl
                      :reader theme-templates-package)))

(ublog:register-theme-static-dir
 "isimple"
 (merge-pathnames "static/" (asdf:component-pathname  (asdf:find-system '#:ublog-theme-isimple))))


(defun recent-posts-widget ()
  (iter (for item in (ublog.internal.datastore:ds.list-recent-posts 0 10 :fields '("title")))
        (collect
            (list :title (gethash "title" item)
                  :url (restas:genurl 'ublog.public::post-permalink :id (gethash "_id" item))))))

(defun tags-widget ()
  (let ((tags (ublog.internal.datastore:ds.all-tags)))
    (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
          (collect (list :href (restas:genurl 'ublog.public::posts-with-tag
                                              :tag tag)
                         :name tag)))))

(defmacro define-isimple-method (method (&rest args) &body body)
  (alexandria:with-unique-names (tmplname tmplargs theme)
    `(defmethod ,method ((,theme ublog-isimple-theme)  ,@args)
       (macrolet ((render-template (,tmplname &body ,tmplargs)
                    `(closure-template:ttable-call-template
                      (closure-template:package-ttable (theme-templates-package ,',theme))
                      (string ',,tmplname)
                      (let ((is-not-admin (not (string= (string (slot-value restas:*module* 'restas::package)) "ARBLOG.ADMIN"))))
                        (list* :index-url (restas:genurl (if is-not-admin 'ublog.public::entry 'arblog.admin::entry))
                               :blog-name ublog:*blog-name*
                               :blog-author ublog:*blog-author*
                               :recent-posts (if is-not-admin (recent-posts-widget))
                               :all-tags (if is-not-admin (tags-widget))
                               ,@,tmplargs)))))
         ,@body))))



(defun archive-for-year-link (year)
  (list :title year
        :href (restas:genurl 'ublog.public::archive-for-year
                             :year year)))

(defun archive-for-month-link (year month)
  (list :title (svref local-time:+month-names+ month)
        :href (restas:genurl 'ublog.public::archive-for-month
                             :year year
                             :month (format nil "~2,'0D" month))))

(defun archive-for-day-link (year month day)
  (list :title day
        :href (restas:genurl 'ublog.public::archive-for-day
                             :year year
                             :month (format nil "~2,'0D" month)
                             :day (format nil "~2,'0D" day))))

(defun prepare-post-data (post)
  (let* ((published (gethash "published" post))
         (year (local-time:timestamp-year published))
         (month (local-time:timestamp-month published))
         (day (local-time:timestamp-day published)))
    (list :id (gethash "_id" post)
          :title (gethash "title" post)
          :href (restas:genurl 'ublog.public::one-post
                               :year year
                               :month (format nil "~2,'0D" month)
                               :day (format nil "~2,'0D" day)
                               :urlname (gethash "urlname" post))
          :content (gethash "content" post)
          :markup (gethash "markup" post)
          :all-tags-href (restas:genurl 'ublog.public::all-tags)
          :tags (iter (for tag in (gethash "tags" post))
                      (collect
                          (list :name tag
                                :href (restas:genurl 'ublog.public::posts-with-tag :tag tag))))
          :published (local-time:format-rfc1123-timestring nil published))))


(define-isimple-method theme-list-recent-posts (posts navigation)
  (render-template show-all-blog-post
    (list :posts (mapcar 'prepare-post-data posts)
          :disqus (list :enabled ublog:*disqus-enabled*
                        :shortname ublog:*disqus-shortname*)
          :navigation navigation)))

(define-isimple-method theme-archive-for-year (year months)
  (render-template archive-for-year
    (list :year year
          :months (iter (for month in months)
                        (collect (archive-for-month-link year month))))))

(define-isimple-method theme-archive-for-month (year month posts)
  (render-template archive-for-month
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (svref local-time:+month-names+ month))))

(define-isimple-method theme-archive-for-day (year month day posts)
  (render-template archive-for-day
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (svref local-time:+month-names+ month)
          :day day)))

(define-isimple-method theme-one-post (post)
  (let ((id (gethash "_id" post)))
    (render-template show-one-post
      (list* :disqus (list :shortname ublog:*disqus-shortname*
                           :developer-mode ublog:*disqus-developer-mode*
                           :enabled ublog:*disqus-enabled*
                           :identifier id
                           :permalink (restas:genurl* 'ublog.public::post-permalink :id id))
             (prepare-post-data post)))))


;;;; Tags

(define-isimple-method theme-all-tags (tags)
  (render-template tags-page
    (list :tags
          (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
                (collect (list :href (restas:genurl 'ublog.public::posts-with-tag
                                                    :tag tag)
                               :name tag))))))

(define-isimple-method theme-posts-with-tag (tag posts navigation)
  (render-template post-with-tag-page
    (list :tag tag
          :atom-feed-href (restas:genurl 'ublog.public::posts-with-tag-feed :tag tag)
          :navigation navigation
          :posts (mapcar 'prepare-post-data posts))))

;;;; Admin

(defun render-published (published)
  (format nil
          "~A.~A.~A"
         (local-time:timestamp-year published)
         (local-time:timestamp-month published)
         (local-time:timestamp-day published)))

(define-isimple-method theme-admin-posts (posts navigation)
  (render-template admin-post-page
    (list :posts (iter (for post in posts)
                       (collect (list :id (gethash "_id" post)
                                      :title (gethash "title" post)
                                      :href (restas:genurl 'ublog.admin::edit-post :id (gethash "_id" post))
                                      :published (render-published (gethash "published" post)))))
          :navigation navigation
          :create-post-href (restas:genurl 'ublog.admin::create-post))))

(define-isimple-method theme-admin-edit-post (&key title markup tags preview)
  (render-template admin-edit-post-page
    (list :post (list :title title
                      :markup markup
                      :tags (iter (for tag in tags)
                                  (collect (list :name tag))))
          :preview preview)))

