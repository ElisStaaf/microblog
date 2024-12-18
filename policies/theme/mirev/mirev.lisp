;;;; mirev.lisp

(defpackage #:microblog.theme.mirev
  (:use #:cl #:iter #:microblog.policy.theme)
  (:export #:microblog-mirev-theme
           #:theme-templates-package))

(in-package #:microblog.theme.mirev)

(defclass microblog-mirev-theme ()
  ((templates-package :initarg :templates-package
                      :initform '#:microblog.theme.mirev.tmpl
                      :reader theme-templates-package)))

(microblog:register-theme-static-dir
 "mirev"
 (merge-pathnames "static/" (asdf:component-pathname  (asdf:find-system '#:microblog-theme-mirev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme-render-tagged-data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-mirev-method (method (&rest args) &body body)
  (alexandria:with-unique-names (tmplname tmplargs theme)
    `(defmethod ,method ((,theme microblog-mirev-theme)  ,@args)
       (macrolet ((render-template (,tmplname &body ,tmplargs)
                    `(closure-template:ttable-call-template
                      (closure-template:package-ttable (theme-templates-package ,',theme))
                      (string ',,tmplname)
                      (list* :blog-name microblog:*blog-name*
                             ,@,tmplargs))))
         ,@body))))

(defun archive-for-year-link (year)
  (list :title year
        :href (restas:genurl 'microblog.public::archive-for-year
                             :year year)))

(defun archive-for-month-link (year month)
  (list :title (svref local-time:+month-names+ month)
        :href (restas:genurl 'microblog.public::archive-for-month
                             :year year
                             :month (format nil "~2,'0D" month))))

(defun archive-for-day-link (year month day)
  (list :title day
        :href (restas:genurl 'microblog.public::archive-for-day
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
          :href (restas:genurl 'microblog.public::one-post
                               :year year
                               :month (format nil "~2,'0D" month)
                               :day (format nil "~2,'0D" day)
                               :urlname (gethash "urlname" post))
          :content (gethash "content" post)
          :markup (gethash "markup" post)
          :all-tags-href (restas:genurl 'microblog.public::all-tags)
          :tags (iter (for tag in (gethash "tags" post))
                      (collect
                          (list :name tag
                                :href (restas:genurl 'microblog.public::posts-with-tag :tag tag))))
          :published (list :year (archive-for-year-link year)
                           :month (archive-for-month-link year month)
                           :day (archive-for-day-link year month day)))))


(define-mirev-method theme-list-recent-posts (posts navigation)
  (render-template show-all-blog-post
    (list :posts (mapcar 'prepare-post-data posts)
          :disqus (list :enabled microblog:*disqus-enabled*
                        :shortname microblog:*disqus-shortname*)
          :navigation navigation)))

(define-mirev-method theme-archive-for-year (year months)
  (render-template archive-for-year
    (list :year year
          :months (iter (for month in months)
                        (collect (archive-for-month-link year month))))))

(define-mirev-method theme-archive-for-month (year month posts)
  (render-template archive-for-month
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (svref local-time:+month-names+ month))))

(define-mirev-method theme-archive-for-day (year month day posts)
  (render-template archive-for-day
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (svref local-time:+month-names+ month)
          :day day)))

(define-mirev-method theme-one-post (post)
  (let ((id (gethash "_id" post)))
    (render-template show-one-post
      (list* :disqus (list :shortname microblog:*disqus-shortname*
                           :developer-mode microblog:*disqus-developer-mode*
                           :enabled microblog:*disqus-enabled*
                           :identifier id
                           :permalink (restas:genurl* 'microblog.public::post-permalink :id id))
             (prepare-post-data post)))))


;;;; Tags

(define-mirev-method theme-all-tags (tags)
  (render-template tags-page
    (list :tags
          (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
                (collect (list :href (restas:genurl 'microblog.public::posts-with-tag
                                                    :tag tag)
                               :name tag))))))

(define-mirev-method theme-posts-with-tag (tag posts navigation)
  (render-template post-with-tag-page
    (list :tag tag
          :atom-feed-href (restas:genurl 'microblog.public::posts-with-tag-feed :tag tag)
          :navigation navigation
          :posts (mapcar 'prepare-post-data posts))))

;;;; Admin

(defun render-published (published)
  (format nil
          "~A.~A.~A"
         (local-time:timestamp-year published)
         (local-time:timestamp-month published)
         (local-time:timestamp-day published)))

(define-mirev-method theme-admin-posts (posts navigation)
  (render-template admin-post-page
    (list :posts (iter (for post in posts)
                       (collect (list :id (gethash "_id" post)
                                      :title (gethash "title" post)
                                      :href (restas:genurl 'microblog.admin::edit-post :id (gethash "_id" post))
                                      :published (render-published (gethash "published" post)))))
          :navigation navigation
          :create-post-href (restas:genurl 'microblog.admin::create-post))))

(define-mirev-method theme-admin-edit-post (&key title markup tags preview)
  (render-template admin-edit-post-page
    (list :post (list :title title
                      :markup markup
                      :tags (iter (for tag in tags)
                                  (collect (list :name tag))))
          :preview preview)))
