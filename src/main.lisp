(in-package #:plan-rss)

;;; Vendor --------------------------------------------------------------------


(defmacro with-rss-channel-header ((title link &key description
                                          (generator "xml-emitter")
                                          (language "en-us")
                                          image image-title image-link)
                                   &body body)
  `(progn
     (xml-emitter:emit-simple-tags :title ,title
                                   :link ,link
                                   :description ,description
                                   :generator ,generator
                                   :language ,language)
     (when ,image
       (xml-emitter:with-tag ("image")
         (xml-emitter:emit-simple-tags :title (or ,image-title ,title)
                                       :url ,image
                                       :link (or ,image-link ,link))))
     ,@body))

(defmacro with-rss-item ((title &key link description author category
                                comments guid pubDate source)
                         &body body)
  `(xml-emitter:with-tag ("item")
     (xml-emitter:emit-simple-tags :title ,title
                                   :link ,link
                                   :description ,description
                                   :author ,author
                                   :category ,category
                                   :comments ,comments
                                   :guid ,guid
                                   "pubDate" ,pubDate
                                   :source ,source)
     ,@body))

(defmacro with-rss2 ((stream &key (encoding "ISO-8859-1") (attrs '(("version" "2.0")))) &body body)
  `(xml-emitter:with-xml-output (,stream :encoding ,encoding)
     (xml-emitter:with-tag ("rss" ,attrs)
       (xml-emitter:with-tag ("channel")
         ,@body))))


;;; Options -------------------------------------------------------------------

(defvar *version* NIL "Application version")
(defvar *title* NIL "Feed's <title>")
(defvar *link* NIL "Feed's <link>")
(defvar *generator* NIL "Feed's <generator>")
(defvar *image* NIL "Feed's image <url>")
(defvar *atom-link-self* NIL "Feed's <atom:link rel=self>")

(opts:define-opts
  (:name :help
         :description "print the help text and exit"
         :short #\h
         :long "help")
  (:name :version
         :description "print the version and exit"
         :short #\v
         :long "version")
  (:name :title
         :description "use TITLE as feed's <title>"
         :required T
         :short #\t
         :long "title"
         :arg-parser #'identity
         :meta-var "TITLE")
  (:name :link
         :description "use LINK as feed's <link>"
         :required T
         :short #\l
         :long "link"
         :arg-parser #'identity
         :meta-var "LINK")
  (:name :image
         :description "use IMAGE as feed's image <url>"
         :short #\m
         :long "image"
         :arg-parser #'identity
         :meta-var "IMAGE")
  (:name :atom-link-self
         :description "use SELF as feed's atom:link with rel=self"
         :short #\s
         :long "atom-link-self"
         :arg-parser #'identity
         :meta-var "SELF"))

(defun parse-opts ()
  (multiple-value-bind (options)
      (handler-case
          (handler-bind ((opts:missing-required-option (lambda (condition)
                                                         (if (or (member "-h" argv :test #'equal)
                                                                 (member "--help" argv :test #'equal)
                                                                 (member "-v" argv :test #'equal)
                                                                 (member "--version" argv :test #'equal))
                                                           (invoke-restart 'opts:skip-option)
                                                           (progn
                                                             (format t "~a~%" condition)
                                                             (opts:exit 1))))))
            (opts:get-opts argv))
        (opts:unknown-option (condition)
          (format t "~a~%" condition)
          (opts:exit 1))
        (opts:missing-arg (condition)
          (format t "~a~%" condition)
          (opts:exit 1)))
    (if (getf options :help)
      (progn
        (opts:describe
          :prefix "Reads a .plan file from stdin, and prints to stdout a feed with all the parsed entries"
          :args "[keywords]")
        (opts:exit)))
    (if (getf options :version)
      (progn
        (format T "~a~%" *version*)
        (opts:exit)))
    ; required arguments
    (setf *title* (getf options :title)
          *link* (getf options :link)
          *generator* (format NIL "plan-rss ~a" *version*))
    ; optional ones
    (if (getf options :image)
      (setf *image* (getf options :image)))
    (if (getf options :atom-link-self)
      (setf *atom-link-self* (getf options :atom-link-self)))))

;;; Utils ---------------------------------------------------------------------

(defvar *day-header-scanner* (ppcre:create-scanner "^# [0-9]{4}-[0-9]{2}-[0-9]{2}"))

(defun day-header-p (s)
  (ppcre:scan *day-header-scanner* s))

(defun eof-p (s)
  (eq s :eof))

(defun day-header-date (s)
  (second (split-sequence:split-sequence #\Space s)))

(defun day-header-date-rfc2822 (s)
  (let* ((parts (split-sequence:split-sequence #\- s))
         (parts (mapcar #'parse-integer parts))
         (d (dt:make-date (first parts) (second parts) (third parts))))
    (dt:rfc-2822 (dt:day+ d 1))))

;;; Stream --------------------------------------------------------------------

(defvar *last-line* NIL "Last, read, line")

(defun read-next ()
  (setf *last-line* (read-line NIL NIL :eof)))

(defun read-until (stop-p)
  (loop
    :for line = (read-next)
    :until (funcall stop-p line)
    :collect line))

(defun read-until-day-header ()
  (read-until (lambda (s) (or (eof-p s)
                              (day-header-p s)))))

(defun read-channel-description ()
  (format NIL "~{~&~A~}" (read-until-day-header)))

(defstruct plan-day
  date content)

(defun read-plan-day ()
  (unless (eof-p *last-line*)
    (make-plan-day :date *last-line* :content (read-channel-description))))

;;; Main ----------------------------------------------------------------------

(defun process-input ()
  (with-rss2 (*standard-output* :encoding "utf-8" :attrs '(("xmlns:atom" "http://www.w3.org/2005/Atom")
                                                                       ("version" "2.0")))
    (with-rss-channel-header (*title* *link* :description (read-channel-description)
                                                         :generator *generator*
                                                         :image *image*)
      (when *atom-link-self*
        (xml-emitter:empty-tag "atom:link" `(("href" ,*atom-link-self*)
                                             ("rel" "self")
                                             ("type" "application/rss+xml")))))
    (loop
      :for day = (read-plan-day)
      :for date = (and day (day-header-date (plan-day-date day)))
      :while day
      :do (with-rss-item (date :link *link*
                                           :pubDate (day-header-date-rfc2822 date))
            (xml-emitter:simple-tag "guid" (format NIL "~a#~a" *link* date)
                                     '(("isPermaLink" "false")))
            (xml-emitter:with-simple-tag ("description")
              (xml-emitter:xml-as-is "<![CDATA[<pre>")
              (xml-emitter:xml-out (plan-day-content day))
              (xml-emitter:xml-as-is "</pre>]]>"))))))

(defun toplevel()
  (parse-opts)
  (process-input))

;;; REPL  ---------------------------------------------------------------------

#+NIL
(setf *version* "0.0.1"
      *title* "Matteo Landi's blog"
      *link* "https://matteolandi.net/.plan"
      *generator* (format NIL "plan-rss ~a" *version*)
      *image* "https://matteolandi.net/static/avatar-144.jpg"
      *atom-link-self* "https://matteolandi.net/plan.xml")

#+NIL
(defun fake-input-stream ()
  (open ".plan.example" :if-does-not-exist nil))
