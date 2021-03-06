(in-package #:plan-rss)

;;; Options -------------------------------------------------------------------

(defvar *atom-link-self* NIL "Feed's <atom:link rel=self>")
(defvar *generator* NIL "Feed's <generator>")
(defvar *image* NIL "Feed's image <url>")
(defvar *max-items* 20 "Maximum number of <item>s to generate")
(defvar *link* NIL "Feed's <link>")
(defvar *pre-wrap* T "Wrap text in <pre> tags")
(defvar *title* NIL "Feed's <title>")
(defvar *version* NIL "Application version")

(opts:define-opts
  (:name :atom-link-self
   :description "use SELF as feed's atom:link with rel=self"
   :long "atom-link-self"
   :arg-parser #'identity
   :meta-var "SELF")
  (:name :disable-pre-tag-wrapping
   :description "disable wrapping text inside <pre> tags"
   :long "disable-pre-tag-wrapping")
  (:name :help
   :description "print the help text and exit"
   :short #\h
   :long "help")
  (:name :image
   :description "use IMAGE as feed's image <url>"
   :long "image"
   :arg-parser #'identity
   :meta-var "IMAGE")
  (:name :link
   :description "use LINK as feed's <link>"
   :required T
   :short #\l
   :long "link"
   :arg-parser #'identity
   :meta-var "LINK")
  (:name :max-items
   :description "maximum number of <item>s entries to generate (defaults to 20, and can be set to 0 to generate <item>s for all entries)"
   :long "max-items"
   :arg-parser #'parse-integer
   :meta-var "ENTRIES")
  (:name :title
   :description "use TITLE as feed's <title>"
   :required T
   :short #\t
   :long "title"
   :arg-parser #'identity
   :meta-var "TITLE")
  (:name :version
   :description "print the version and exit"
   :short #\v
   :long "version"))

(define-condition exit (error)
  ((code
     :initarg :code
     :initform 0
     :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Trying to exit with code: ~S"
                     (exit-code condition)))))

(defun parse-opts (argv)
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
                                                             (error 'exit :code 1))))))
            (opts:get-opts argv))
        (opts:unknown-option (condition)
          (format t "~a~%" condition)
          (error 'exit :code 1))
        (opts:missing-arg (condition)
          (format t "~a~%" condition)
          (error 'exit :code 1)))
    (if (getf options :help)
      (progn
        (opts:describe
          :prefix "Reads a .plan file from stdin, and prints to stdout a RSS feed with all the parsed entries"
          :args "[keywords]")
        (error 'exit)))
    (if (getf options :version)
      (progn
        (format T "~a~%" *version*)
        (error 'exit)))
    ; required arguments
    (setf *title* (getf options :title)
          *link* (getf options :link)
          *generator* (format NIL "plan-rss ~a" *version*))
    ; optional ones
    (if (getf options :image)
      (setf *image* (getf options :image)))
    (if (getf options :atom-link-self)
      (setf *atom-link-self* (getf options :atom-link-self)))
    (if (getf options :disable-pre-tag-wrapping)
      (setf *pre-wrap* NIL))
    (if (getf options :max-items)
      (setf *max-items* (getf options :max-items)))))

;;; Utils ---------------------------------------------------------------------

(defvar *day-header-scanner* (ppcre:create-scanner "^# [0-9]{4}-[0-9]{2}-[0-9]{2}"))

(defun day-header-p (s)
  (ppcre:scan *day-header-scanner* s))

(defun eof-p (s)
  (eq s :eof))

(defun day-header-date (s)
  (second (split-sequence:split-sequence #\Space s)))

(defun day-title-date-rfc2822 (s)
  (let* ((parts (split-sequence:split-sequence #\- s))
         (parts (mapcar #'parse-integer parts))
         (d (dt:make-date (first parts) (second parts) (third parts))))
    (dt:rfc-2822 (dt:day+ d 1))))

(defmacro hexadecimal-string (seq)
  `(format NIL "~{~(~2,'0x~)~}" (coerce ,seq 'list)))

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
  (format NIL "~{~A~^~%~}" (read-until-day-header)))

;;; Plan-day -----------------------------------------------------------------

(defstruct plan-day
  date content)

(defun plan-day-title (d)
  (day-header-date (plan-day-date d)))

(defun plan-day-pub-date (d)
  (day-title-date-rfc2822 (plan-day-title d)))

(defun plan-day-guid (d)
  (hexadecimal-string (md5:md5sum-string (plan-day-content d))))

(defun read-plan-day ()
  (unless (eof-p *last-line*)
    (make-plan-day :date *last-line* :content (read-channel-description))))

(defun read-most-recent-plan-days (max-items)
  (loop
    :with hq
    :for day = (read-plan-day)
    :while day
    :do (setf hq (merge 'list hq (list day) #'string< :key #'plan-day-title))
    :when (> (length hq) max-items) :do (pop hq)
    :finally (return (reverse hq))))

(defun plan-day-reader (max-items)
  (if (zerop max-items)
    #'read-plan-day
    (let ((items (read-most-recent-plan-days max-items)))
      (lambda ()
        (pop items)))))

;;; Main ----------------------------------------------------------------------

(defun process-input ()
  (xml-emitter:with-rss2 (*standard-output* :encoding "utf-8" :attrs '(("xmlns:atom" "http://www.w3.org/2005/Atom")
                                                                       ("version" "2.0")))
    (xml-emitter:with-rss-channel-header (*title* *link*
                                                  :description (read-channel-description)
                                                  :generator *generator*
                                                  :image *image*)
      (when *atom-link-self*
        (xml-emitter:empty-tag "atom:link" `(("href" ,*atom-link-self*)
                                             ("rel" "self")
                                             ("type" "application/rss+xml")))))
    (loop
      :with reader = (plan-day-reader *max-items*)
      :for day = (funcall reader)
      :while day
      :do (xml-emitter:with-rss-item ((plan-day-title day)
                                      :link *link*
                                      :pubDate (plan-day-pub-date day))
            (xml-emitter:simple-tag "guid" (plan-day-guid day)
                                    '(("isPermaLink" "false")))
            (xml-emitter:with-simple-tag ("description")
              (xml-emitter:xml-as-is "<![CDATA[")
              (when *pre-wrap*
                (xml-emitter:xml-as-is "<pre>"))
              (xml-emitter:xml-out (plan-day-content day))
              (when *pre-wrap*
                (xml-emitter:xml-as-is "</pre>"))
              (xml-emitter:xml-as-is "]]>"))))))

(defun toplevel()
  (handler-case (parse-opts (opts:argv))
    (exit (condition)
      (opts:exit (exit-code condition))))
  (process-input))

;;; REPL ----------------------------------------------------------------------

#+NIL
(setf
  *atom-link-self* "https://matteolandi.net/plan.xml"
  *max-items* 20
  *generator* (format NIL "plan-rss ~a" *version*)
  *image* "https://matteolandi.net/static/avatar-144.jpg"
  *link* "https://matteolandi.net/.plan"
  *pre-wrap* T
  *title* "Matteo Landi's blog"
  *version* "0.0.1")

#+NIL
(defun fake-input-stream ()
  (make-string-input-stream "This is my log ...

When I accomplish something, I write a * line that day.

Whenever a bug / missing feature / idea is mentioned during the day and I don't fix it, I make a note of it and mark it with ?.  Some things get noted many times before they get fixed.

Occasionally I go back through the old notes and mark with a + the things I have since fixed, and with a ~ the things I have since lost interest in.

--- Matteo Landi

# 2019-11-01
* xml-emitter: Add support for guid isPermaLink=false (https://github.com/VitoVan/xml-emitter/pull/3)
* xml-emitter: Add support for atom:link with rel=\"self\" (https://github.com/VitoVan/xml-emitter/pull/4)

# 2019-10-30
Finally A/I came back online, and I was finally able to create a request for a mailing list (to use it with the other college friends).  Anyway, the request has been created, so hopefully over the following days we will hear back from them...stay tuned!

"))

#+NIL
(let ((*standard-input* (fake-input-stream))) (process-input))
