(in-package #:plan-rss)

(defvar *version* NIL "Application version")

(opts:define-opts
  (:name :help
         :description "print the help text and exit"
         :short #\h
         :long "help")
  (:name :version
         :description "print the version and exit"
         :short #\v
         :long "version")
  (:name :debug
         :description "parse the RC file and exit"
         :short #\d
         :long "debug"))

(defun parse-opts ()
  (multiple-value-bind (options)
    (handler-case
        (opts:get-opts)
      (opts:unknown-option (condition)
        (format t "~s option is unknown!~%" (opts:option condition))
        (opts:exit 1)))
    (if (getf options :help)
      (progn
        (opts:describe
          :prefix "Guess commands to run from stdin, and print them to stdout."
          :args "[keywords]")
        (opts:exit)))
    (if (getf options :version)
      (progn
        (format T "~a~%" *version*)
        (opts:exit)))))

(defun guess (line &optional (guessers (reverse *guessers*)))
  (loop
    :for fn :in guessers
    :for command = (funcall fn line)
    :when (stringp command) :collect command
    :when (consp command) :append command))

(defun process-input ()
  (loop
    :with seen = (make-hash-table :test 'equal)
    :for line = (read-line NIL NIL :eof)
    :until (eq line :eof)
    :do (loop
          :for command :in (guess line)
          :when (and command (not (gethash command seen)))
          :do (progn
                (setf (gethash command seen) T)
                (format T "~a~%" command)))))

(defun toplevel ()
  (parse-opts)
  (process-input))

(in-package :xml-emitter)
(with-rss2 (*standard-output* :encoding "utf-8")
  (rss-channel-header "Peter's Blog" "http://peter.blogspot.com/"
          :description "A place where I sometimes post stuff"
          :image "myhead.jpg")
  (rss-item "Breaking news!"
      :link "http://google.com/"
      :description "The biggest problem with the DO-ODD macro above is that it puts BODY
into LOOP. Code from the user of the macro should never be run in the
environment established by the LOOP macro. LOOP does a number of
things behind your back, and it's hard to disable them. For example,
what happens here?>"
      :category "Lisp"
      :pubDate "Sun, 29 Sep 2002 19:59:01 GMT")
  (rss-item "RSS emitter created"
      :description "An RSS emitter has been released! Hahahahaha!"
      :link "http://gmail.google.com/"))
