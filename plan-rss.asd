(asdf:defsystem #:plan-rss
  :description "Matteo's .plan to RSS feed converter"

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (#:cl-ppcre #:split-sequence #:unix-opts #:xml-emitter)

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "plan-rss"
  :entry-point "plan-rss:toplevel"

  :serial t
  :components ((:file "package")
               (:module "src" :serial t
                        :components
                        ((:file "main")))))
