#|
  This file is a part of quickdocs-extracter project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage quickdocs-extracter-test-asd
  (:use :cl :asdf))
(in-package :quickdocs-extracter-test-asd)

(defsystem quickdocs-extracter-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:quickdocs-extracter
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "quickdocs-extracter"))))
  :description "Test system for quickdocs-extracter"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
