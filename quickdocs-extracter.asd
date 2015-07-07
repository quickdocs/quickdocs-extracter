#|
  This file is a part of quickdocs-extracter project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Extracts symbol informations of libraries for Quickdocs API reference

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage quickdocs-extracter-asd
  (:use :cl :asdf))
(in-package :quickdocs-extracter-asd)

(defsystem quickdocs-extracter
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:quickdocs-parser)
  :components ((:module "src"
                :components
                ((:file "quickdocs-extracter" :depends-on ("parser" "data"))
                 (:file "parser")
                 (:file "data")
                 (:file "util"))))
  :description "Extracts symbol informations of libraries for Quickdocs API reference"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op quickdocs-extracter-test))))
