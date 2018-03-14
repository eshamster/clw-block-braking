#|
  This file is a part of clw-block-braking project.
  Copyright (c) 2018 eshamster (hamgoostar@gmail.com)
|#

#|
  A sample block braking written by Common Lisp as Web application

  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage clw-block-braking-asd
  (:use :cl :asdf))
(in-package :clw-block-braking-asd)

(defsystem clw-block-braking
  :version "0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game
               :ningle
               :cl-markup
               :clack
               :clw-block-braking/src/clw-block-braking)
  :description "A sample block braking written by Common Lisp as Web application"
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
  :in-order-to ((test-op (test-op clw-block-braking-test))))
