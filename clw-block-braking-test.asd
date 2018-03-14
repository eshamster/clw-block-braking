#|
  This file is a part of clw-block-braking project.
  Copyright (c) 2017 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage clw-block-braking-test-asd
  (:use :cl :asdf))
(in-package :clw-block-braking-test-asd)

(defsystem clw-block-braking-test
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:eshamster
               :prove
               :dexador)
  :components ((:module "t"
                :components
                ((:test-file "clw-block-braking"))))
  :description "Test system for clw-block-braking"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
