(in-package :cl-user)
(defpackage clw-block-braking-test
  (:use :cl
        :clw-block-braking
        :prove))
(in-package :clw-block-braking-test)

(plan 1)

(defvar *port* 21464)

;; Only test connection
(unwind-protect
     (progn
       (clw-block-braking:start :port *port*)
       (handler-case
           (ok (dex:get (format nil "http://localhost:~D" *port*)))
         (error (e)
           (fail (format nil "~A" e)))))
  (clw-block-braking:stop))

(finalize)
