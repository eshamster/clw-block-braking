(defpackage clw-block-braking/src/game/parameter
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :get-param))
(in-package :clw-block-braking/src/game/parameter)

(eval-when (:execute :compile-toplevel :load-toplevel)
  ;; width : height = 3 : 4
  (defvar.ps+ field-height 560)
  (defvar.ps+ field-width (/ (* field-height 3.0) 4))

  (defun.ps+ calc-absolute-length (relative-length base-length)
    (* relative-length base-length 0.001))

  "#Ex1. '#lx500' represents a half length of the field width."
  "#Ex2. '#ly500' represents a half length of the field height."
  (set-dispatch-macro-character
   #\# #\l
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (case (peek-char nil stream)
         (#\x (read-char stream)
              `(calc-absolute-length ,(read stream) field-width))
         (#\y (read-char stream)
              `(calc-absolute-length ,(read stream) field-height))
         (t (error "Not recognized character after #l"))))))

(defvar.ps+ *params*
  (convert-to-layered-hash
   (:field (:x 190 :y 20 :width field-width :height field-height :depth 0)
    :ball (:r #lx10 :color #xffaaaa
           :speed (:base (:min #ly5 :max #ly10))
           :angle (:min (/ PI 7) :max-accele (/ PI 8))
           :dist-from-paddle #ly10)
    :paddle (:width (:min #lx130 :max #lx55)
             :height #ly10 :depth 8
             :base-line-height #ly80 :lane-space #ly30 :lane-count 4))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))
