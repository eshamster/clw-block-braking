(defpackage clw-block-braking/src/game/parameter
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :get-param))
(in-package :clw-block-braking/src/game/parameter)

(defvar.ps+ *params*
  (convert-to-layered-hash
   (:field (:x 190 :y 20 :width 420 :height 560 :depth 0)
    :ball (:r 5 :color #xffaaaa
           :speed (:base (:min 3 :max 6))
           :angle (:min (/ PI 7) :max-accele (/ PI 8))
           :dist-from-paddle 5)
    :paddle (:width 40 :height 6 :depth 8
             :base-line-height 56  :lane-space 12 :lane-count 4))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))
