(defpackage clw-block-braking/src/game/parameter
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :get-param))
(in-package :clw-block-braking/src/game/parameter)

(defvar.ps+ *params*
  (convert-to-layered-hash
   (:field (:x 190 :y 20 :width 420 :height 560 :depth 0))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))
