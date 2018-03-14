(defpackage clw-block-braking/src/game/field
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :get-field
           :init-field)
  (:import-from :clw-block-braking/src/game/parameter
                :get-param))
(in-package :clw-block-braking/src/game/field)

(defvar.ps+ *field* nil)

(defun.ps+ get-field () *field*)

(defun.ps+ init-field ()
  (let ((field (make-ecs-entity)))
    (add-ecs-component-list
     field
     (make-point-2d :x (get-param :field :x)
                    :y (get-param :field :y))
     (make-model-2d :model (make-solid-rect :width (get-param :field :width)
                                            :height (get-param :field :height))
                    :depth (get-param :field :depth)))
    (add-ecs-entity field)
    (setf *field* field)))

