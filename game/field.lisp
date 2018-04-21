(defpackage clw-block-braking/game/field
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :get-field
           :init-field
           :field-width
           :field-height)
  (:import-from :clw-block-braking/game/parameter
                :get-param))
(in-package :clw-block-braking/game/field)

(defvar.ps+ *field* nil)

(defun.ps+ get-field () *field*)

(defun.ps+ field-width (field)
  (get-entity-param field :width))

(defun.ps+ field-height (field)
  (get-entity-param field :height))

(defun.ps+ init-field ()
  (let ((field (make-ecs-entity))
        (width (get-param :field :width))
        (height (get-param :field :height)))
    (add-entity-tag field :field)
    (add-ecs-component-list
     field
     (make-point-2d :x (get-param :field :x)
                    :y (get-param :field :y))
     (make-model-2d :model (make-solid-rect :width width :height height)
                    :depth (get-param :field :depth))
     (init-entity-params :width width :height height))
    (add-ecs-entity field)
    (setf *field* field)))

