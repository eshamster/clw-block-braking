(defpackage clw-block-braking/game/wall
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-wall)
  (:import-from :clw-block-braking/game/field
                :field-width
                :field-height))
(in-package :clw-block-braking/game/wall)

(defun.ps+ init-wall (field)
  (let ((large-len (* 2 (field-height field)))
        (field-width (field-width field))
        (field-height (field-height field)))
    (dolist (param (list (list (* -1 large-len) 0 large-len field-height) ; left
                         (list field-width 0 large-len field-height) ; right
                         (list 0 field-height field-width large-len) ; top
                         ))
      (add-ecs-entity
       (make-a-wall (nth 0 param)
                    (nth 1 param)
                    (nth 2 param)
                    (nth 3 param))
       field))))

(defun.ps+ make-a-wall (x y width height)
  (let ((wall (make-ecs-entity)))
    (add-entity-tag wall :wall)
    (add-ecs-component-list
     wall
     (make-point-2d :x x :y y)
     (make-model-2d :model (make-wired-rect :width width :height height
                                            :color #xff0000)
                    :depth 100)
     (make-physic-polygon
      :target-tags '(:ball)
      :pnt-list (list (make-point-2d :x 0 :y 0)
                      (make-point-2d :x width :y 0)
                      (make-point-2d :x width :y height)
                      (make-point-2d :x 0 :y height)))
     (init-entity-params :width width
                         :height height))
    wall))
