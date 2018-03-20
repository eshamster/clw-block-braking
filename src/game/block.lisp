(defpackage clw-block-braking/src/game/block
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block)
  (:import-from :clw-block-braking/src/game/field
                :field-width
                :field-height))
(in-package :clw-block-braking/src/game/block)

(defun.ps+ make-rect-block (x y width height)
  (let ((blk (make-ecs-entity)))
    (add-entity-tag blk :block)
    (add-ecs-component-list
     blk
     (make-point-2d :x x :y y)
     (make-physic-polygon
      :target-tags '(:ball)
      :pnt-list (list (make-point-2d :x 0 :y 0)
                      (make-point-2d :x width :y 0)
                      (make-point-2d :x width :y height)
                      (make-point-2d :x 0 :y height))
      :on-collision (lambda (mine target)
                      (when (has-entity-tag target :ball)
                        (register-next-frame-func
                         (lambda () (delete-ecs-entity mine))))))
     (init-entity-params :width width
                         :height height))
    blk))
