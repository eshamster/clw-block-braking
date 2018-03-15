(defpackage clw-block-braking/src/game/block
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block
           :make-test-blocks)
  (:import-from :clw-block-braking/src/game/field
                :field-width
                :field-height))
(in-package :clw-block-braking/src/game/block)

;; TODO: (temporal funciton)
(defun.ps+ make-test-blocks (field)
  (let ((width (field-width field))
        (height (field-height field)))
    (dotimes (i 10)
      (add-ecs-entity
       (make-rect-block (+ (* width 0.04) (* width 0.08 i)) (* height 0.8)
                        (* width 0.08) (* height 0.04))
       field))))

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
                         (lambda () (delete-ecs-entity mine)))))))
    blk))
