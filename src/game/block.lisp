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
    (dotimes (y 2)
      (dotimes (x 10)
        (when (not (and (= y 0)
                        (or (= x 7)
                            (= x 8))))
          (add-ecs-entity
           (make-rect-block (+ (* width 0.04) (* width 0.08 x))
                            (+ (* height 0.7) (* height 0.2 y))
                            (* width 0.08)
                            (* height 0.04))
           field))))
    (dotimes (y 4)
      (dotimes (x 2)
        (add-ecs-entity
         (make-rect-block (+ (* width 0.04) (* width 0.8 x))
                          (+ (* height 0.73) (* height 0.04 y))
                          (* width 0.08)
                          (* height 0.04))
         field)))))

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
