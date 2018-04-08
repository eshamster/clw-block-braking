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

(defun.ps+ add-block-braking-animation (blk)
  (frame-promise-then
   (get-texture-promise "block-braking")
   (lambda (texture)
     (let* ((entity (make-ecs-entity))
            (scale 2)
            (model-2d (make-model-2d :model (make-texture-model
                                             :width (* (get-entity-param blk :width) scale)
                                             :height (* (get-entity-param blk :height) scale)
                                             :texture texture)
                                     :depth 1))
            (anime-2d (init-animation-2d
                       :interval 0 :vert-count 5 :horiz-count 6
                       :model model-2d :texture texture
                       :animation-end-callback
                       (lambda (anime)
                         (declare (ignore anime))
                         (delete-ecs-entity entity))))
            (blk-point (get-ecs-component 'point-2d blk)))
       (add-ecs-component-list
        entity
        (make-point-2d :x (- (point-2d-x blk-point)
                             (/ (* (get-entity-param blk :width) (1- scale)) 2))
                       :y (- (point-2d-y blk-point)
                             (/ (* (get-entity-param blk :height) (1- scale)) 2)))
        model-2d
        anime-2d)
       (start-animation anime-2d)
       (add-ecs-entity-to-buffer entity (clw-block-braking/src/game/field:get-field))))))

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
                         (lambda () (delete-ecs-entity mine)))
                        (add-block-braking-animation blk))))
     (init-entity-params :width width
                         :height height))
    blk))
