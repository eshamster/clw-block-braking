(defpackage clw-block-braking/game/block
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block)
  (:import-from :clw-block-braking/game/ui
                :make-ui-component)
  (:import-from :clw-block-braking/game/field
                :field-width
                :field-height
                :get-field))
(in-package :clw-block-braking/game/block)

(defun.ps+ add-block-braking-animation (blk)
  (frame-promise-then
   (if (has-entity-tag blk :gravity)
       (get-texture-promise "block-braking-gravity")
       (get-texture-promise "block-braking"))
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
                             (* (get-entity-param blk :width) (1- scale)))
                       :y (- (point-2d-y blk-point)
                             (* (get-entity-param blk :height) (1- scale))))
        model-2d
        anime-2d)
       (start-animation anime-2d)
       (add-ecs-entity-to-buffer entity (get-field))))))

(defun.ps+ add-display-on-hover (blk width height)
  (let* ((margin 2)
         (hover-model (make-model-2d :model (make-wired-rect :width (+ width (* margin 2))
                                                             :height (+ height (* margin 2))
                                                             :color #xff8800)
                                     :offset (make-point-2d
                                              :x (- (/ width -2) margin)
                                              :y (- (/ height -2) margin))
                                     :depth 100)))
    (add-ecs-component-list
     blk
     hover-model
     (make-ui-component :on-hover (lambda (_)
                                    (declare (ignore _))
                                    (enable-model-2d blk
                                                     :target-model-2d hover-model))
                        :on-not-hover (lambda (_)
                                        (declare (ignore _))
                                        (disable-model-2d blk
                                                          :target-model-2d hover-model))))
    (disable-model-2d blk :target-model-2d hover-model)
    blk))

(defun.ps+ make-rect-block (x y width height)
  (let ((blk (make-ecs-entity))
        (h-width (/ width 2))
        (h-height (/ height 2)))
    (add-entity-tag blk :block)
    (add-ecs-component-list
     blk
     (make-point-2d :x (+ x h-width) :y (+ y h-height))
     (make-physic-polygon
      :target-tags '(:ball :mouse)
      :pnt-list (list (make-point-2d :x (* -1 h-width) :y (* -1 h-height))
                      (make-point-2d :x h-width :y (* -1 h-height))
                      (make-point-2d :x h-width :y h-height)
                      (make-point-2d :x (* -1 h-width) :y h-height))
      :on-collision (lambda (mine target)
                      (when (has-entity-tag target :ball)
                        (register-next-frame-func
                         (lambda () (delete-ecs-entity mine)))
                        (add-block-braking-animation blk))))
     (init-entity-params :width width
                         :height height))
    (add-display-on-hover blk width height)
    blk))
