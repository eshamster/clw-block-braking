(defpackage clw-block-braking/src/game/paddle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block
           :make-paddle)
  (:import-from :clw-block-braking/src/game/parameter
                :get-param)
  (:import-from :clw-block-braking/src/game/field
                :field-width
                :field-height))
(in-package :clw-block-braking/src/game/paddle)

(defun.ps+ move-paddle-by-mouse (paddle field)
  (check-entity-tags paddle :paddle)
  (with-ecs-components ((point point-2d)) paddle
    (let ((mouse-x (- (get-mouse-x) (point-2d-x (calc-global-point field))))
          (field-width (field-width field))
          (field-height (field-height field))
          (half-width (/ (get-entity-param paddle :width) 2)))
      (setf (point-2d-x point)
            (cond ((> (+ mouse-x half-width) field-width) (- field-width half-width))
                  ((< (- mouse-x half-width) 0) half-width)
                  (t mouse-x))))))

(defun.ps+ make-paddle (field)
  (let* ((paddle (make-ecs-entity))
         (x (* (field-width field) 0.5))
         (y (* (field-height field) 0.1))
         (width (get-param :paddle :width))
         (half-width (/ width 2))
         (height (get-param :paddle :height))
         (half-height (/ height 2)))
    (add-entity-tag paddle :paddle)
    (add-ecs-component-list
     paddle
     (make-point-2d :x x :y y)
     (make-model-2d :model (make-wired-rect :width width :height height :color #xff0000)
                    :depth (get-param :paddle :depth)
                    :offset (make-point-2d :x (* -1 half-width) :y (* -1 half-height)))
     (make-physic-polygon
      :target-tags '(:ball)
      :pnt-list (list (make-point-2d :x (* -1 half-width) :y (* -1 half-height))
                      (make-point-2d :x       half-width  :y (* -1 half-height))
                      (make-point-2d :x       half-width  :y       half-height)
                      (make-point-2d :x (* -1 half-width) :y       half-height)))
     (make-script-2d :func (lambda (_this) 
                             (move-paddle-by-mouse _this field)))
     (init-entity-params :width width
                         :height height))
    paddle))
