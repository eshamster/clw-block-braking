(defpackage clw-block-braking/src/game/ball
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-ball)
  (:import-from :clw-block-braking/src/game/parameter
                :get-param)
  (:import-from :clw-block-braking/src/game/field
                :get-field
                :field-width
                :field-height))
(in-package :clw-block-braking/src/game/ball)

(defun.ps+ move-ball (ball)
  (check-entity-tags ball :ball)
  (let* ((field (get-field))
         (width (field-width field))
         (height (field-height field))
         (speed (get-entity-param ball :speed))
         (angle (get-entity-param ball :angle))
         (r (get-entity-param ball :r)))
    (with-ecs-components ((point point-2d)) ball
      (with-slots (x y) point
        (incf x (* speed (cos angle)))
        (incf y (* speed (sin angle)))
        ;; TODO: correct not to sink into
        (when (> (+ x r) width)
          (set-entity-param ball :angle (- PI angle)))
        (when (< (- x r) 0)
          (set-entity-param ball :angle (- PI angle)))
        (when (> (+ y r) height)
          (set-entity-param ball :angle (* angle -1)))))))

(defun.ps+ make-ball (field)
  (check-entity-tags field :field)
  (let ((ball (make-ecs-entity))
        (width (field-width field))
        (r (get-param :ball :r)))
    (add-entity-tag ball :ball)
    (add-ecs-component-list
     ball
     (make-point-2d :x (/ width 2) :y 50)
     (make-model-2d :model (make-solid-circle :r r :color (get-param :ball :color)))
     (make-script-2d :func (lambda (entity) (move-ball entity)))
     (init-entity-params :speed (get-param :ball :speed :init)
                         :angle (/ PI 4)
                         :r r))
    ball))
