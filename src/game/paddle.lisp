(defpackage clw-block-braking/src/game/paddle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block
           :make-paddle
           :get-paddle-global-pnt
           :move-paddle-to
           :change-paddle-lane)
  (:import-from :clw-block-braking/src/game/parameter
                :get-param)
  (:import-from :clw-block-braking/src/game/field
                :field-width
                :field-height))
(in-package :clw-block-braking/src/game/paddle)

(defun.ps+ make-paddle-model (width height)
  (make-model-2d :model (make-solid-rect :width width :height height :color #xff0000)
                 :depth (get-param :paddle :depth)
                 :offset (make-point-2d :x (* -1/2 width) :y (* -1/2 height))))

(defun.ps+ make-paddle-physic (width height)
  (add-to-event-log (+ width ":" height))
  (let ((half-width (/ width 2))
        (half-height (/ height 2)))
    (make-physic-polygon
     :target-tags '(:ball)
     :pnt-list (list (make-point-2d :x (* -1 half-width) :y (* -1 half-height))
                     (make-point-2d :x       half-width  :y (* -1 half-height))
                     (make-point-2d :x       half-width  :y       half-height)
                     (make-point-2d :x (* -1 half-width) :y       half-height)))))

(defun.ps+ change-paddle-width (paddle width)
  (delete-ecs-component-type 'physic-2d paddle)
  (delete-ecs-component-type 'model-2d paddle)
  (let ((height (get-entity-param paddle :height)))
    (add-ecs-component-list
     paddle
     (make-paddle-model width height)
     (make-paddle-physic width height)))
  (set-entity-param paddle :width width))

(defun.ps+ calc-paddle-width (lane)
  (lerp-scalar (get-param :paddle :width :min)
               (get-param :paddle :width :max)
               (* 1.0 (/ lane (1- (get-param :paddle :lane-count))))))

(defun.ps+ change-paddle-lane (paddle up-p)
  (check-entity-tags paddle :paddle)
  (let ((lane (get-entity-param paddle :lane))
        (lane-count (get-param :paddle :lane-count)))
    (incf lane (if up-p 1 -1))
    (when (>= lane lane-count)
      (setf lane (1- lane-count)))
    (when (< lane 0)
      (setf lane 0))
    (setf (point-2d-y (get-ecs-component 'point-2d paddle))
          (+ (get-param :paddle :base-line-height)
             (* (get-param :paddle :lane-space)
                lane)))
    (set-entity-param paddle :lane lane)
    (change-paddle-width paddle (calc-paddle-width lane))))

(defun.ps+ move-paddle-to (paddle global-x)
  (check-entity-tags paddle :paddle)
  (with-ecs-components ((point point-2d)) paddle
    (let* ((field (get-entity-param paddle :field))
           (global-pnt (calc-global-point paddle))
           (center-x (point-2d-x
                      (calc-local-point
                       paddle (make-point-2d :x global-x :y (point-2d-y global-pnt)))))
           (field-width (field-width field))
           (half-width (/ (get-entity-param paddle :width) 2)))
      (setf (point-2d-x point)
            (cond ((> (+ center-x half-width) field-width) (- field-width half-width))
                  ((< (- center-x half-width) 0) half-width)
                  (t center-x))))))

(defun.ps+ get-paddle-global-pnt (paddle)
  (check-entity-tags paddle :paddle)
  (let ((result (calc-global-point paddle)))
    (decf (point-2d-x result) (/ (get-entity-param paddle :width) 2))
    (decf (point-2d-y result) (/ (get-entity-param paddle :height) 2))
    result))

(defun.ps+ make-paddle (field)
  (let* ((paddle (make-ecs-entity))
         (x (* (field-width field) 0.5))
         (y (get-param :paddle :base-line-height))
         (lane 0)
         (width (calc-paddle-width lane))
         (height (get-param :paddle :height)))
    (add-entity-tag paddle :paddle)
    (add-ecs-component-list
     paddle
     (make-point-2d :x x :y y)
     (make-paddle-model width height)
     (make-paddle-physic width height)
     (init-entity-params :width width
                         :height height
                         :field field
                         :lane lane))
    paddle))
