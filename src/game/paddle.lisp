(defpackage clw-block-braking/src/game/paddle
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block
           :make-paddle
           :get-paddle-global-pnt
           :move-paddle-to
           :change-paddle-lane
           :add-paddle-move-event)
  (:import-from :clw-block-braking/src/game/parameter
                :get-param)
  (:import-from :clw-block-braking/src/game/field
                :field-width
                :field-height))
(in-package :clw-block-braking/src/game/paddle)

;; --- event --- ;;

(defun.ps+ add-paddle-move-event (paddle name func)
  (setf (gethash name (get-entity-param paddle :move-event))
        func))

(defun.ps+ call-paddle-move-event (paddle)
  (maphash (lambda (name func)
             (declare (ignore name))
             (funcall func paddle))
           (get-entity-param paddle :move-event)))

;; --- paddle marker --- ;;

(defvar.ps+ *paddle-marker-scale* 3)

(defun.ps+ calc-paddle-marker-offset (paddle)
  (let ((width (get-entity-param paddle :width))
        (height (get-entity-param paddle :height))
        (scale *paddle-marker-scale*))
    (make-point-2d :x (+ (/ (+ width height) 2)
                         (* (/ scale -2) height))
                   :y (* (/ scale -2) height))))

(defun.ps+ update-paddle-marker-by-lane (paddle)
  (check-entity-tags paddle :paddle)
  (let ((lane (get-entity-param paddle :lane))
        (lane-count (get-param :paddle :lane-count))
        (up-model (get-entity-param paddle :up-model))
        (down-model (get-entity-param paddle :down-model)))
    (when (and up-model down-model)
      (disable-model-2d paddle :target-model-2d up-model)
      (disable-model-2d paddle :target-model-2d down-model)
      (setf (model-2d-offset up-model) (calc-paddle-marker-offset paddle))
      (setf (model-2d-offset down-model) (calc-paddle-marker-offset paddle))
      (when (> lane 0)
        (enable-model-2d paddle :target-model-2d up-model))
      (when (< lane (1- lane-count))
        (enable-model-2d paddle :target-model-2d down-model)))))

(defun.ps+ append-paddle-marker (paddle)
  (check-entity-tags paddle :paddle)
  (let ((width (get-entity-param paddle :width))
        (height (get-entity-param paddle :height))
        (scale *paddle-marker-scale*))
    (frame-promise-all
     (mapcar (lambda (texture-name)
               (make-texture-model-promise
                :width (* height scale) :height (* height scale)
                :texture-name texture-name))
             '("paddle-marker-up" "paddle-marker-down"))
     (lambda (values)
       (assert (= (length values) 2))
       (let* ((model-2d-list
               (mapcar (lambda (model)
                         (make-model-2d :model model
                                        :depth (1+ (get-param :paddle :depth))
                                        :offset (calc-paddle-marker-offset paddle)))
                       values))
              (up-model-2d (nth 0 model-2d-list))
              (down-model-2d (nth 1 model-2d-list)))
         (add-ecs-component-list
          paddle
          up-model-2d
          down-model-2d)
         (set-entity-param paddle :up-model up-model-2d)
         (set-entity-param paddle :down-model down-model-2d)
         (update-paddle-marker-by-lane paddle))))))

;; --- basic --- ;;

(defun.ps+ make-paddle-model (width height)
  (make-model-2d :model (make-solid-rect :width width :height height :color #xff0000)
                 :depth (get-param :paddle :depth)
                 :offset (make-point-2d :x (* -1/2 width) :y (* -1/2 height))))

(defun.ps+ make-paddle-physic (width height)
  (let ((half-width (/ width 2))
        (half-height (/ height 2)))
    (make-physic-polygon
     :target-tags '(:ball)
     :pnt-list (list (make-point-2d :x (* -1 half-width) :y (* -1 half-height))
                     (make-point-2d :x       half-width  :y (* -1 half-height))
                     (make-point-2d :x       half-width  :y       half-height)
                     (make-point-2d :x (* -1 half-width) :y       half-height)))))

;; Note: Maybe deleting current and creating new is better
(defun.ps+ change-paddle-width (paddle width)
  (delete-ecs-component-type 'physic-2d paddle)
  (delete-ecs-component (get-entity-param paddle :model) paddle)
  (let* ((height (get-entity-param paddle :height))
         (new-model (make-paddle-model width height)))
    (add-ecs-component-list
     paddle
     new-model
     (make-paddle-physic width height))
    (set-entity-param paddle :model new-model))
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
    (change-paddle-width paddle (calc-paddle-width lane))
    (update-paddle-marker-by-lane paddle)
    (call-paddle-move-event paddle)))

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
                  (t center-x)))))
  (call-paddle-move-event paddle))

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
         (height (get-param :paddle :height))
         (model (make-paddle-model width height)))
    (add-entity-tag paddle :paddle)
    (add-ecs-component-list
     paddle
     (make-point-2d :x x :y y)
     model
     (make-paddle-physic width height)
     (init-entity-params :width width
                         :height height
                         :field field
                         :lane lane
                         :model model
                         :move-event (make-hash-table)))
    (append-paddle-marker paddle)
    paddle))
