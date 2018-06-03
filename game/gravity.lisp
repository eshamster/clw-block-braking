(defpackage clw-block-braking/game/gravity
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :add-gravity-to-entity
           :delete-gravity-from-current)
  (:import-from :clw-block-braking/game/parameter
                :get-param))
(in-package :clw-block-braking/game/gravity)

(defun.ps+ add-gravity-display (entity)
  ;; block
  (let ((width (get-entity-param entity :width))
        (height (get-entity-param entity :height)))
    (assert width)
    (assert height)
    (frame-promise-then
     (make-texture-model-promise
      :width width :height height
      :texture-name "gravity_block")
     (lambda (model)
       (let ((gravity-block (make-ecs-entity)))
         (when (find-the-entity entity)
           (add-ecs-component-list
            gravity-block
            (make-point-2d)
            (make-model-2d :model model
                           :offset (make-point-2d :x (/ width -2)
                                                  :y (/ height -2))
                           ;; TODO: parameterize
                           :depth 100)
            (make-script-2d :func (lambda (mine)
                                    (when (not (has-entity-tag entity :gravity))
                                      (register-next-frame-func
                                       (lambda () (delete-ecs-entity mine)))))))
           (add-ecs-entity gravity-block entity))))))
  ;; range
  (draw-debug-point :point (make-point-2d)
                    :parent entity
                    :r (get-param :gravity :range)
                    :fn-delete-condition
                    (lambda (_)
                      (declare (ignore _))
                      (not (has-entity-tag entity :gravity)))))

(defun.ps+ add-gravity-to-entity (entity)
  (check-type entity ecs-entity)
  (unless (find-the-entity entity)
    (return-from add-gravity-to-entity))
  (let ((pre-entity (find-a-entity-by-tag :gravity)))
    ;; only one block can have gravity
    (when pre-entity
      (delete-gravity-from-current))
    (when (eq entity pre-entity)
      (return-from add-gravity-to-entity)))
  (assert (not (find-a-entity-by-tag :gravity)))
  (add-entity-tag entity :gravity)
  (add-gravity-display entity))

(defun.ps+ delete-gravity-from-current ()
  (let ((current (find-a-entity-by-tag :gravity)))
    (when current
      (delete-entity-tag current :gravity))))

