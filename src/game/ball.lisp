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

;; TODO: correct not to sink into
(defun.ps+ reflect-by-vertical (ball)
  (check-entity-tags ball :ball)
  (set-entity-param ball :angle
                    (- PI (get-entity-param ball :angle))))

(defun.ps+ reflect-by-horizontal (ball)
  (check-entity-tags ball :ball)
  (set-entity-param ball :angle
                    (* (get-entity-param ball :angle) -1)))

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
        (when (> (+ x r) width)
          (reflect-by-vertical ball))
        (when (< (- x r) 0)
          (reflect-by-vertical ball))
        (when (> (+ y r) height)
          (reflect-by-horizontal ball))))))

(defun.ps+ reflect-to-rect (ball rect)
  (check-entity-tags ball :ball)
  (let ((ball-pnt (calc-global-point ball))
        (rect-pnt (calc-global-point rect))
        (rect-width (get-entity-param rect :width))
        (rect-height (get-entity-param rect :height)))
    (flet ((calc-angle-from (target-offset-x target-offset-y)
             (vector-angle (make-vector-2d
                            :x (- (vector-2d-x ball-pnt)
                                  (+ (vector-2d-x rect-pnt) target-offset-x))
                            :y (- (vector-2d-y ball-pnt)
                                  (+ (vector-2d-y rect-pnt) target-offset-y))))))
      ;; a: angle; L: left, R: right, B: bottom, T: Top
      (let ((a-LB (calc-angle-from 0 0))
            (a-LT (calc-angle-from 0 rect-height))
            (a-RT (calc-angle-from rect-width rect-height))
            (a-RB (calc-angle-from rect-width 0)))
        (cond
          ;; collide from bottom of block
          ((and (>= a-LB (* -3/4 PI)) (<= a-LB 0)
                (>= a-RB (* -1 PI)) (<= a-RB (* -1/4 PI)))
           (reflect-by-horizontal ball))
          ;; collide from top of block
          ((and (>= a-LT 0) (<= a-LT (* 3/4 PI))
                (>= a-RT (* 1/4 PI)) (<= a-RT PI))
           (reflect-by-horizontal ball))
          ;; collide from left of block
          ((and (or (and (>= a-LB (* 1/2 PI)) (<= a-LB PI))
                    (and (>= a-LB (* -1 PI)) (<= a-LB (* -3/4 PI))))
                (or (and (>= a-LT (* 3/4 PI)) (<= a-LT PI))
                    (and (>= a-LT (* -1 PI)) (<= a-LT (* -1/2 PI)))))
           (reflect-by-vertical ball))
          ;; collide from right of block
          ((and (>= a-RB (* -1/4 PI)) (<= a-RB (* 1/2 PI))
                (>= a-RT (* -1/2 PI)) (<= a-RT (* 1/4 PI)))
           (add-to-event-log "Right")
           (reflect-by-vertical ball))
          ;; something wrong...
          (t (error "The ball collides to rect from unrecognized direction")))))))

(defun.ps+ process-collide (ball target)
  (check-entity-tags ball :ball)
  (cond ((has-entity-tag target :block)
         (unless (get-entity-param ball :collided-p)
           (reflect-to-rect ball target)
           (set-entity-param ball :collided-p t)))
        ((has-entity-tag target :paddle)
         ;; TODO: Reflect when colliding
         (add-to-event-log "ball collision to paddle"))
        (t (error "Collides to unknown object."))))

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
     (make-script-2d :func (lambda (entity)
                             (set-entity-param entity :collided-p nil)
                             (move-ball entity)))
     (make-physic-circle :target-tags '(:block :paddle)
                         :r r
                         :on-collision #'process-collide)
     (init-entity-params :speed (get-param :ball :speed :init)
                         :angle (/ PI 3.9)
                         :collided-p nil ; reflect once per frame at most
                         :r r))
    ball))
