(defpackage clw-block-braking/game/controller
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-controller)
  (:import-from :clw-block-braking/game/ball
                :shoot-ball
                :ball-shot-p)
  (:import-from :clw-block-braking/game/paddle
                :move-paddle-to
                :change-paddle-lane)
  (:import-from :clw-block-braking/game/parameter
                :get-param))
(in-package :clw-block-braking/game/controller)

;; --- gravity --- ;;

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
         (add-ecs-entity gravity-block entity)))))
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

;; --- initialization --- ;;

(defun.ps+ process-by-mouse (entity)
  (check-entity-tags entity :mouse)
  ;; update point
  (with-ecs-components (point-2d) entity
    (setf (point-2d-x point-2d) (get-mouse-x)
          (point-2d-y point-2d) (get-mouse-y)))
  (when (eq (get-left-mouse-state) :down-now)
    (let ((ball (find-a-entity-by-tag :ball)))
      (assert ball)
      (if (ball-shot-p ball)
          ;; add gravity to current target
          (let ((target (get-entity-param entity :current-target)))
            (if target
                (add-gravity-to-entity target)
                (delete-gravity-from-current)))
          ;; shoot ball
          (shoot-ball ball))))
  (set-entity-param entity :current-target nil)
  ;; move paddle
  (let ((paddle (find-a-entity-by-tag :paddle)))
    (let ((wheel (get-mouse-wheel-delta-y)))
      (unless (= wheel 0)
        (change-paddle-lane paddle (< wheel 0))))
    (move-paddle-to paddle (get-mouse-x))))

(defun.ps+ make-mouse-entity ()
  (let ((mouse (make-ecs-entity))
        (r 3))
    (add-entity-tag mouse :mouse)
    (add-ecs-component-list
     mouse
     (make-point-2d)
     (make-model-2d :model (make-solid-circle :r r :color #x000000)
                    :depth 100)
     (make-model-2d :model (make-wired-circle :r 10 :color #x000000)
                    :depth 100)
     (make-physic-circle
      :r 3
      :on-collision
      (lambda (mine target)
        (let ((pre-target (get-entity-param mine :current-target))
              (mine-point (calc-global-point mine)))
          ;; In case where multiple targets are collided in a frame,
          ;; choose the nearest target.
          (when (or (not pre-target)
                    (< (calc-dist-p2 mine (calc-global-point target))
                       (calc-dist-p2 mine (calc-global-point pre-target))))
            (set-entity-param mine :current-target target)))))
     (make-script-2d :func #'process-by-mouse)
     (init-entity-params :current-target nil))
    mouse))

(defun.ps+ init-controller ()
  (add-ecs-entity (make-mouse-entity)))
