(defpackage clw-block-braking/game/state/menu
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-menu-state)
  (:import-from :clw-block-braking/game/state/menu_stage-selector
                :init-stage-selector
                :generate-stage-list))
(in-package :clw-block-braking/game/state/menu)

;; --- mouse --- ;;

(defun.ps+ make-mouse-pointer ()
  (let ((mouse (make-ecs-entity))
        (r 3))
    (add-ecs-component-list
     mouse
     (make-point-2d)
     (make-model-2d :model (make-solid-circle :r r :color #xff0088)
                    :depth 100)
     (make-script-2d
      :func (lambda (entity)
              (with-ecs-components (point-2d) entity
                (setf (point-2d-x point-2d) (get-mouse-x)
                      (point-2d-y point-2d) (get-mouse-y))))))
    mouse))

;; --- menu --- ;;

(def-game-state menu ((dummy-parent (make-ecs-entity))
                      next-state)
  :start-process
  (state-lambda (dummy-parent next-state)
    (let* ((font-size 25)
           (margin 20)
           (center-x (/ (get-screen-width) 2))
           (top-y (+ (/ (get-screen-height) 2)
                     (+ (* font-size 2) margin)))
           (area (make-text-area :font-size font-size :text-align :center
                                 :margin margin
                                 :x center-x
                                 :y top-y)))
      ;; dummy parent
      (add-ecs-entity dummy-parent)
      ;; text area
      (frame-promise-then
       (add-text-to-area area
                         :text "Click here to start"
                         :color #x00ffff)
       ;; Note: shadow "area"
       (lambda (area)
         (let* ((area-size (get-text-area-size area))
                (h-width (/ (getf area-size :width) 2))
                (height (getf area-size :height))
                (hover-model (make-model-2d :model (make-wired-rect :width (* 2 h-width)
                                                                    :height height
                                                                    :color #xff8800)
                                            :offset (make-point-2d :x (* -1 h-width)
                                                                   :y (- (* -1 height)
                                                                         margin))
                                            :depth 100)))
           (add-ecs-component-list
            area
            (make-ui-component
             :on-click-up (lambda ()
                            (setf next-state
                                  (make-state
                                   :init
                                   :stage-list (generate-stage-list
                                                (find-a-entity-by-tag :stage-selector)))))
             :on-mouse-hover (lambda ()
                               (enable-model-2d
                                area :target-model-2d hover-model))
             :on-mouse-not-hover (lambda ()
                                   (disable-model-2d
                                    area :target-model-2d hover-model)))
            (make-physic-polygon
             :pnt-list (list (make-vector-2d :x (* -1 h-width) :y (- (* -1 height) margin))
                             (make-vector-2d :x h-width :y (- (* -1 height) margin))
                             (make-vector-2d :x h-width :y (* margin -1))
                             (make-vector-2d :x (* -1 h-width) :y (* margin -1))))
            hover-model)
           (add-ecs-entity area dummy-parent)
           (disable-model-2d area :target-model-2d hover-model))))
      ;; mouse
      (add-ecs-entity (make-mouse-pointer) dummy-parent)
      ;; stage selector
      (init-stage-selector dummy-parent
                           :offset (make-point-2d :x 270 :y 370)))
    t)

  :process
  (state-lambda (next-state)
    next-state)

  :end-process
  (state-lambda (dummy-parent)
    (delete-ecs-entity dummy-parent)
    t))
