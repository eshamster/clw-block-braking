(defpackage clw-block-braking/game/state/menu
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-menu-state)
  (:import-from :clw-block-braking/game/ui
                :make-ui-component)
  (:import-from :clw-block-braking/game/state/utils
                :make-state
                :def-game-state))
(in-package :clw-block-braking/game/state/menu)

(def-game-state menu ((dummy-parent (make-ecs-entity))
                      next-state)
  :start-process
  (lambda (_this)
    (let* ((font-size 25)
           (margin 20)
           (area (make-text-area :font-size font-size :text-align :center
                                 :margin margin
                                 :x (/ (get-screen-width) 2)
                                 :y (+ (/ (get-screen-height) 2)
                                       (+ (* font-size 2) margin))))
           (dummy-parent (slot-value _this 'dummy-parent)))
      (add-ecs-entity dummy-parent)
      (add-text-to-area area
                        :text "Click to start"
                        :color #x00ffff)
      (let ((h-width (/ (* 0.8 (get-screen-width)) 2))
            (h-height (/ (* 0.2 (get-screen-height)) 2)))
        (add-ecs-component-list
         area
         (make-ui-component :on-click (lambda (_)
                                        (declare (ignore _))
                                        (setf (slot-value _this 'next-state)
                                              (make-state :init))))
         (make-physic-polygon
          :pnt-list (list (make-vector-2d :x (* -1 h-width) :y (* -1 h-height))
                          (make-vector-2d :x h-width :y (* -1 h-height))
                          (make-vector-2d :x h-width :y h-height)
                          (make-vector-2d :x (* -1 h-width) :y h-height)))))
      (setf-collider-model-enable t)
      (add-ecs-entity area dummy-parent))
      (setf-collider-model-enable nil)
    t)

  :process
  (lambda (_this)
    (slot-value _this 'next-state))

  :end-process
  (lambda (_this)
    (delete-ecs-entity (slot-value _this 'dummy-parent))
    t))
