(defpackage clw-block-braking/game/state/interval
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-interval-state)
  (:import-from :clw-block-braking/game/state/utils
                :reset-ball-on-field)
  (:import-from :clw-block-braking/game/timer
                :reset-timer))
(in-package :clw-block-braking/game/state/interval)

(def-game-state interval ((parent-entity (make-ecs-entity))
                          next-stage-number)
  :start-process
  (state-lambda (parent-entity)
    (let* ((parent parent-entity)
           (font-size 25)
           (margin 20)
           (area (make-text-area :font-size font-size :text-align :center
                                 :margin margin
                                 :x (/ (get-screen-width) 2)
                                 :y (+ (/ (get-screen-height) 2)
                                       (+ (* font-size 2) margin)))))
      (add-text-to-area area
                        :text "Stage Clear!!"
                        :color #x00ffff)
      (add-text-to-area area
                        :text "Click for next stage!"
                        :color #x00ffff)
      (add-ecs-entity parent)
      (add-ecs-entity area parent))
    t)

  :process
  (state-lambda (next-stage-number)
    (when (eq (get-left-mouse-state) :down-now)
      (make-state :main
                  :stage-number next-stage-number)))

  :end-process
  (state-lambda (parent-entity)
    (reset-timer)
    (reset-ball-on-field)
    (delete-ecs-entity parent-entity)
    t))
