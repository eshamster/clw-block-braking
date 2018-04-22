(defpackage clw-block-braking/game/state/init
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-init-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state)
  (:import-from :clw-block-braking/game/ball
                :make-ball)
  (:import-from :clw-block-braking/game/controller
                :init-controller)
  (:import-from :clw-block-braking/game/field
                :get-field
                :init-field)
  (:import-from :clw-block-braking/game/life
                :init-life)
  (:import-from :clw-block-braking/game/paddle
                :make-paddle)
  (:import-from :clw-block-braking/game/score-register
                :init-score-register)
  (:import-from :clw-block-braking/game/timer
                :init-timer)
  (:import-from :clw-block-braking/game/wall
                :init-wall))
(in-package :clw-block-braking/game/state/init)

(defstruct.ps+
    (game-init-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (init-field)
                  (init-controller)
                  (let*  ((field (get-field))
                          (paddle (make-paddle field))
                          (ball (make-ball field paddle)))
                    (add-ecs-entity-to-buffer paddle field)
                    (add-ecs-entity-to-buffer ball field)
                    (init-wall field)
                    (init-life field))
                  (init-timer)
                  (init-score-register)
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (make-state :main
                              :stage-number 1))))))
