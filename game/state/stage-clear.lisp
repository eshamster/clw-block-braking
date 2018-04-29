(defpackage clw-block-braking/game/state/stage-clear
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-stage-clear-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state
                :def-game-state
                :get-current-ball)
  (:import-from :clw-block-braking/game/ball
                :stop-ball)
  (:import-from :clw-block-braking/game/score-register
                :register-score)
  (:import-from :clw-block-braking/game/stage-generator
                :get-max-stage-number)
  (:import-from :clw-block-braking/game/timer
                :get-current-sec))
(in-package :clw-block-braking/game/state/stage-clear)

(def-game-state stage-clear (cleared-stage-number)
  :start-process
  (lambda (_this)
    (stop-ball (get-current-ball))
    (register-score :stage (slot-value _this 'cleared-stage-number)
                    :time (get-current-sec))
    t)

  :process
  (lambda (_this)
    (let ((next-stage
           (1+ (slot-value _this 'cleared-stage-number))))
      (add-to-event-log next-stage)
      (if (<= next-stage (get-max-stage-number))
          (make-state :interval :next-stage-number next-stage)
          (make-state :all-clear)))))
