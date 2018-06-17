(defpackage clw-block-braking/game/state/stage-clear
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-stage-clear-state)
  (:import-from :clw-block-braking/game/state/utils
                :get-current-ball)
  (:import-from :clw-block-braking/game/ball
                :stop-ball)
  (:import-from :clw-block-braking/game/score-register
                :register-score)
  (:import-from :clw-block-braking/game/stage-generator
                :get-max-stage-number)
  (:import-from :clw-block-braking/game/stage-manager
                :get-current-stage-number
                :go-to-next-stage)
  (:import-from :clw-block-braking/game/timer
                :get-current-sec))
(in-package :clw-block-braking/game/state/stage-clear)

(def-game-state stage-clear ()
  :start-process
  (lambda (_this)
    (declare (ignore _this))
    (stop-ball (get-current-ball))
    (register-score :stage (get-current-stage-number)
                    :time (get-current-sec))
    t)

  :process
  (lambda (_this)
    (declare (ignore _this))
    (let ((next-stage (go-to-next-stage)))
      (if next-stage
          (make-state :interval :next-stage-number next-stage)
          (make-state :all-clear)))))
