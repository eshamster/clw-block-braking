(defpackage clw-block-braking/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-main-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state
                :reset-ball-on-field)
  (:import-from :clw-block-braking/game/field
                :get-field)
  (:import-from :clw-block-braking/game/life
                :add-life-decrease-event)
  (:import-from :clw-block-braking/game/stage-generator
                :generate-stage)
  (:import-from :clw-block-braking/game/timer
                :start-timer
                :stop-timer))
(in-package :clw-block-braking/game/state/main)

(defun.ps+ stage-cleared-p ()
  (not (find-a-entity-by-tag :block)))

(defstruct.ps+
    (game-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (let*  ((field (get-field)))
                    (generate-stage (game-main-state-stage-number _this)
                                    field))
                  (add-life-decrease-event
                   :reset-or-gameover
                   (lambda (rest-life)
                     (if (>= rest-life 0)
                         (reset-ball-on-field)
                         (setf (game-main-state-gameover-p _this) t))))
                  (start-timer)
                  t))
               (process
                (lambda (_this)
                  (cond ((stage-cleared-p)
                         (make-state :stage-clear
                                     :cleared-stage-number (game-main-state-stage-number _this)))
                        ((game-main-state-gameover-p _this)
                         (make-state :gameover))
                        (t nil))))
               (end-process
                (lambda (_this)
                  (stop-timer)
                  t))))
    stage-number
    (gameover-p nil))
