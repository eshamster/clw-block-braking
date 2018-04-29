(defpackage clw-block-braking/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-main-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state
                :def-game-state
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

(def-game-state main (stage-number (gameover-p nil))
  :start-process
  (lambda (_this)
    (let*  ((field (get-field)))
      (generate-stage (slot-value _this 'stage-number)
                      field))
    (add-life-decrease-event
     :reset-or-gameover
     (lambda (rest-life)
       (if (>= rest-life 0)
           (reset-ball-on-field)
           (setf (game-main-state-gameover-p _this) t))))
    (start-timer)
    t)

  :process
  (lambda (_this)
    (cond ((stage-cleared-p)
           (make-state :stage-clear
                       :cleared-stage-number (slot-value _this 'stage-number)))
          ((slot-value _this 'gameover-p)
           (make-state :gameover))
          (t nil))))

