(defpackage clw-block-braking/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-main-state)
  (:import-from :clw-block-braking/game/state/utils
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
  (state-lambda (stage-number gameover-p)
    (let*  ((field (get-field)))
      (generate-stage stage-number field))
    (add-life-decrease-event
     :reset-or-gameover
     (lambda (rest-life)
       (if (>= rest-life 0)
           (reset-ball-on-field)
           (setf gameover-p t))))
    (start-timer)
    t)

  :process
  (state-lambda (gameover-p)
    (cond ((stage-cleared-p)
           (make-state :stage-clear))
          (gameover-p
           (make-state :gameover))
          (t nil)))

  :end-process
  (state-lambda ()
    (stop-timer)
    t))

