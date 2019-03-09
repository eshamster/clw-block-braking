(defpackage clw-block-braking/game/state/gameover
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-gameover-state)
  (:import-from :clw-block-braking/game/state/utils
                :delete-all-entities-in-next-frame))
(in-package :clw-block-braking/game/state/gameover)

(def-game-state gameover ((first-frame t))
  :start-process
  (state-lambda ()
    (let* ((font-size 25)
           (margin 20)
           (area (make-text-area :font-size font-size :text-align :center
                                 :margin margin
                                 :x (/ (get-screen-width) 2)
                                 :y (+ (/ (get-screen-height) 2)
                                       (+ (* font-size 2) margin)))))
      (add-text-to-area area :text "GAME OVER!!" :color #xff0000)
      (add-text-to-area area
                        :text "Click to return menu"
                        :color #x00ffff)
      (add-ecs-entity area))
    t)

  :process
  (state-lambda ()
    (when (eq (get-left-mouse-state) :down-now)
      (make-state :menu)))

  :end-process
  (state-lambda (first-frame)
    (if first-frame
        (progn (delete-all-entities-in-next-frame)
               (setf first-frame nil)
               nil)
        t)))
