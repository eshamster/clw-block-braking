(defpackage clw-block-braking/game/state/gameover
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-gameover-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state
                :delete-all-entities-in-next-frame))
(in-package :clw-block-braking/game/state/gameover)

(defstruct.ps+
    (game-gameover-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
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
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (when (eq (get-left-mouse-state) :down-now)
                    (make-state :menu))))
               (end-process
                (lambda (_this)
                  (with-slots (first-frame) _this
                    (if first-frame
                        (progn (delete-all-entities-in-next-frame)
                               (setf first-frame nil)
                               nil)
                        t))))))
    (first-frame t))
