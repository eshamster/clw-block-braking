(defpackage clw-block-braking/game/state/menu
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-menu-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state))
(in-package :clw-block-braking/game/state/menu)

(defstruct.ps+
    (game-menu-state
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
                    (add-text-to-area area
                                      :text "Click to start"
                                      :color #x00ffff)
                    (add-ecs-entity area))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (when (eq (get-left-mouse-state) :down-now)
                    (make-state :init))))
               (end-process
                (lambda (_this)
                  (declare (ignore _this))
                  t)))))
