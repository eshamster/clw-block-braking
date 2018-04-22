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
                  ;; TODO: Prevent multiple load
                  (load-font "js/")
                  (load-texture :name "block"
                                :path "/images/block.png"
                                :alpha-path "/images/block_alpha.png")
                  (load-texture :name "ball"
                                :path "/images/ball.png"
                                :alpha-path "/images/ball_alpha.png")
                  (load-texture :name "block-braking"
                                :path "/images/block_braking.png"
                                :alpha-path "/images/block_braking_alpha.png")
                  (load-texture :name "paddle-marker-up"
                                :path "/images/paddle_marker.png"
                                :alpha-path "/images/paddle_marker_alpha.png"
                                :height 0.5)
                  (load-texture :name "paddle-marker-down"
                                :path "/images/paddle_marker.png"
                                :alpha-path "/images/paddle_marker_alpha.png"
                                :y 0.5 :height 0.5)
                  (setf-collider-model-enable nil)
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
