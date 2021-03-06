(defpackage clw-block-braking/game/state/global-init
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-global-init-state))
(in-package :clw-block-braking/game/state/global-init)

(def-game-state global-init ()
  :start-process
  (state-lambda ()
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
    (load-texture :name "block-braking-gravity"
                  :path "/images/block_braking_gravity.png"
                  :alpha-path "/images/block_braking_gravity_alpha.png")
    (load-texture :name "paddle-marker-up"
                  :path "/images/paddle_marker.png"
                  :alpha-path "/images/paddle_marker_alpha.png"
                  :height 0.5)
    (load-texture :name "paddle-marker-down"
                  :path "/images/paddle_marker.png"
                  :alpha-path "/images/paddle_marker_alpha.png"
                  :y 0.5 :height 0.5)
    (load-texture :name "gravity_block"
                  :path "/images/gravity_block.png"
                  :alpha-path "/images/gravity_block_alpha.png")
    (setf-collider-model-enable nil)
    t)

  :process
  (state-lambda ()
    (make-state :menu)))
