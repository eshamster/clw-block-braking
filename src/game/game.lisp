(defpackage clw-block-braking/src/game/game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-func
           :update-func)
  (:import-from :clw-block-braking/src/game/clw-block-braking-state
                :make-clw-block-braking-start-state))
(in-package :clw-block-braking/src/game/game)

(defun.ps+ init-func (scene)
  (init-game-state (make-clw-block-braking-start-state))
  (init-default-systems :scene scene)
  (init-input))

(defun.ps+ update-func ()
  (process-game-state))
