(defpackage clw-block-braking/game/game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-func
           :update-func)
  (:import-from :clw-block-braking/game/ui
                :init-ui-system)
  (:import-from :clw-block-braking/game/state/package
                :init-clw-block-braking-state))
(in-package :clw-block-braking/game/game)

(defun.ps+ init-func (scene)
  (init-clw-block-braking-state)
  (init-default-systems :scene scene)
  (register-ecs-system "clw-bb-ui" (init-ui-system))
  (init-input))

(defun.ps+ update-func ()
  (process-game-state))
