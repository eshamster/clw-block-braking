(defpackage clw-block-braking/game/state/package
  (:use :cl
        :ps-experiment
        :clw-block-braking/game/state/all-clear
        :clw-block-braking/game/state/stage-clear
        :clw-block-braking/game/state/gameover
        :clw-block-braking/game/state/init
        :clw-block-braking/game/state/interval
        :clw-block-braking/game/state/main
        :clw-block-braking/game/state/menu)
  (:export init-clw-block-braking-state)
  (:import-from :cl-web-2d-game
                :init-game-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state
                :register-state-maker))
(in-package :clw-block-braking/game/state/package)

(defun.ps+ init-clw-block-braking-state ()
  (register-state-maker :all-clear #'make-game-all-clear-state)
  (register-state-maker :gameover #'make-game-gameover-state)
  (register-state-maker :init #'make-game-init-state)
  (register-state-maker :interval #'make-game-interval-state)
  (register-state-maker :main #'make-game-main-state)
  (register-state-maker :menu #'make-game-menu-state)
  (register-state-maker :stage-clear #'make-game-stage-clear-state)
  (init-game-state (make-state :menu)))
