(defpackage clw-block-braking/game/state/package
  (:use :cl
        :ps-experiment
        ;; The followings are required to make package-inferred-system to recognize them
        :clw-block-braking/game/state/all-clear
        :clw-block-braking/game/state/stage-clear
        :clw-block-braking/game/state/gameover
        :clw-block-braking/game/state/global-init
        :clw-block-braking/game/state/init
        :clw-block-braking/game/state/interval
        :clw-block-braking/game/state/main
        :clw-block-braking/game/state/menu)
  (:export init-clw-block-braking-state)
  (:import-from :cl-web-2d-game
                :init-game-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state))
(in-package :clw-block-braking/game/state/package)

(defun.ps+ init-clw-block-braking-state ()
  (init-game-state (make-state :global-init)))
