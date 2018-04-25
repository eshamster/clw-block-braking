(defpackage clw-block-braking/game/state/package
  (:use :cl
        :ps-experiment
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
                :make-state
                :register-state-maker))
(in-package :clw-block-braking/game/state/package)

(defun.ps+ init-clw-block-braking-state ()
  (macrolet ((register-all (&rest target-list)
               `(progn ,@(mapcar (lambda (target)
                                   `(register-state-maker
                                     ,target
                                     (function ,(intern (format nil "MAKE-GAME-~A-STATE" target)))))
                                 target-list))))
    (register-all :all-clear
                  :gameover
                  :global-init
                  :init
                  :interval
                  :main
                  :menu
                  :stage-clear))
  (init-game-state (make-state :global-init)))
