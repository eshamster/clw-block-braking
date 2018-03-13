(defpackage clw-block-braking/src/game/clw-block-braking-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-clw-block-braking-start-state))
(in-package :clw-block-braking/src/game/clw-block-braking-state)

(defstruct.ps+
    (clw-block-braking-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  nil)))))

(defstruct.ps+
    (clw-block-braking-start-state
     (:include game-state
               (start-process
                (lambda (_this)
                  ;; TODO: Prevent multiple load
                  (load-font "js/")
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (make-clw-block-braking-main-state)))
               (end-process
                (lambda (_this)
                  t)))))
