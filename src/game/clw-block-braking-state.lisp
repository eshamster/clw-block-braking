(defpackage clw-block-braking/src/game/clw-block-braking-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-clw-block-braking-start-state)
  (:import-from :clw-block-braking/src/game/field
                :init-field
                :get-field)
  (:import-from :clw-block-braking/src/game/ball
                :make-ball))
(in-package :clw-block-braking/src/game/clw-block-braking-state)

(defstruct.ps+
    (clw-block-braking-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (init-field)
                  (let ((field (get-field)))
                    (add-ecs-entity-to-buffer (make-ball field) field))
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
