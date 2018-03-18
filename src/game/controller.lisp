(defpackage clw-block-braking/src/game/controller
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-controller)
  (:import-from :clw-block-braking/src/game/ball
                :shoot-ball)
  (:import-from :clw-block-braking/src/game/paddle
                :move-paddle-to))
(in-package :clw-block-braking/src/game/controller)

(defun.ps+ init-controller ()
  (let ((controller (make-ecs-entity)))
    (add-entity-tag controller :controller)
    (add-ecs-component-list
     controller
     (make-script-2d
      :func (lambda (_)
              (declare (ignore _))
              (when (= (get-left-mouse-state) :down-now)
                (shoot-ball (find-a-entity-by-tag :ball)))
              (move-paddle-to (find-a-entity-by-tag :paddle)
                              (get-mouse-x)))))
    (add-ecs-entity controller)))
