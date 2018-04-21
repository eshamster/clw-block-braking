(defpackage clw-block-braking/game/controller
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-controller)
  (:import-from :clw-block-braking/game/ball
                :shoot-ball)
  (:import-from :clw-block-braking/game/paddle
                :move-paddle-to
                :change-paddle-lane))
(in-package :clw-block-braking/game/controller)

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
              (let ((paddle (find-a-entity-by-tag :paddle)))
                (let ((wheel (get-mouse-wheel-delta-y)))
                  (unless (= wheel 0)
                    (change-paddle-lane paddle (< wheel 0))))
                (move-paddle-to paddle (get-mouse-x))))))
    (add-ecs-entity controller)))
