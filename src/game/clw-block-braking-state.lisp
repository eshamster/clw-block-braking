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
                :make-ball
                :reset-ball)
  (:import-from :clw-block-braking/src/game/block
                :make-test-blocks)
  (:import-from :clw-block-braking/src/game/life
                :add-life-decrease-event
                :get-rest-life
                :init-life)
  (:import-from :clw-block-braking/src/game/paddle
                :make-paddle))
(in-package :clw-block-braking/src/game/clw-block-braking-state)

(defstruct.ps+
    (clw-block-braking-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (init-field)
                  (let*  ((field (get-field))
                          (paddle (make-paddle field))
                          (ball (make-ball field paddle)))
                    (add-ecs-entity-to-buffer paddle field)
                    (add-ecs-entity-to-buffer ball field)
                    (make-test-blocks field)
                    ;; life
                    (init-life)
                    (add-life-decrease-event
                     :reset-or-gameover
                     (lambda (rest-life)
                       (when (>= rest-life 0)
                         (let ((fallen-ball (find-a-entity-by-tag :ball)))
                           (assert fallen-ball)
                           (reset-ball fallen-ball))))))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (add-to-monitoring-log (+ "Life: " (get-rest-life)))
                  nil)))))

(defstruct.ps+
    (clw-block-braking-start-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  ;; TODO: Prevent multiple load
                  (load-font "js/")
                  (setf-collider-model-enable t)
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (make-clw-block-braking-main-state)))
               (end-process
                (lambda (_this)
                  t)))))
