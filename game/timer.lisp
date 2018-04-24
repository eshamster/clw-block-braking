(defpackage clw-block-braking/game/timer
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-timer
           :start-timer
           :stop-timer
           :reset-timer
           :get-current-sec))
(in-package :clw-block-braking/game/timer)

(defun.ps+ find-timer ()
  (find-a-entity-by-tag :timer))

(defun.ps+ start-timer (&optional (timer (find-timer)))
  (check-entity-tags timer :timer)
  (set-entity-param timer :enable-p t))

(defun.ps+ stop-timer (&optional (timer (find-timer)))
  (check-entity-tags timer :timer)
  (set-entity-param timer :enable-p nil))

(defun.ps+ reset-timer (&optional (timer (find-timer)))
  (check-entity-tags timer :timer)
  (stop-timer timer)
  (set-entity-param timer :frame-count 0))

(defun.ps+ get-current-sec (&optional (timer (find-timer)))
  (let ((scale 10))
    (/ (floor (/ (* (get-entity-param timer :frame-count) scale)
                                   60))
                         scale)))

(defun.ps+ init-timer ()
  (let ((timer (make-ecs-entity)))
    (flet ((display-timer (entity)
             (let ((frame-count (get-entity-param entity :frame-count)))
               (when (= (mod frame-count 60) 0)
                 (clear-text-area entity)
                 (add-text-to-area entity
                                   :text (floor (/ frame-count 60))
                                   :color #xffffff)))))
      (add-entity-tag timer :timer)
      (add-ecs-component-list
       timer
       ;; TODO: parameterize
       (make-point-2d :x 780 :y 550)
       (make-text-area-component :font-size 20
                                 :text-align :right
                                 :margin 10)
       (make-script-2d :func (lambda (entity)
                               (when (get-entity-param entity :enable-p)
                                 (aset-entity-param entity :frame-count (1+ it)))
                               (display-timer entity)))
       (init-entity-params :frame-count 0
                           :enable-p nil))
      (display-timer timer))
    (add-ecs-entity timer)))
