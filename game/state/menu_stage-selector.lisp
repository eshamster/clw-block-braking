(defpackage clw-block-braking/game/state/menu_stage-selector
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-stage-selector
           :generate-stage-list)
  (:import-from :clw-block-braking/game/ui
                :make-ui-component)
  (:import-from :clw-block-braking/game/stage-generator
                :get-max-stage-number))
(in-package :clw-block-braking/game/state/menu_stage-selector)

(defun.ps+ increment-selector (selector delta)
  (aset-entity-param selector :selected
                     (mod (+ it delta)
                          (length (get-entity-param selector :choices))))
  ;; TODO: (temporal)
  (add-to-event-log
   (nth (get-entity-param selector :selected) (get-entity-param selector :choices))))

(defun.ps+ generate-all-stage-list ()
  (loop for i from 1 to (get-max-stage-number)
     collect i))

(defun.ps+ generate-stage-list (selector)
  (let ((target (nth (get-entity-param selector :selected)
                     (get-entity-param selector :choices))))
    (case target
      (:all (generate-all-stage-list))
      (t (list target)))))

(defun.ps+ make-stage-selector ()
  (let ((selector (make-ecs-entity))
        (choices (generate-all-stage-list)))
    (push :all choices)
    (add-entity-tag selector :stage-selector)
    (add-ecs-component-list
     selector
     (make-script-2d
      :func (lambda (entity)
              (let ((wheel-delta (get-mouse-wheel-delta-y)))
                (cond ((> wheel-delta 0)
                       (increment-selector entity 1))
                      ((< wheel-delta 0)
                       (increment-selector entity -1))))))
     (init-entity-params :choices choices
                         :selected 0))
    selector))
