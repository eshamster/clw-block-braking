(defpackage clw-block-braking/game/stage-manager
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-stage-manager
           :go-to-next-stage
           :get-current-stage-number
           :get-selected-stage-list))
(in-package :clw-block-braking/game/stage-manager)

(defun.ps+ find-stage-manager ()
  (find-a-entity-by-tag :stage-manager))

(defun.ps+ get-current-stage-number (&optional (manager (find-stage-manager)))
  (check-entity-tags manager :stage-manager)
  (nth (get-entity-param manager :current-stage-index)
       (get-entity-param manager :stage-list)))

(defun.ps+ go-to-next-stage (&optional (manager (find-stage-manager)))
  (check-entity-tags manager :stage-manager)
  (let ((max-stage-index (1- (length (get-entity-param manager :stage-list))))
        (current-index (get-entity-param manager :current-stage-index)))
    (when (< current-index max-stage-index)
      (set-entity-param manager :current-stage-index (1+ current-index))
      (get-current-stage-number manager))))

(defun.ps+ get-selected-stage-list (&optional (manager (find-stage-manager)))
  (check-entity-tags manager :stage-manager)
  (get-entity-param manager :stage-list))

(defun.ps+ make-stage-manager (stage-list)
  (let ((manager (make-ecs-entity)))
    (add-entity-tag manager :stage-manager)
    (add-ecs-component-list
     manager
     (init-entity-params
      :stage-list stage-list
      :current-stage-index 0))
    manager))
