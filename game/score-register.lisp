(defpackage clw-block-braking/game/score-register
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-score-register
           :register-score
           :get-score
           :score
           :score-time))
(in-package :clw-block-braking/game/score-register)

(defstruct.ps+ score (time 0))

(defun.ps+ find-score-register ()
  (find-a-entity-by-tag :score-register))

(defun.ps+ reset-score-register (&optional (score-register (find-score-register)))
  (check-entity-tags score-register :score-register)
  (set-entity-param score-register :frame-count 0))

(defun.ps+ register-score (&key (score-register (find-score-register))
                                stage time)
  (check-entity-tags score-register :score-register)
  (assert stage)
  ;; Note: In JavaScript, 0 is interpreted as false.
  (assert (not (null time)))
  (setf (gethash stage (get-entity-param score-register :register))
        (make-score :time time)))

(defun.ps+ get-score (stage &optional (score-register (find-score-register)))
  (check-entity-tags score-register :score-register)
  (let ((score (gethash stage (get-entity-param score-register :register))))
    (if score score (make-score))))

(defun.ps+ init-score-register ()
  (let ((score-register (make-ecs-entity)))
    (add-entity-tag score-register :score-register)
    (add-ecs-component-list
     score-register
     (init-entity-params :register (make-hash-table)))
    (add-ecs-entity score-register)))
