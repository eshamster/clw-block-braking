(defpackage clw-block-braking/game/state/utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :get-current-ball
           :reset-ball-on-field
           :delete-all-entities-in-next-frame)
  (:import-from :clw-block-braking/game/ball
                :reset-ball))
(in-package :clw-block-braking/game/state/utils)

;; --- other utils --- ;;

(defun.ps+ get-current-ball ()
  (let ((current-ball (find-a-entity-by-tag :ball)))
    (assert current-ball)
    current-ball))

(defun.ps+ reset-ball-on-field ()
  (reset-ball (get-current-ball)))

(defun.ps+ delete-all-entities-in-next-frame ()
  (do-ecs-entities entity
    (unless (ecs-entity-parent entity)
      ;; Note: Without the following (shallow) copy,
      ;; all registered functions wrongly share a same entity.
      (let ((entity-copy entity))
        (register-next-frame-func
         (lambda () (delete-ecs-entity entity-copy)))))))
