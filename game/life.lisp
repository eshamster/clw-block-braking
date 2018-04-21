(defpackage clw-block-braking/game/life
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-life
           :get-rest-life
           :add-life-decrease-event)
  (:import-from :clw-block-braking/game/ball
                :add-ball-falling-event)
  (:import-from :clw-block-braking/game/parameter
                :get-param))
(in-package :clw-block-braking/game/life)

;; --- event --- ;;

(defvar.ps+ *life-decrease-event* (make-hash-table))

(defun.ps+ add-life-decrease-event (name func)
  (setf (gethash name *life-decrease-event*) func))

;; --- entities for visual --- ;;

;; TODO: Fix blink when updating.
;; This is because a frame where existing models are deleted and
;; a frame where where new models are added are different.
;; TODO: Adjust display position.
(defun.ps+ update-display-of-life (life-entity rest-life)
  (check-entity-tags life-entity :life)
  (let ((r (get-param :ball :r)))
    (delete-ecs-component-type 'model-2d life-entity)
    (dotimes (i rest-life)
      (frame-promise-then
       (make-texture-model-promise
        :width (* 2 r) :height (* 2 r)
        :texture-name "ball")
       (lambda (model)
         (let ((model-cmp (make-model-2d
                           :model model
                           :offset (make-point-2d :x (* 2 r i))
                           :depth (get-param :ball :depth))))
           (add-ecs-component-list
            life-entity
            model-cmp)))))))

(defun.ps+ init-life-entity (field init-life)
  (let ((life (make-ecs-entity)))
    (add-entity-tag life :life)
    (add-ecs-component-list
     life
     (make-point-2d))
    (update-display-of-life life init-life)
    (add-life-decrease-event
     :update-life-display
     (lambda (rest-life)
       (update-display-of-life life rest-life)))
    (add-ecs-entity life field)))

;; --- others --- ;;

(defvar.ps+ *rest-life* -1)
(defvar.ps+ *init-life* 2)

(defun.ps+ init-life (field)
  (setf *rest-life* *init-life*)
  (init-life-entity field *init-life*)
  (add-ball-falling-event
   :decrease-life
   (lambda (ball)
     (declare (ignore ball))
     (decf *rest-life*)
     (maphash (lambda (name func)
                (declare (ignore name))
                (funcall func *rest-life*))
              *life-decrease-event*))))

(defun.ps+ get-rest-life ()
  *rest-life*)
