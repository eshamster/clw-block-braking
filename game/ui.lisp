(defpackage clw-block-braking/game/ui
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-ui-system
           :ui-component
           :make-ui-component
           :ui-component-on-click))
(in-package :clw-block-braking/game/ui)

(defstruct.ps+ (ui-component (:include ecs-component))
    on-click)

(defvar.ps+ *current-target* nil)

(defstruct.ps+
    (ui-system
     (:include
      ecs-system
      (target-component-types '(point-2d ui-component physic-2d))
      (process
       (lambda (entity)
         (with-ecs-components (point-2d physic-2d ui-component) entity
           (when (ui-component-on-click ui-component)
             (let* ((mouse-pnt (make-point-2d :x (get-mouse-x)
                                              :y (get-mouse-y)))
                    (mouse-physic (make-physic-circle :r 0))
                    (collide-p (collide-physics-p physic-2d point-2d
                                                  mouse-physic mouse-pnt)))
               (case (get-left-mouse-state)
                 (:down-now
                  (when collide-p
                    (setf *current-target* entity)))
                 (:up-now
                  (when (and collide-p
                             (eq *current-target* entity)
                             (find-the-entity entity))
                    (let ((on-click (ui-component-on-click ui-component)))
                      (when on-click
                        (funcall on-click nil))))
                  (setf *current-target* nil)))))))))))

(defun.ps+ init-ui-system ()
  (make-ui-system))
