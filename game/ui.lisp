(defpackage clw-block-braking/game/ui
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-ui-system
           :ui-component
           :make-ui-component
           :ui-component-on-click-up))
(in-package :clw-block-braking/game/ui)

(defstruct.ps+ (ui-component (:include ecs-component))
    on-click-up
  on-hover
  on-not-hover)

(defvar.ps+ *current-target* nil)

(defstruct.ps+
    (ui-system
     (:include
      ecs-system
      (target-component-types '(point-2d ui-component physic-2d))
      (process
       (lambda (entity)
         (with-ecs-components (physic-2d ui-component) entity
           (let* ((mouse-pnt (make-point-2d :x (get-mouse-x)
                                            :y (get-mouse-y)))
                  (mouse-physic (make-physic-circle :r 0))
                  (collide-p (collide-physics-p physic-2d (calc-global-point entity)
                                                mouse-physic mouse-pnt)))
             ;; hover
             (with-slots (on-hover on-not-hover) ui-component
               (if collide-p
                   (when on-hover
                     (funcall on-hover nil))
                   (when on-not-hover
                     (funcall on-not-hover nil))))
             ;; click
             (case (get-left-mouse-state)
               (:down-now
                (when collide-p
                  (setf *current-target* entity)))
               (:up-now
                (when (and collide-p
                           (eq *current-target* entity)
                           (find-the-entity entity))
                  (let ((on-click-up (ui-component-on-click-up ui-component)))
                    (when on-click-up
                      (funcall on-click-up nil))))
                (setf *current-target* nil))))))))))

(defun.ps+ init-ui-system ()
  (make-ui-system))
