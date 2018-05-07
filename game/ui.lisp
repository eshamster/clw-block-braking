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

(defstruct.ps+
    (ui-system
     (:include
      ecs-system
      (target-component-types '(point-2d ui-component physic-2d))
      (process
       (lambda (entity)
         (with-ecs-components (point-2d physic-2d ui-component) entity
           (when (ui-component-on-click ui-component)
             (let ((mouse-pnt (make-point-2d :x (get-mouse-x)
                                             :y (get-mouse-y)))
                   (mouse-physic (make-physic-circle :r 0)))
               (when (collide-physics-p physic-2d point-2d
                                        mouse-physic mouse-pnt)
                 (add-to-monitoring-log (+ "Hover on UI: " (ecs-entity-id entity)))
                 (when (eq (get-left-mouse-state) :up-now)
                   (let ((on-click (ui-component-on-click ui-component)))
                     (when on-click
                       (funcall on-click nil)))))))))))))

(defun.ps+ init-ui-system ()
  (make-ui-system))
