(defpackage clw-block-braking/src/game/ball
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-ball
           :reset-ball
           :add-ball-falling-event
           :shoot-ball)
  (:import-from :clw-block-braking/src/game/parameter
                :get-param)
  (:import-from :clw-block-braking/src/game/field
                :get-field
                :field-width
                :field-height)
  (:import-from :clw-block-braking/src/game/paddle
                :get-paddle-global-pnt))
(in-package :clw-block-braking/src/game/ball)

(defvar.ps+ *ball-falling-event* (make-hash-table))

(defun.ps+ add-ball-falling-event (name func)
  (setf (gethash name *ball-falling-event*) func))

;; TODO: correct not to sink into
(defun.ps+ reflect-by-vertical (ball)
  (check-entity-tags ball :ball)
  (set-entity-param ball :angle
                    (- PI (get-entity-param ball :angle))))

(defun.ps+ reflect-by-horizontal (ball)
  (check-entity-tags ball :ball)
  (set-entity-param ball :angle
                    (* (get-entity-param ball :angle) -1)))

(defun.ps+ move-ball-normally (ball)
  (check-entity-tags ball :ball)
  (let* ((field (get-field))
         (width (field-width field))
         (height (field-height field))
         (speed (get-entity-param ball :speed))
         (angle (get-entity-param ball :angle))
         (r (get-entity-param ball :r)))
    (with-ecs-components ((point point-2d)) ball
      (with-slots (x y) point
        (incf x (* speed (cos angle)))
        (incf y (* speed (sin angle)))
        ;; reflect
        (when (> (+ x r) width)
          (reflect-by-vertical ball))
        (when (< (- x r) 0)
          (reflect-by-vertical ball))
        (when (> (+ y r) height)
          (reflect-by-horizontal ball))
        ;; fall
        (when (and (< (+ y r) 0)
                   (not (get-entity-param ball :fallen-p)))
          (set-entity-param ball :fallen-p t)
          (maphash (lambda (name func)
                     (declare (ignore name))
                     (funcall func ball))
                   *ball-falling-event*))))))

(defun.ps+ move-ball-on-paddle (ball)
  (check-entity-tags ball :ball)
  (let* ((paddle (get-entity-param ball :paddle))
         (paddle-pnt (get-paddle-global-pnt paddle)))
    (with-ecs-components ((pnt point-2d)) ball
      (let* ((global-pnt (make-point-2d
                          :x (+ (point-2d-x paddle-pnt)
                                (/ (get-entity-param paddle :width) 2))
                          :y (+ (point-2d-y paddle-pnt)
                                (get-entity-param paddle :height)
                                (get-entity-param ball :r)
                                (get-param :ball :dist-from-paddle))))
             (local-pnt (calc-local-point ball global-pnt)))
        (setf (point-2d-x pnt) (point-2d-x local-pnt))
        (setf (point-2d-y pnt) (point-2d-y local-pnt))))))

(defun.ps+ shoot-ball (ball)
  (set-entity-param ball :on-paddle-p nil))

(defun.ps+ move-ball (ball)
  (check-entity-tags ball :ball)
  (if (get-entity-param ball :on-paddle-p)
      (move-ball-on-paddle ball)
      (move-ball-normally ball)))

;; The rect-pnt is left-bottom point of the rect.
(defun.ps+ calc-col-direction (ball rect-global-pnt rect-width rect-height)
  (check-entity-tags ball :ball)
  (let* ((ball-pnt (calc-global-point ball))
         (ball-x (vector-2d-x ball-pnt))
         (ball-y (vector-2d-y ball-pnt))
         (rect-x (vector-2d-x rect-global-pnt))
         (rect-y (vector-2d-y rect-global-pnt))
         ;; opz = outer product z
         (opz1 (calc-outer-product-z
                (make-vector-2d :x rect-width :y rect-height)
                (make-vector-2d :x (- ball-x rect-x) :y (- ball-y rect-y))))
         (opz2 (calc-outer-product-z
                (make-vector-2d :x rect-width :y (* -1 rect-height))
                (make-vector-2d :x (- ball-x rect-x)
                                :y (- ball-y (+ rect-y rect-height))))))
    (cond ((>= (* opz1 opz2) 0)
           (if (>= opz1 0)
               :from-top
               :from-bottom))
          (t (if (>= opz1 0)
                 :from-left
                 :from-right)))))

(defun.ps+ reflect-to-block (ball block)
  (check-entity-tags ball :ball)
  (check-entity-tags block :block)
  (let ((block-pnt (calc-global-point block))
        (block-width (get-entity-param block :width))
        (block-height (get-entity-param block :height)))
    (ecase (calc-col-direction ball block-pnt block-width block-height)
      ((:from-left :from-right) (reflect-by-vertical ball))
      ((:from-top :from-bottom) (reflect-by-horizontal ball)))))

;; TODO: Adjust angle according to the collision point
(defun.ps+ reflect-to-paddle (ball paddle)
  (check-entity-tags ball :ball)
  (check-entity-tags paddle :paddle)
  (let* ((width (get-entity-param paddle :width))
         (height (get-entity-param paddle :height))
         (paddle-pnt (get-paddle-global-pnt paddle)))
    (ecase (calc-col-direction ball paddle-pnt width height)
      ((:from-left :from-right)
       (reflect-by-vertical ball)
       (reflect-by-horizontal ball))
      ((:from-top :from-bottom)
       (reflect-by-horizontal ball)))))

(defun.ps+ process-collide (ball target)
  (check-entity-tags ball :ball)
  (cond ((has-entity-tag target :block)
         (unless (get-entity-param ball :col-to-block-p)
           (reflect-to-block ball target)
           (set-entity-param ball :col-to-block-p t)))
        ((has-entity-tag target :paddle)
         (when (get-entity-param ball :enable-col-to-paddle-p)
           (reflect-to-paddle ball target)
           (set-entity-param ball :enable-col-to-paddle-p nil)))
        (t (error "Collides to unknown object."))))

(defun.ps+ make-ball (field paddle)
  (check-entity-tags field :field)
  (let ((ball (make-ecs-entity))
        (width (field-width field))
        (r (get-param :ball :r)))
    (add-entity-tag ball :ball)
    (add-ecs-component-list
     ball
     (make-point-2d :x (/ width 2) :y 50)
     (make-model-2d :model (make-solid-circle :r r :color (get-param :ball :color)))
     (make-script-2d :func (lambda (entity)
                             (set-entity-param entity :col-to-block-p nil)
                             (when (> (- (vector-2d-y (calc-global-point entity))
                                         (get-entity-param entity :r))
                                      (+ (vector-2d-y (get-paddle-global-pnt paddle))
                                         (get-entity-param paddle :height)))
                               (set-entity-param entity :enable-col-to-paddle-p t))
                             (move-ball entity)))
     (make-physic-circle :target-tags '(:block :paddle)
                         :r r
                         :on-collision #'process-collide)
     (init-entity-params :speed (get-param :ball :speed :init)
                         :angle (/ PI 3.9)
                         :col-to-block-p nil ; reflect once per frame at most
                         :enable-col-to-paddle-p t
                         :on-paddle-p t
                         :paddle paddle
                         :field field
                         :fallen-p nil
                         :r r))
    ball))

(defun.ps+ reset-ball (ball)
  (check-entity-tags ball :ball)
  (let ((field (get-entity-param ball :field))
        (paddle (get-entity-param ball :paddle)))
    (register-next-frame-func
     (lambda ()
       (delete-ecs-entity ball)
       (add-ecs-entity (make-ball field paddle)
                       field)))))
