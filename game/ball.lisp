(defpackage clw-block-braking/game/ball
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-ball
           :reset-ball
           :stop-ball
           :add-ball-falling-event
           :shoot-ball
           :ball-shot-p)
  (:import-from :clw-block-braking/game/parameter
                :get-param)
  (:import-from :clw-block-braking/game/field
                :get-field
                :field-width
                :field-height)
  (:import-from :clw-block-braking/game/paddle
                :get-paddle-global-pnt
                :add-paddle-move-event))
(in-package :clw-block-braking/game/ball)

;; --- gravity --- ;;

(defun.ps+ incf-gravity (gravity-point target-point target-vector)
  (let ((pre-abs (vector-abs target-vector))
        (accell (setf-vector-abs
                 (decf-vector (clone-vector-2d gravity-point)
                              target-point)
                 (get-param :gravity :value))))
    (incf-vector target-vector accell)
    (setf-vector-abs target-vector pre-abs)))

(defun.ps+ find-gravity-entity ()
  (find-a-entity-by-tag :gravity))

(defun.ps+ add-gravity-effect (ball gravity-entity)
  (assert gravity-entity)
  (let ((grav-point (calc-global-point gravity-entity))
        (ball-point (calc-global-point ball)))
    (when (< (calc-dist grav-point ball-point)
             (get-param :gravity :range))
      (incf-gravity grav-point ball-point
                    (get-entity-param ball :speed)))))

;; --- ball --- ;;

;; utils

(defun.ps+ ball-shot-p (ball)
  (check-entity-tags ball :ball)
  (not (get-entity-param ball :on-paddle-p)))

;; others

(defvar.ps+ *ball-falling-event* (make-hash-table))

(defun.ps+ add-ball-falling-event (name func)
  (setf (gethash name *ball-falling-event*) func))

(defun.ps+ reflect-by-vertical (ball)
  (check-entity-tags ball :ball)
  (let* ((speed (get-entity-param ball :speed))
         (angle (vector-angle speed)))
    (setf-vector-angle speed (- PI angle))))

(defun.ps+ reflect-by-horizontal (ball)
  (check-entity-tags ball :ball)
  (let* ((speed (get-entity-param ball :speed))
         (angle (vector-angle speed)))
    (setf-vector-angle speed (* angle -1))))

(defun.ps+ move-ball-normally (ball)
  (check-entity-tags ball :ball)
  (let* ((speed (get-entity-param ball :speed))
         (r (get-entity-param ball :r)))
    (with-ecs-components ((point point-2d)) ball
      (with-slots (x y) point
        (incf x (vector-2d-x speed))
        (incf y (vector-2d-y speed))
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

(defun.ps+ calc-current-base-speed-abs (ball)
  (check-entity-tags ball :ball)
  (let ((lane (get-entity-param (get-entity-param ball :paddle)
                                :lane)))
    (* (lerp-scalar (get-param :ball :speed :base :min)
                    (get-param :ball :speed :base :max)
                    (/ lane (- (get-param :paddle :lane-count) 1.0)))
       (get-entity-param ball :speed-scale))))

(defun.ps+ shoot-ball (ball)
  (unless (ball-shot-p ball)
    (set-entity-param ball :on-paddle-p nil)
    (let ((speed (get-entity-param ball :speed))
          (speed-abs (calc-current-base-speed-abs ball)))
      (setf-vector-abs speed speed-abs)
      (setf-vector-angle speed (/ PI 3.9))
      (set-entity-param ball :basic-speed-abs speed-abs))))

(defun.ps+ move-ball (ball)
  (check-entity-tags ball :ball)
  (when (ball-shot-p ball)
    (move-ball-normally ball)))

(defun.ps+ update-speed (ball)
  (check-entity-tags ball :ball)
  (let* ((pre-base-speed-abs (get-entity-param ball :basic-speed-abs))
         (new-base-speed-abs (calc-current-base-speed-abs ball))
         (speed (get-entity-param ball :speed)))
    (set-entity-param ball :basic-speed-abs new-base-speed-abs)
    (setf-vector-abs speed
                     (+ (vector-abs speed)
                        (- new-base-speed-abs pre-base-speed-abs)))
    (let ((grav-entity (find-gravity-entity)))
      (when grav-entity
        (add-gravity-effect ball grav-entity)))))

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
  (let ((block-pnt (calc-global-point block))
        (block-width (get-entity-param block :width))
        (block-height (get-entity-param block :height)))
    (ecase (calc-col-direction ball block-pnt block-width block-height)
      ((:from-left :from-right) (reflect-by-vertical ball))
      ((:from-top :from-bottom) (reflect-by-horizontal ball)))))

;; Note: This assumes that it will be called after reflection procedure.
(defun.ps+ adjust-angle-by-paddle (ball paddle)
  (let* ((paddle-width (get-entity-param paddle :width))
         (paddle-pnt (get-paddle-global-pnt paddle))
         (ball-pnt (calc-global-point ball))
         (diff-x (- (point-2d-x ball-pnt)
                    (+ (point-2d-x paddle-pnt) (/ paddle-width 2))))
         (changed-angle (* (get-param :ball :angle :max-accele)
                           (max -1
                                (min 1 (/ diff-x (/ paddle-width 2))))))
         (min-angle-abs (get-param :ball :angle :min))
         ;; current
         (speed (get-entity-param ball :speed))
         (current-angle (vector-angle speed)))
    (setf-vector-angle speed
                       (max min-angle-abs
                            (min (- PI min-angle-abs)
                                 ;; diff-angle is used for normalization
                                 (diff-angle
                                  (- current-angle changed-angle)
                                  0))))))

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
       (reflect-by-horizontal ball)))
    (adjust-angle-by-paddle ball paddle)))

;; To avoid to sink into target
(defun.ps+ reverse-ball-to-not-collide-point (ball target pre-point)
  "When the ball collides to the target, reverse the ball to point where they don't collide.
Return reversed distance."
  (check-entity-tags ball :ball)
  (let* ((point (get-ecs-component 'point-2d ball))
         (not-adjusted-point (clone-point-2d point))
         (min-point (clone-point-2d pre-point))
         (max-point (clone-point-2d point))
         (repeat-times 6))
    ;; Reverse ball to reflection point
    (lerp-vector-2d min-point max-point 0.5
                    point)
    (dotimes (i repeat-times)
      (if (collide-entities-p ball target)
          (copy-point-2d-to max-point point)
          (copy-point-2d-to min-point point))
      (lerp-vector-2d min-point max-point 0.5
                      point))
    ;; Calculate reversed distance
    (calc-dist point not-adjusted-point)))

(defun.ps+ process-collide (ball target)
  (check-entity-tags ball :ball)
  (let ((rest-dist (reverse-ball-to-not-collide-point
                    ball target (get-entity-param ball :pre-point))))
    (cond ((or (has-entity-tag target :block)
               (has-entity-tag target :wall))
           (unless (get-entity-param ball :col-to-block-p)
             (reflect-to-block ball target)
             (set-entity-param ball :col-to-block-p t))
           (when (has-entity-tag target :block)
             (let ((speed-scale (+ (get-entity-param ball :speed-scale)
                                   (get-param :ball :speed :accell-scale :per-block))))
               (set-entity-param ball :speed-scale
                                 (min speed-scale (get-param :ball :speed :accell-scale :max))))))
          ((has-entity-tag target :paddle)
           (when (get-entity-param ball :enable-col-to-paddle-p)
             (reflect-to-paddle ball target)
             (set-entity-param ball :enable-col-to-paddle-p nil)))
          (t (error "Collides to unknown object.")))
    ;; proceed rest distance
    (let ((point (get-ecs-component 'point-2d ball))
          (angle (vector-angle (get-entity-param ball :speed))))
      (incf (point-2d-x point) (* rest-dist (cos angle)))
      (incf (point-2d-y point) (* rest-dist (sin angle))))))

(defun.ps+ ball-is-above-paddle-p (ball paddle)
  (> (- (vector-2d-y (calc-global-point ball))
        (get-entity-param ball :r))
     (+ (vector-2d-y (get-paddle-global-pnt paddle))
        (get-entity-param paddle :height))))

(defun.ps+ make-ball (field paddle)
  (check-entity-tags field :field)
  (let ((ball (make-ecs-entity))
        (width (field-width field))
        (r (get-param :ball :r)))
    (add-entity-tag ball :ball)
    (add-ecs-component-list
     ball
     (make-point-2d :x (/ width 2) :y 50)
     (make-script-2d :func (lambda (entity)
                             (set-entity-param entity :col-to-block-p nil)
                             (when (ball-is-above-paddle-p entity paddle)
                               (set-entity-param entity :enable-col-to-paddle-p t))
                             (set-entity-param entity :pre-point
                                               (clone-point-2d (get-ecs-component 'point-2d entity)))
                             (move-ball entity)
                             (update-speed entity)))
     (make-physic-circle :target-tags '(:block :paddle :wall)
                         :r r
                         :on-collision #'process-collide)
     (init-entity-params :speed (make-vector-2d)
                         :basic-speed-abs 0
                         :speed-scale 1
                         :col-to-block-p nil ; reflect once per frame at most
                         :enable-col-to-paddle-p t
                         :on-paddle-p t
                         :paddle paddle
                         :field field
                         :fallen-p nil
                         :r r))
    (frame-promise-then
     (make-texture-model-promise
      :width (* 2 r) :height (* 2 r)
      :texture-name "ball")
     (lambda (model)
       (add-ecs-component-list
        ball
        (make-model-2d :model model
                       :offset (make-point-2d :x (* -1 r) :y (* -1 r))
                       :depth (get-param :ball :depth)))))
    (add-paddle-move-event paddle
                           :move-on-paddle
                           (lambda (_)
                             (declare (ignore _))
                             (when (get-entity-param ball :on-paddle-p)
                               (move-ball-on-paddle ball))))
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

;; Note: There is not chance to restart under the current implementation.
(defun.ps+ stop-ball (ball)
  (delete-ecs-component-type 'script-2d ball))
