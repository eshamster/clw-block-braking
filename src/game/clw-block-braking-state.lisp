(defpackage clw-block-braking/src/game/clw-block-braking-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-clw-block-braking-start-state)
  (:import-from :clw-block-braking/src/game/field
                :init-field
                :get-field)
  (:import-from :clw-block-braking/src/game/ball
                :make-ball
                :reset-ball)
  (:import-from :clw-block-braking/src/game/block
                :make-test-blocks)
  (:import-from :clw-block-braking/src/game/controller
                :init-controller)
  (:import-from :clw-block-braking/src/game/life
                :add-life-decrease-event
                :get-rest-life
                :init-life)
  (:import-from :clw-block-braking/src/game/paddle
                :make-paddle))
(in-package :clw-block-braking/src/game/clw-block-braking-state)

(defstruct.ps+
    (clw-block-braking-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (init-field)
                  (let*  ((field (get-field))
                          (paddle (make-paddle field))
                          (ball (make-ball field paddle)))
                    (add-ecs-entity-to-buffer paddle field)
                    (add-ecs-entity-to-buffer ball field)
                    (make-test-blocks field)
                    (init-controller)
                    ;; life
                    (init-life)
                    (add-life-decrease-event
                     :reset-or-gameover
                     (lambda (rest-life)
                       (if (>= rest-life 0)
                           (let ((fallen-ball (find-a-entity-by-tag :ball)))
                             (assert fallen-ball)
                             (reset-ball fallen-ball))
                           (setf (clw-block-braking-main-state-gameover-p _this) t)))))
                  t))
               (process
                (lambda (_this)
                  (add-to-monitoring-log (+ "Life: " (get-rest-life)))
                  (if (clw-block-braking-main-state-gameover-p _this)
                      (make-clw-block-braking-gameover-state)
                      nil)))))
    (gameover-p nil))

(defstruct.ps+
    (clw-block-braking-start-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  ;; TODO: Prevent multiple load
                  (load-font "js/")
                  (setf-collider-model-enable t)
                  (let* ((font-size 25)
                         (margin 20)
                         (area (make-text-area :font-size font-size :text-align :center
                                               :margin margin
                                               :x (/ (get-screen-width) 2)
                                               :y (+ (/ (get-screen-height) 2)
                                                     (+ (* font-size 2) margin)))))
                    (add-text-to-area area
                                      :text "Click to start"
                                      :color #x00ffff)
                    (add-ecs-entity area))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (when (eq (get-left-mouse-state) :down-now)
                    (make-clw-block-braking-main-state))))
               (end-process
                (lambda (_this)
                  (declare (ignore _this))
                  t)))))

(defstruct.ps+
    (clw-block-braking-gameover-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (let* ((font-size 25)
                         (margin 20)
                         (area (make-text-area :font-size font-size :text-align :center
                                               :margin margin
                                               :x (/ (get-screen-width) 2)
                                               :y (+ (/ (get-screen-height) 2)
                                                     (+ (* font-size 2) margin)))))
                    (add-text-to-area area :text "GAME OVER!!" :color #xff0000)
                    (add-text-to-area area
                                      :text "Click to return menu"
                                      :color #x00ffff)
                    (add-ecs-entity area))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (when (eq (get-left-mouse-state) :down-now)
                    (make-clw-block-braking-start-state))))
               (end-process
                (lambda (_this)
                  (with-slots (first-frame) _this
                    (if first-frame
                        (progn (do-ecs-entities entity
                                 (unless (ecs-entity-parent entity)
                                   (register-next-frame-func
                                    (lambda () (delete-ecs-entity entity)))))
                               (setf first-frame nil)
                               nil)
                        t))))))
    (first-frame t))
