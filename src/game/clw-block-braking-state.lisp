(defpackage clw-block-braking/src/game/clw-block-braking-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-menu-state)
  (:import-from :clw-block-braking/src/game/field
                :init-field
                :get-field)
  (:import-from :clw-block-braking/src/game/ball
                :make-ball
                :reset-ball
                :stop-ball)
  (:import-from :clw-block-braking/src/game/stage-generator
                :generate-stage)
  (:import-from :clw-block-braking/src/game/controller
                :init-controller)
  (:import-from :clw-block-braking/src/game/life
                :add-life-decrease-event
                :get-rest-life
                :init-life)
  (:import-from :clw-block-braking/src/game/paddle
                :make-paddle)
  (:import-from :clw-block-braking/src/game/wall
                :init-wall))
(in-package :clw-block-braking/src/game/clw-block-braking-state)

(defun.ps+ get-current-ball ()
  (let ((current-ball (find-a-entity-by-tag :ball)))
    (assert current-ball)
    current-ball))

(defun.ps+ reset-ball-on-field ()
  (reset-ball (get-current-ball)))

(defun.ps+ stage-cleared-p ()
  (not (find-a-entity-by-tag :block)))

(defstruct.ps+
    (game-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (let*  ((field (get-field)))
                    (generate-stage (game-main-state-stage-number _this)
                                    field))
                  (add-life-decrease-event
                   :reset-or-gameover
                   (lambda (rest-life)
                     (if (>= rest-life 0)
                         (reset-ball-on-field)
                         (setf (game-main-state-gameover-p _this) t))))
                  t))
               (process
                (lambda (_this)
                  (add-to-monitoring-log (+ "Life: " (get-rest-life)))
                  (cond ((stage-cleared-p)
                         (make-game-interval-state
                          :next-stage-number
                          (1+ (game-main-state-stage-number _this))))
                        ((game-main-state-gameover-p _this)
                         (make-game-gameover-state))
                        (t nil))))))
    stage-number
    (gameover-p nil))

(defstruct.ps+
    (game-interval-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (stop-ball (get-current-ball))
                  (let* ((parent (game-interval-state-parent-entity _this))
                         (font-size 25)
                         (margin 20)
                         (area (make-text-area :font-size font-size :text-align :center
                                               :margin margin
                                               :x (/ (get-screen-width) 2)
                                               :y (+ (/ (get-screen-height) 2)
                                                     (+ (* font-size 2) margin)))))
                    (add-text-to-area area
                                      :text "Stage Clear!!"
                                      :color #x00ffff)
                    (add-text-to-area area
                                      :text "Click for next stage!"
                                      :color #x00ffff)
                    (add-ecs-entity parent)
                    (add-ecs-entity area parent))
                  t))
               (process
                (lambda (_this)
                  (when (eq (get-left-mouse-state) :down-now)
                    (make-game-main-state
                     :stage-number (game-interval-state-next-stage-number _this)))))
               (end-process
                (lambda (_this)
                  (reset-ball-on-field)
                  (delete-ecs-entity
                   (game-interval-state-parent-entity _this))
                  t))))
    (parent-entity (make-ecs-entity))
    next-stage-number)

(defstruct.ps+
    (game-init-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (init-field)
                  (init-controller)
                  (init-life)
                  (let*  ((field (get-field))
                          (paddle (make-paddle field))
                          (ball (make-ball field paddle)))
                    (add-ecs-entity-to-buffer paddle field)
                    (add-ecs-entity-to-buffer ball field)
                    (init-wall field))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (make-game-main-state
                   :stage-number 1))))))

(defstruct.ps+
    (game-menu-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  ;; TODO: Prevent multiple load
                  (load-font "js/")
                  (load-texture :name "block"
                                :path "/images/block.png"
                                :alpha-path "/images/block_alpha.png")
                  (setf-collider-model-enable nil)
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
                    (make-game-init-state))))
               (end-process
                (lambda (_this)
                  (declare (ignore _this))
                  t)))))

(defstruct.ps+
    (game-gameover-state
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
                    (make-game-menu-state))))
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
