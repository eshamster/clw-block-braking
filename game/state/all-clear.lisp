(defpackage clw-block-braking/game/state/all-clear
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-game-all-clear-state)
  (:import-from :clw-block-braking/game/state/utils
                :make-state
                :def-game-state
                :delete-all-entities-in-next-frame)
  (:import-from :clw-block-braking/game/field
                :get-field
                :field-height)
  (:import-from :clw-block-braking/game/score-register
                :get-score
                :get-total-score
                :score-time
                :register-score
                :update-best-record-p
                :get-best-score)
  (:import-from :clw-block-braking/game/stage-generator
                :get-max-stage-number)
  (:import-from :clw-block-braking/game/stage-manager
                :get-selected-stage-list))
(in-package :clw-block-braking/game/state/all-clear)

(def-game-state all-clear ((first-frame t))
  :start-process
  (lambda (_this)
    (declare (ignore _this))
    (let ((field (get-field)))
      (let* ((font-size 25)
             (margin 20)
             (area (make-text-area :font-size font-size :text-align :left
                                   :margin margin
                                   :x font-size
                                   :y (- (field-height field) font-size))))
        (add-text-to-area area
                          :text "ALL STAGE CLEAR!!"
                          :color #xff0088)
        (dolist (text '("Click for" "returning to menu"))
          (add-text-to-area area
                            :text text
                            :color #x00ffff))
        (add-ecs-entity area field))
      (let* ((font-size 15)
             (margin 10)
             (stage-count (get-max-stage-number))
             (area (make-text-area :font-size font-size :text-align :left
                                   :margin margin
                                   :x font-size
                                   :y (* (+ stage-count 5) (+ font-size margin)))))
        ;; XXX: Adding text is invalid as CL code
        (let ((stage-list (get-selected-stage-list)))
          (flet ((add-score-text (stage text)
                   ;; TODO: Display "New!!" more drastically!
                   (add-text-to-area area
                                     :text (+ text " (Best: " (get-best-score stage)
                                              (if (update-best-record-p stage)
                                                  ") New!!"
                                                  ")"))
                                     :color #xff8800)))
            (dolist (stage stage-list)
              (let ((score (get-score stage)))
                (add-score-text stage (+ "Stage" stage ": " score))))
            (when (> (length stage-list) 1)
              (let ((total-score (get-total-score)))
                (register-score :stage :all
                                :time total-score)
                (add-text-to-area area
                                  :text "------"
                                  :color #xff0088)
                (add-score-text :all (+ "TOTAL: " total-score))))))
        (add-ecs-entity area field)))
    t)

  :process
  (lambda (_this)
    (declare (ignore _this))
    (when (eq (get-left-mouse-state) :down-now)
      (make-state :menu)))

  :end-process
  (lambda (_this)
    (with-slots (first-frame) _this
      (if first-frame
          (progn (delete-all-entities-in-next-frame)
                 (setf first-frame nil)
                 nil)
          t))))

