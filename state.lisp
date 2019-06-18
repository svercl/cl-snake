(in-package #:snake)

(defclass state () ())

(defgeneric update (state)
  (:method (state) (declare (ignore state))))
(defgeneric draw (state)
  (:method (state) (declare (ignore state))))
(defgeneric handle-key (state key)
  (:method (state key) (declare (ignore state key))))

(defclass main-menu-state (state)
  ((start-callback :initarg :start)))

(defmethod handle-key ((this main-menu-state) key)
  (with-slots (start-callback) this
    (when (eq key :space)
      (funcall start-callback))))

(defmethod draw ((this main-menu-state))
  (declare (ignore this))
  ;; TODO(bsvercl): Center this?
  (gamekit:draw-text "Welcome to SNAKE"
                     (gamekit:vec2 (/ +screen-width+ 2)
                                   (/ +screen-height+ 2)))
  (gamekit:draw-text "Press SPACE to play"
                     (gamekit:add (gamekit:vec2 (/ +screen-width+ 2)
                                                (/ +screen-height+ 2))
                                  (gamekit:vec2 0 -20))))

(defclass game-state (state)
  ((player :reader player-of)
   (food-pos :reader food-pos-of)
   (score :initform 0 :reader score-of)
   (paused :initform nil)
   (end-callback :initarg :end)
   (powerups :initform nil :reader %powerups-of))
  (:documentation "This is where we play."))

(defun new-food-pos ()
  "Generates a new spot on the grid."
  (random-vec +segments-across-width+ +segments-across-height+))

(defmethod initialize-instance :after ((this game-state) &key)
  (with-slots (player food-pos) this
    (setf food-pos (new-food-pos))
    ;; TODO(bsvercl): We should really have a CENTERED function or something.
    (setf player (make-snake (gamekit:vec2 (/ +segments-across-width+ 2)
                                           (/ +segments-across-height+ 2))))))

(defmethod update ((this game-state))
  (with-slots (player food-pos score end-callback) this
    (let* ((pos (snake-position player))
           (ate-food-p (bodge-math:vec= food-pos pos)))
      (advance player ate-food-p)
      (when ate-food-p
        (setf food-pos (new-food-pos))
        (incf score 50))
      (when (hit-itself player)
        (funcall end-callback)))))

(defmethod draw ((this game-state))
  ;; TODO(bsvercl): Drop into CL-BODGE to speed this up,
  ;; or just remove this completely?
  (loop for x from 0 below +segments-across-width+
        do (loop for y from 0 below +segments-across-height+
                 do (draw-segment (gamekit:vec2 x y)
                                  :fill-paint +transparent+
                                  :stroke-paint +grid-color+)))
  ;; Draw food
  (draw-segment (food-pos-of this) :fill-paint +food-color+)
  ;; Draw SNAKE
  (let* ((player (player-of this))
         (segments (segments-of player))
         (color (color-of player)))
    (dolist (pos segments)
      (draw-segment pos :fill-paint color)))
  ;; Draw SCORE
  (gamekit:draw-text (format nil "Score: ~A" (score-of this))
                     (gamekit:vec2 25 (- +screen-height+ 25))))

(defmethod handle-key ((this game-state) key)
  (with-slots (player food-pos) this
    (case key
      (:w (change-direction player :up))
      (:s (change-direction player :down))
      (:a (change-direction player :left))
      (:d (change-direction player :right))
      ;; NOTE(bsvercl): These are for debugging.
      (:space (change-direction player :nothing))
      (:q (setf food-pos (new-food-pos))))))

(defclass game-over-state (state)
  ((restart-callback :initarg :restart)))

(defmethod draw ((this game-over-state))
  (declare (ignore this))
  (gamekit:draw-text "Good job, dummy! You ate yourself."
                     (gamekit:vec2 (/ +screen-width+ 2)
                                   (/ +screen-height+ 2)))
  (gamekit:draw-text "Press SPACE to restart."
                     (gamekit:subt (gamekit:vec2 (/ +screen-width+ 2)
                                                 (/ +screen-height+ 2))
                                   (gamekit:vec2 0 20))))

(defmethod handle-key ((this game-over-state) key)
  (with-slots (restart-callback) this
    (when (eq key :space)
      (funcall restart-callback))))
