;;;; snake.lisp

(in-package #:snake)

(defparameter +segment-size+ 25.0)
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)
(defparameter +snake-color+ (gamekit:vec4 1.0 0.75 0.5 1.0))
(defparameter +food-color+ (gamekit:vec4 0.5 0.25 1.0 1.0))

;; TODO(bsvercl): Use this for wrapping snake position.
(defgeneric mod-vec (vec divisor))
(defmethod mod-vec ((vec gamekit:vec2) divisor)
  (gamekit:vec2 (mod (gamekit:x vec) divisor)
                (mod (gamekit:y vec) divisor)))

(defclass snake ()
  ((segments :initarg :segments :accessor segments-of)
   (direction :initarg :direction :accessor direction-of))
  (:documentation "The moving thing, usually user controlled."))

(defun make-snake (starting-position &optional (direction (gamekit:vec2)))
  "Creates a SNAKE with STARTING-POSITION and DIRECTION."
  (let ((segments (make-array 1 :element-type 'gamekit:vec2
                                :initial-element starting-position
                                :adjustable t
                                :fill-pointer t)))
    (make-instance 'snake :segments segments :direction direction)))

(defun snake-head (snake)
  (aref (segments-of snake) 0))

(defun snake-position (snake)
  (snake-head snake))

(defmethod (setf snake-position) (pos snake)
  (with-slots (segments) snake
    (setf (aref segments 0) pos)))

(defun snake-positions (snake)
  (loop for segment across (segments-of snake)
        collect segment))

(defun change-direction (snake direction)
  (setf (direction-of snake) direction))

(gamekit:defgame snake-game ()
  ((player :type snake :accessor player-of)
   (food-pos :initform nil :accessor food-pos-of))
  (:viewport-title "Snake")
  (:viewport-width +screen-width+)
  (:viewport-height +screen-height+))

(defun new-food-pos ()
  (flet ((random-location (size)
           (* (floor (/ (random size) +segment-size+)) +segment-size+)))
    (gamekit:vec2 (random-location +screen-width+)
                  (random-location +screen-height+))))

(defmethod gamekit:post-initialize ((this snake-game))
  (with-slots (player food-pos) this
    (setf player (make-snake (gamekit:vec2)))
    ;; TODO(bsvercl): avoid selecting a spot occupied by the player.
    (setf food-pos (new-food-pos))
    ;; TODO(bsvercl): ??? I think we can go deeper.
    (macrolet ((binder (keycode &body body)
                 `(gamekit:bind-button ,keycode :pressed
                                       (lambda () ,@body))))
      ;; TODO(bsvercl): Something like this. Although minor, it would be kind of nice.
      ;; (loop for (keycode . dir) in `((:w . ,(gamekit:vec2 0 1))
      ;;                                (:s . ,(gamekit:vec2 0 -1))
      ;;                                (:a . ,(gamekit:vec2 -1 0))
      ;;                                (:d . ,(gamekit:vec2 1 0)))
      ;;       do (binder keycode (change-direction player dir)))))))
      (with-slots (direction) player
        (binder :w (change-direction player (gamekit:vec2 0 1)))
        (binder :s (change-direction player (gamekit:vec2 0 -1)))
        (binder :a (change-direction player (gamekit:vec2 -1 0)))
        (binder :d (change-direction player (gamekit:vec2 1 0)))
        (binder :space (change-direction player (gamekit:vec2))))
      ;; NOTE: This is just for debugging.
      (binder :q (setf food-pos (new-food-pos))))))

(defmethod gamekit:act ((this snake-game))
  (let* ((player (player-of this))
         (direction (direction-of player))
         (position (snake-position player)))
    (setf (snake-position player) (gamekit:add position direction))
    (when (> (gamekit:x position) +screen-width+)
      (setf (gamekit:x (snake-position player)) 0))
    (when (< (gamekit:x position) 0)
      (setf (gamekit:x (snake-position player)) +screen-width+))
    (when (> (gamekit:y position) +screen-height+)
      (setf (gamekit:y (snake-position player)) 0))
    (when (< (gamekit:y position) 0)
      (setf (gamekit:y (snake-position player)) +screen-height+))))

(defmethod gamekit:draw ((this snake-game))
  ;; TODO(bsvercl): Drop into CL-BODGE to speed this up.
  ;; This draws a grid using hard-coded constants. :) Horribly.
  ;; Don't do this at home, kids!
  (loop for x from 0 to 31
        for xx = (floor (* x +segment-size+))
        do (loop for y from 0 to 23
                 for yy = (* y +segment-size+)
                 do (gamekit:draw-rect (gamekit:vec2 xx yy)
                                       +segment-size+
                                       +segment-size+
                                       :fill-paint (gamekit:vec4)
                                       :stroke-paint (gamekit:vec4 0.8 0.8 0.8 1.0))))
  ;; Draw SNAKE
  (loop for segment across (segments-of (player-of this))
        do (gamekit:draw-rect segment
                              +segment-size+
                              +segment-size+
                              :fill-paint +snake-color+))
  ;; Draw food
  (gamekit:draw-rect (food-pos-of this)
                     +segment-size+
                     +segment-size+
                     :fill-paint +food-color+))

(defun play ()
  (gamekit:start 'snake-game :viewport-resizable nil))
