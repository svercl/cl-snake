;;;; snake.lisp

(in-package #:snake)

(defparameter +segment-size+ 25.0)
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)

(defparameter +snake-color+ (gamekit:vec4 1.0 0.75 0.5 1.0))
(defparameter +food-color+ (gamekit:vec4 0.5 0.25 1.0 1.0))
(defparameter +grid-color+ (gamekit:vec4 0.8 0.8 0.8 1.0))
(defparameter +transparent+ (gamekit:vec4))

(defun vec2= (a b)
  (and (= (gamekit:x a)
          (gamekit:x b))
       (= (gamekit:y a)
          (gamekit:y b))))

;; TODO(bsvercl): Use this for wrapping snake position.
(defun mod-vec (vec divisor)
  "MOD x and y of VEC by DIVISOR."
  (gamekit:vec2 (mod (gamekit:x vec) divisor)
                (mod (gamekit:y vec) divisor)))

(defun floor-vec (vec)
  "FLOOR x and y of VEC."
  (gamekit:vec2 (floor (gamekit:x vec))
                (floor (gamekit:y vec))))

(defun snap-to-grid (vec &optional (cell-size (gamekit:vec2 +segment-size+ +segment-size+)))
  "Snap VEC to CELL-SIZE."
  (gamekit:mult (floor-vec (gamekit:div vec cell-size)) cell-size))

(defclass snake ()
  ((segments :initarg :segments :accessor segments-of)
   (direction :initarg :direction :accessor direction-of))
  (:documentation "The moving thing, usually user controlled."))

(defun make-snake (starting-position &optional (direction (gamekit:vec2)))
  "Creates a SNAKE with STARTING-POSITION and DIRECTION."
  (let ((segments (list starting-position)))
    (make-instance 'snake :segments segments :direction direction)))

(defun snake-position (snake)
  (first (segments-of snake)))

(defmethod (setf snake-position) (pos snake)
  (setf (first (segments-of snake)) pos))

(defun change-direction (snake new-direction)
  "Modify DIRECTION of SNAKE with NEW-DIRECTION."
  (setf (direction-of snake) new-direction))

(defun advance (snake)
  (with-slots (segments direction) snake
    (let* ((position (snake-position snake))
           (new-head (gamekit:add position (gamekit:mult direction +segment-size+)))
           (segments-without-end (butlast segments))
           (new-segments (push new-head segments-without-end)))
      (setf segments new-segments)
      (cond
        ((> (gamekit:x position) +screen-width+)
         (setf (gamekit:x (snake-position snake)) 0))
        ((< (gamekit:x position) 0)
         (setf (gamekit:x (snake-position snake)) +screen-width+))
        ((> (gamekit:y (snake-position snake)) +screen-height+)
         (setf (gamekit:y (snake-position snake)) 0))
        ((< (gamekit:y (snake-position snake)) 0)
         (setf (gamekit:y (snake-position snake)) +screen-height+))))))

(gamekit:defgame snake-game ()
  ((player :reader player-of)
   (food-pos :accessor food-pos-of))
  (:viewport-title "Snake")
  (:viewport-width +screen-width+)
  (:viewport-height +screen-height+))

(defun new-food-pos ()
  (snap-to-grid (gamekit:vec2 (random +screen-width+)
                              (random +screen-height+))))

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
      (binder :w (change-direction player (gamekit:vec2 0 1)))
      (binder :s (change-direction player (gamekit:vec2 0 -1)))
      (binder :a (change-direction player (gamekit:vec2 -1 0)))
      (binder :d (change-direction player (gamekit:vec2 1 0)))
      (binder :space (change-direction player (gamekit:vec2)))
      ;; NOTE: This is just for debugging.
      (binder :q (setf food-pos (new-food-pos))))))

;; TODO(bsvercl): Game moves too fast.
(defmethod gamekit:act ((this snake-game))
  (with-slots (player food-pos) this
    (let ((position (snake-position player)))
      (advance player)
      (when (vec2= food-pos position)
        (setf food-pos (new-food-pos)))))
  ;; TODO(bsvercl): Remove this SLEEP.
  (sleep 0.1))

(defmethod gamekit:draw ((this snake-game))
  ;; TODO(bsvercl): Drop into CL-BODGE to speed this up.
  (loop for x from 0 to (floor (/ +screen-width+ +segment-size+))
        for xx = (floor (* x +segment-size+))
        do (loop for y from 0 to (floor (/ +screen-height+ +segment-size+))
                 for yy = (* y +segment-size+)
                 do (gamekit:draw-rect (gamekit:vec2 xx yy)
                                       +segment-size+
                                       +segment-size+
                                       :fill-paint +transparent+
                                       :stroke-paint +grid-color+)))
  ;; Draw SNAKE
  (mapc #'(lambda (pos)
            (gamekit:draw-rect pos +segment-size+ +segment-size+
                               :fill-paint +snake-color+))
        (segments-of (player-of this)))
  ;; Draw food
  (gamekit:draw-rect (food-pos-of this)
                     +segment-size+
                     +segment-size+
                     :fill-paint +food-color+))

(defun play ()
  (gamekit:start 'snake-game :viewport-resizable nil))
