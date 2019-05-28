;;;; snake.lisp

(in-package #:snake)

(defparameter +segment-size+ 25)
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)
(defparameter +segments-across-width+ (floor (/ +screen-width+ +segment-size+)))
(defparameter +segments-across-height+ (floor (/ +screen-height+ +segment-size+)))

(defparameter +snake-color+ (gamekit:vec4 1.0 0.75 0.5 1.0))
(defparameter +food-color+ (gamekit:vec4 0.5 0.25 1.0 1.0))
(defparameter +grid-color+ (gamekit:vec4 0.9 0.9 0.9 0.5))
(defparameter +transparent+ (gamekit:vec4))

(defun vec2= (a b)
  (and (= (gamekit:x a)
          (gamekit:x b))
       (= (gamekit:y a)
          (gamekit:y b))))

(defun mod-vec (this that)
  "MOD x and y of THIS by x and y of THAT."
  (gamekit:vec2 (mod (gamekit:x this) (gamekit:x that))
                (mod (gamekit:y this) (gamekit:y that))))

(defclass snake ()
  ((segments :initarg :segments :accessor segments-of)
   (direction :initarg :direction :accessor direction-of))
  (:documentation "The moving thing, usually user controlled."))

(defun make-snake (starting-position &optional (direction (gamekit:vec2)))
  "Creates a SNAKE with STARTING-POSITION and DIRECTION."
  (make-instance 'snake :segments (list starting-position)
                        :direction direction))

(defun snake-position (snake)
  "Head position of SNAKE."
  (first (segments-of snake)))

(defmethod (setf snake-position) (pos snake)
  (setf (first (segments-of snake)) pos))

;; TODO(bsvercl): Don't allow Left<->Right Up<->Down
(defun change-direction (snake new-direction)
  "Modify DIRECTION of SNAKE with NEW-DIRECTION."
  (setf (direction-of snake) new-direction))

(defun advance (snake ate-food-p)
  (with-slots (segments direction) snake
    (let* ((position (snake-position snake))
           (head (gamekit:add position  direction))
           (which-segments (if ate-food-p segments (butlast segments)))
           (new-segments (push head which-segments)))
      (mapc #'(lambda (pos) (gamekit:add pos direction)) segments)
      (setf segments new-segments)
      ;; Wrap SNAKE around the boundaries
      (setf (snake-position snake) (mod-vec (snake-position snake)
                                            (gamekit:vec2 +segments-across-width+
                                                          +segments-across-height+))))))

(gamekit:defgame snake-game ()
  ((player :reader player-of)
   (food-pos :reader food-pos-of)
   (score :initform 0 :reader score-of))
  (:viewport-title "Snake")
  (:viewport-width +screen-width+)
  (:viewport-height +screen-height+)
  (:act-rate 10))

(defun new-food-pos ()
  "Generates a new spot on the grid."
  (gamekit:vec2 (random +segments-across-width+)
                (random +segments-across-height+)))

(defmethod gamekit:post-initialize ((this snake-game))
  (with-slots (player food-pos) this
    (setf player (make-snake (gamekit:vec2 (/ +segments-across-width+ 2)
                                           (/ +segments-across-height+ 2))))
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
      ;; NOTE: These are for debugging.
      (binder :space (change-direction player (gamekit:vec2)))
      (binder :e (setf (snake-position player) (gamekit:vec2)))
      (binder :q (setf food-pos (new-food-pos))))))

(defmethod gamekit:act ((this snake-game))
  ;; TODO(bsvercl): This is slow on slow processors. ;)
  (with-slots (player food-pos score) this
    (let* ((position (snake-position player))
           (ate-food-p (vec2= food-pos position)))
      (advance player ate-food-p)
      (when ate-food-p
        (setf food-pos (new-food-pos))
        (incf score (random 100))))))

(defmethod gamekit:draw ((this snake-game))
  ;; TODO(bsvercl): Drop into CL-BODGE to speed this up.
  (loop for x from 0 below +segments-across-width+
        do (loop for y from 0 below +segments-across-height+
                 do (gamekit:draw-rect (gamekit:mult (gamekit:vec2 x y)
                                                     +segment-size+)
                                       +segment-size+ +segment-size+
                                       :fill-paint +transparent+
                                       :stroke-paint +grid-color+)))
  ;; Draw food
  (gamekit:draw-rect (gamekit:mult (food-pos-of this) +segment-size+)
                     +segment-size+ +segment-size+
                     :fill-paint +food-color+)
  ;; Draw SNAKE
  (mapc #'(lambda (pos)
            (gamekit:draw-rect (gamekit:mult pos +segment-size+)
                               +segment-size+ +segment-size+
                               :fill-paint +snake-color+))
        (segments-of (player-of this)))
  ;; Draw SCORE
  (gamekit:draw-text (format nil "Score: ~R" (score-of this))
                     (gamekit:vec2 25 (- +screen-height+ 25))))

(defun play ()
  (gamekit:start 'snake-game :viewport-resizable nil))
