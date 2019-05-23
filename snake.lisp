;;;; snake.lisp

(in-package #:snake)

(gamekit:defgame snake-game () ())

(defun play ()
  (gamekit:start 'snake-game :viewport-resizable nil))
