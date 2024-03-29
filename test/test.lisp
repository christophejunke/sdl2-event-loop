(defpackage :sdl2-event-loop.test
  (:use :cl :sdl2 :sdl2-event-loop :sdl2-event-loop.events :alexandria)
  (:local-nicknames (GEV sdl2-event-loop.events.general)
                    (WEV sdl2-event-loop.events.window))
  (:export #:single-loop
           #:dispatch))

(in-package :sdl2-event-loop.test)

(defparameter *message*
  "NOTE: nothing is displayed in the window.
Events are logged to *STANDARD-OUTPUT*.")

;;;;
;;;; Example of single loop event handling
;;;;

(defun single-loop ()
  (format t "~&~a~%" *message*)
  (with-captured-bindings (@progn . :all)
    (with-everything (:window (w :w 600 :h 600) :gl gl)
      (@progn
        (flet ((info (&rest rest) (print rest) (values)))
          (declare (inline info))
          (do-match-events (:method :wait :timeout 600)
            (:quit (return))
            (:idle (info :idle))
            (with-window-event-moved (_ :x x :y y)
              (info :window :x x :y y))
            (with-window-event-focus-gained (_)
              (info :focused))
            (with-mouse-motion-event (_ :xrel dx :yrel dy)
              (info :mouse :dx dx :dy dy))
            (with-finger-motion-event (_ :dx dx :dy dy)
              (info :finger :dx dx :dy dy))))))))

;;;;
;;;; Example of CLOS-based event-handler
;;;;

(defgeneric handle-event (game event-type event)
  (:documentation "Game event handler for EVENT of type EVENT-TYPE")
  ;; HANDLE-EVENT handle both SDL2 events and game specific events  
  (:method (game type _)
    "The default method does nothing"))

;; Client code would call this to exit the loop
(defun quit-event-loop ()
  "Exit a loop started with SERVE-EVENTS"
  (throw :quit nil))

(defun serve-events (game)
  "Repeatedly serve events until an event handler calls QUIT-EVENT-LOOP"
  (catch :quit
    (loop
      (catch :restart
        (handle-event game :game :started)
        (unwind-protect (do-events (event :event-type type :method :wait :timeout nil)
                          (restart-case (handle-event game type event)
                            (ignore () :report "Ignore event")
                            (stop-start () :report "Stop game and restart it"
                              (throw :restart nil))))
          (handle-event game :game :stopped))))))

(defmethod handle-event (g (_ (eql gev:quit)) e)
  "Quit loop on :QUIT events"
  (quit-event-loop))

(defun run (game)
  (format t "~&~a~%~%" *message*)
  (with-captured-bindings (@progn *standard-output*
                                  *error-output*)
    (with-init (:everything)
      ;; in another thread
      (@progn
       (serve-events game)))))

(defclass sample-game ()
  ((main-window :initarg :window :reader main-window)))

(defmethod handle-event ((game sample-game) (_ (eql :game)) (e  (eql :started)))
  (assert (not (slot-boundp game 'main-window)) () "No main window should exist here")
  (initialize-instance game :window (create-window)))

(defmethod handle-event ((game sample-game) (_ (eql :game)) (e (eql :stopped)))
  (when (slot-boundp game 'main-window)
    (destroy-window (main-window game))
    (slot-makunbound game 'main-window)))

;; for debugging
(progn
  (defvar *game* nil)
  (defmethod handle-event :before (game (_ (eql :game)) (e (eql :started)))
    (setf *game* game)))

(defun dispatch ()
  "Example of a CLOS dispatch event handler"
  (run (make-instance 'sample-game)))

(defmethod handle-event (game (type (eql gev:raw-window-event)) event)
  (with-raw-window-event (event :event code)
    (handle-event game (windowevent code) event)))

(defmethod handle-event (game (type (eql wev:moved)) event)
  (with-window-event-moved (event :x x :y y)
    (format t "~&Window moved for game ~s: ~d, ~d~%" game x y)))

(defmethod handle-event (game (type (eql wev:close)) event)
  (with-window-event-close (event :window-id w)
    (format t "~&Window closed ~a~%" w)))

(defmethod handle-event ((game sample-game) (type (eql gev:key-down)) event)
  (with-key-down-event (event :keysym keysym)
    (if-let (command (case (scancode keysym)
                       (:scancode-escape (break "Entering debugger"))
                       (:scancode-up :go-up)
                       (:scancode-down :go-down)
                       (:scancode-left :go-left)
                       (:scancode-right :go-right)))
      ;; here :command is not an SDL2 event type, it is part of the
      ;; game logic (it works as long as event type names do not
      ;; collide).
      (handle-event game :command command))))

(defmethod handle-event (game (type (eql gev:text-input)) event)
  (with-text-input-event (event :text code)
    (format t
            "~&Text input: ~S~%"
            (if (typep code 'char-code)
                (string (code-char code))
                code))))

(defmethod handle-event ((game sample-game) (type (eql :command)) event)
  "Handler for a game command"
  (format t "~&Game ~s received command ~a~%" game event))

;; user-defined SDL2 type

(register-user-event-type :test-event)
(defmethod handle-event (game (type (eql :test-event)) event)
  (format t "~&Game ~s received :test-event~%" game))

(defmethod handle-event (game (type (eql gev:finger-up)) event)
  "Releasing the finger sends the custom user event :test-event"
  (with-finger-up-event (event)
    (format t "~&Finger up~%")
    (push-user-event :test-event)))
