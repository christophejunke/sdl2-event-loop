(defpackage :sdl2-event-loop.test
  (:use :cl :sdl2 :sdl2-event-loop :sdl2-event-loop.events :alexandria)
  (:local-nicknames (GEV sdl2-event-loop.events.general)
                    (WEV sdl2-event-loop.events.window))
  (:import-from #:sdl2-event-loop.impl
                sdl2-event-loop.impl:with-captured-bindings)
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
  (with-captured-bindings (rebind-progn *standard-output* *error-output*)
    (with-everything (:window (w :w 600 :h 600) :gl gl)
      (rebind-progn
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
    "The default method logs all but some events"
    ;; too much output otherwise
    (unless (member type (load-time-value
                          (list gev:raw-window-event
                                gev:mouse-motion
                                gev:idle)))
      (format t "~&Event ~@{~a~^ ~}~%" game type)
      (finish-output))))

;; Client code would call this to exit the loop
(defun quit-event-loop ()
  "Exit a loop started with SERVE-EVENTS"
  (throw :quit nil))

(defun serve-events (game)
  "Repeatedly serve events until an event handler calls QUIT-EVENT-LOOP"
  (catch :quit
    (handle-event game :game-started t)
    (unwind-protect (do-events (event :event-type type :method :wait :timeout nil)
                      (restart-case (handle-event game type event)
                        (ignore () :report "Ignore event")))
      (handle-event game :game-stopped t))))

(defmethod handle-event (g (_ (eql gev:quit)) e)
  "Quit loop on :QUIT events"
  (quit-event-loop))

(defun run (game)
  (loop
    (format t "~&~a~%~%" *message*)
    (restart-case (with-captured-bindings (rebind-progn *standard-output*
                                                        *error-output*)
                    (with-init (:everything)
                      ;; in another thread
                      (rebind-progn (serve-events game)))
                    ;; exit loop in normal case
                    (return-from run))
      (retry () 
        :report "Restart game"))))

(defclass sample-game ()
  ((main-window :initarg :window :reader main-window)))

(defmethod handle-event ((game sample-game) (_ (eql :game-started)) e)
  (assert (not (slot-boundp game 'main-window)) () "No main window should exist here")
  (initialize-instance game :window (create-window)))

(defmethod handle-event ((game sample-game) (_ (eql :game-stopped)) e)
  (slot-makunbound game 'main-window)
  (reinitialize-instance game))

;; for debugging
(progn
  (defvar *game* nil)
  (defmethod handle-event :before (game (_ (eql :game-started)) e)
    (assert (not *game*))
    (setf *game* game))
  (defmethod handle-event :after (game (_ (eql :game-stopped)) e)
    (setf *game* nil)))

(defun dispatch ()
  "Example of a CLOS dispatch event handler"
  (run (make-instance 'sample-game)))

(defmethod handle-event (game (type (eql gev:raw-window-event)) event)
  (with-raw-window-event (event :event code)
    (handle-event game (windowevent code) event)))

(defmethod handle-event (game (type (eql wev:moved)) event)
  (with-window-event-moved (event :x x :y y)
    (format t "~&Window moved for game ~s: ~d, ~d~%" game x y)))

(defmethod handle-event ((game sample-game) (type (eql gev:key-down)) event)
  (with-key-down-event (event :keysym keysym)
    (if-let (command (case (scancode keysym)
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
