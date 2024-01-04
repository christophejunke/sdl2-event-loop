(defpackage :sdl2-event-loop
  (:documentation "Main package for SDL2 event loop macros")
  (:use)
  (:export #:do-match-events
           #:do-events
           #:event-type-case
           #:windowevent))

(defpackage #:sdl2-event-loop.events.general
  (:use)
  (:documentation "Symbols for general event types")
  (:export #:controller-axis-motion
           #:controller-button-down
           #:controller-button-up
           #:controller-device-added
           #:controller-device-remapped
           #:controller-device-removed
           #:dollar-gesture
           #:drop-file
           #:finger-down
           #:finger-motion
           #:finger-up
           #:idle
           #:joy-button-down
           #:joy-button-up
           #:joy-device-added
           #:joy-device-removed
           #:joy-hat-motion
           #:joyaxis-motion
           #:joyball-motion
           #:key-down
           #:key-up
           #:mouse-button-down
           #:mouse-button-up
           #:mouse-motion
           #:mouse-wheel
           #:multi-gesture
           #:quit
           #:raw-window-event  
           #:sys-wm
           #:text-editing
           #:text-input
           #:user))

(defpackage #:sdl2-event-loop.events.window
  (:use)
  (:documentation "Symbols for window event types")
  (:export #:close
           #:display-changed
           #:enter
           #:exposed
           #:focus-gained
           #:focus-lost
           #:hidden
           #:hit-test
           #:iccprof-changed
           #:leave
           #:maximized
           #:minimized
           #:moved
           #:none
           #:resized
           #:restored
           #:shown
           #:size-changed
           #:take-focus))

(defpackage :sdl2-event-loop.events
  (:documentation "All event destructuring macros")
  (:export #:with-controller-axis-motion-event
           #:with-controller-button-down-event
           #:with-controller-button-up-event
           #:with-controller-device-added-event
           #:with-controller-device-remapped-event
           #:with-controller-device-removed-event
           #:with-dollar-gesture-event
           #:with-drop-file-event
           #:with-finger-down-event
           #:with-finger-motion-event
           #:with-finger-up-event
           #:with-joy-button-down-event
           #:with-joy-button-up-event
           #:with-joy-device-added-event
           #:with-joy-device-removed-event
           #:with-joy-hat-motion-event
           #:with-joyaxis-motion-event
           #:with-joyball-motion-event
           #:with-key-down-event
           #:with-key-up-event
           #:with-lisp-message-event
           #:with-mouse-button-down-event
           #:with-mouse-button-up-event
           #:with-mouse-motion-event
           #:with-mouse-wheel-event
           #:with-multi-gesture-event
           #:with-sys-wm-event
           #:with-text-editing-event
           #:with-text-input-event
           #:with-user-event
           #:with-raw-window-event   
           #:with-window-event-close
           #:with-window-event-display-changed
           #:with-window-event-enter
           #:with-window-event-exposed
           #:with-window-event-focus-gained
           #:with-window-event-focus-lost
           #:with-window-event-hidden
           #:with-window-event-hit-test
           #:with-window-event-iccprof-changed
           #:with-window-event-leave
           #:with-window-event-maximized
           #:with-window-event-minimized
           #:with-window-event-moved
           #:with-window-event-none
           #:with-window-event-resized
           #:with-window-event-restored
           #:with-window-event-shown
           #:with-window-event-size-changed
           #:with-window-event-take-focus
           ))

(defpackage :sdl2-event-loop.impl
  (:use :cl :sdl2 :autowrap :sdl2-event-loop :sdl2-event-loop.events)
  (:documentation "Implementation package for SDL2-EVENT-LOOP")
  (:export #:with-captured-bindings))
