(in-package :sdl2-event-loop.impl)

;;;; MANUALLY DEFINED ALIASES

(defvar sdl2-event-loop.events.general:idle :idle)
(defvar sdl2-event-loop.events.general:quit :quit)

;;;; GENERAL EVENTS

(define-event-macro with-controller-axis-motion-event
  sdl2-event-loop.events.general:controller-axis-motion
  :controlleraxismotion)

(define-event-macro with-controller-button-down-event
  sdl2-event-loop.events.general:controller-button-down
  :controllerbuttondown)

(define-event-macro with-controller-button-up-event
  sdl2-event-loop.events.general:controller-button-up
  :controllerbuttonup)

(define-event-macro with-controller-device-added-event
  sdl2-event-loop.events.general:controller-device-added
  :controllerdeviceadded)

(define-event-macro with-controller-device-remapped-event
  sdl2-event-loop.events.general:controller-device-remapped
  :controllerdeviceremapped)

(define-event-macro with-controller-device-removed-event
  sdl2-event-loop.events.general:controller-device-removed
  :controllerdeviceremoved)

(define-event-macro with-dollar-gesture-event
  sdl2-event-loop.events.general:dollar-gesture
  :dollargesture)

(define-event-macro with-drop-file-event
  sdl2-event-loop.events.general:drop-file
  :dropfile)

(define-event-macro with-finger-motion-event
  sdl2-event-loop.events.general:finger-motion
  :fingermotion)

(define-event-macro with-finger-down-event
  sdl2-event-loop.events.general:finger-down
  :fingerdown)

(define-event-macro with-finger-up-event
  sdl2-event-loop.events.general:finger-up
  :fingerup)

(define-event-macro with-joyaxis-motion-event
  sdl2-event-loop.events.general:joyaxis-motion
  :joyaxismotion)

(define-event-macro with-joyball-motion-event
  sdl2-event-loop.events.general:joyball-motion
  :joyballmotion)

(define-event-macro with-joy-button-down-event
  sdl2-event-loop.events.general:joy-button-down
  :joybuttondown)

(define-event-macro with-joy-button-up-event
  sdl2-event-loop.events.general:joy-button-up
  :joybuttonup)

(define-event-macro with-joy-device-added-event
  sdl2-event-loop.events.general:joy-device-added
  :joydeviceadded)

(define-event-macro with-joy-device-removed-event
  sdl2-event-loop.events.general:joy-device-removed
  :joydeviceremoved)

(define-event-macro with-joy-hat-motion-event
  sdl2-event-loop.events.general:joy-hat-motion
  :joyhatmotion)

(define-event-macro with-key-down-event
  sdl2-event-loop.events.general:key-down
  :keydown)

(define-event-macro with-key-up-event
  sdl2-event-loop.events.general:key-up
  :keyup)

(define-event-macro with-mouse-button-down-event
  sdl2-event-loop.events.general:mouse-button-down
  :mousebuttondown)

(define-event-macro with-mouse-button-up-event
  sdl2-event-loop.events.general:mouse-button-up
  :mousebuttonup)

(define-event-macro with-mouse-motion-event
  sdl2-event-loop.events.general:mouse-motion
  :mousemotion)

(define-event-macro with-mouse-wheel-event
  sdl2-event-loop.events.general:mouse-wheel
  :mousewheel)

(define-event-macro with-multi-gesture-event
  sdl2-event-loop.events.general:multi-gesture
  :multigesture)

(define-event-macro with-sys-wm-event
  sdl2-event-loop.events.general:sys-wm
  :syswmevent)

(define-event-macro with-text-editing-event
  sdl2-event-loop.events.general:text-editing
  :textediting)

(define-event-macro with-text-input-event
  sdl2-event-loop.events.general:text-input
  :textinput)

(define-event-macro with-user-event
  sdl2-event-loop.events.general:user
  :userevent)

(define-event-macro with-raw-window-event
  sdl2-event-loop.events.general:raw-window-event
  :windowevent)

;;;; WINDOW EVENTS

(define-windowevent-macro with-window-event-none
  sdl2-event-loop.events.window:none
  :windowevent-none)

(define-windowevent-macro with-window-event-shown
  sdl2-event-loop.events.window:shown
  :windowevent-shown)

(define-windowevent-macro with-window-event-hidden
  sdl2-event-loop.events.window:hidden
  :windowevent-hidden)

(define-windowevent-macro with-window-event-exposed
  sdl2-event-loop.events.window:exposed
  :windowevent-exposed)

(define-windowevent-macro with-window-event-moved
  sdl2-event-loop.events.window:moved
  :windowevent-moved)

(define-windowevent-macro with-window-event-resized
  sdl2-event-loop.events.window:resized
  :windowevent-resized)

(define-windowevent-macro with-window-event-size-changed
  sdl2-event-loop.events.window:size-changed
  :windowevent-size-changed)

(define-windowevent-macro with-window-event-minimized
  sdl2-event-loop.events.window:minimized
  :windowevent-minimized)

(define-windowevent-macro with-window-event-maximized
  sdl2-event-loop.events.window:maximized
  :windowevent-maximized)

(define-windowevent-macro with-window-event-restored
  sdl2-event-loop.events.window:restored
  :windowevent-restored)

(define-windowevent-macro with-window-event-enter
  sdl2-event-loop.events.window:enter
  :windowevent-enter)

(define-windowevent-macro with-window-event-leave
  sdl2-event-loop.events.window:leave
  :windowevent-leave)

(define-windowevent-macro with-window-event-focus-gained
  sdl2-event-loop.events.window:focus-gained  
  :windowevent-focus-gained)

(define-windowevent-macro with-window-event-focus-lost
  sdl2-event-loop.events.window:focus-lost
  :windowevent-focus-lost)

(define-windowevent-macro with-window-event-close
  sdl2-event-loop.events.window:close
  :windowevent-close)

(define-windowevent-macro with-window-event-take-focus
  sdl2-event-loop.events.window:take-focus
  :windowevent-take-focus)

(define-windowevent-macro with-window-event-hit-test
  sdl2-event-loop.events.window:hit-test
  :windowevent-hit-test)

(define-windowevent-macro with-window-event-iccprof-changed
  sdl2-event-loop.events.window:iccprof-changed
  :windowevent-iccprof-changed)

(define-windowevent-macro with-window-event-display-changed
  sdl2-event-loop.events.window:display-changed
  :windowevent-display-changed)


