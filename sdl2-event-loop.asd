(defsystem "sdl2-event-loop"
  :depends-on ("alexandria" "cl-autowrap" "sdl2")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "ffi")
               (:file "events")
               (:file "definitions")))

(defsystem "sdl2-event-loop/test"
  :depends-on ("sdl2-event-loop")
  :pathname "test"
  :components ((:file "test")))

