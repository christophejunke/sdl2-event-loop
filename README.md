SDL2-EVENT-LOOP
===============

# Overview

This system provides an alternative event loop in place of
`SDL2:WITH-EVENT-LOOP`, with the following goals in mind:

- Be it easy to either handle all events in the same
  function, like `SDL2:WITH-EVENT-LOOP`, or delegate event
  handling to other functions.

- Help Common Lisp environments display context-sensititve
  information about each kind of event, notably which
  keyword arguments are available for each event-type.

- Allow window events, which are further categorized into
  different kinds of event subtypes, to be handled like
  other events.

This is achieved, basically, by separating the existing
`SDL2:WITH-EVENT-LOOP` macro as different macros,
`DO-EVENTS` which iterates over events, and
`EVENT-TYPE-CASE` which dispatches according to an event
type. Furthermore, each event-type is associated with a
destructuring macro, making it easier than
`SDL2:WITH-EVENT-LOOP` to know which arguments are expected
for each kind of event.

For example, the following system uses the library in two
different ways. First, load the test system:

    (QL:QUICKLOAD "SDL2-EVENT-LOOP/TEST")
    
A call to `(SDL2-EVENT-LOOP.TEST:SINGLE-LOOP)` starts an
empty SDL2 window that manages all events in a single
function, and `(SDL2-EVENT-LOOP.TEST:DISPATCH)` uses a
generic function that is specialized on the event type. See
the source code for details.

# DO-EVENTS

`DO-EVENTS` performs the same polling/waiting code as
`SDL2:WITH-EVENT-LOOP` but does not dispatch events to event
handlers. Unlike the existing `SDL2:WITH-EVENT-LOOP`, nested
dynamic invocations are not protected (`RECURSIVE`
parameter).

The macro is a DO-style loop with an implicit NIL block,
which binds `EVENT` to an SDL event object (a single
allocation that is mutated). It also bind `EVENT-TYPE` to
the event's type (the type must be computed even when
anonymous). 

    (do-events (<event> [:event-type ( symbol )]
                        [:method ( :poll* | :wait )]
                        [:timeout ( nil* | milliseconds )])
      <body>)

Other options, `:METHOD` and `:TIMEOUT`, have the same
meaning as in `SDL2:WITH-EVENT-LOOP`:

  - `:METHOD` is either `:WAIT` (blocking) or `:POLL`
    (non-blocking, the default).

  - When provided, `:TIMEOUT` must be a positive integer
    representing a timeout in **milliseconds**. `:TIMEOUT`
    is only meaningful when used in conjunction with the
    `:WAIT` method (the blocking wait operation terminates
    after TIMEOUT milliseconds).

Note that there is no `:BACKGROUND` flag, the event loop
must be run in a thread/context where SDL2 is initialized.

When METHOD is `:WAIT` with a non-NIL `:TIMEOUT`, or when
METHOD is `:POLL`, then `EVENT-TYPE` might be `:IDLE`.

`EVENT-TYPE` might also be one of the following keywords:

    :CONTROLLERAXISMOTION :CONTROLLERBUTTONDOWN
    :CONTROLLERBUTTONUP :CONTROLLERDEVICEADDED
    :CONTROLLERDEVICEREMAPPED :CONTROLLERDEVICEREMOVED
    :DOLLARGESTURE :DROPFILE :FINGERDOWN :FINGERMOTION
    :FINGERUP :JOYAXISMOTION :JOYBALLMOTION :JOYBUTTONDOWN
    :JOYBUTTONUP :JOYDEVICEADDED :JOYDEVICEREMOVED
    :JOYHATMOTION :KEYDOWN :KEYUP :MOUSEBUTTONDOWN
    :MOUSEBUTTONUP :MOUSEMOTION :MOUSEWHEEL :MULTIGESTURE
    :QUIT :SYSWMEVENT :TEXTEDITING :TEXTINPUT :USEREVENT
    :WINDOWEVENT

Aliases to these keywords may be found in the two following packages:

   - `SDL2-EVENT-LOOP.EVENTS.GENERAL` for all base events
   - `SDL2-EVENT-LOOP.EVENTS.WINDOW` for window events

Finally, it can be equal to any symbol registered through
`SDL2:REGISTER-USER-EVENT-TYPE`. If so, a call to
`SDL2::FREE-USER-DATA` is done after each iteration or when
unwinding from the loop.

# WITH-CAPTURED-BINDINGS

The main SDL2 API uses a dedicated thread. `SDL2:WITH-INIT` and
`SDL2:WITH-EVERYTHING` transfer their body into another thread;
consequently, the dynamic bindings in effect inside these macros are
possibly different from the one outside of them. For that reason, the
current library expose `WITH-CAPTURED-BINDINGS`: it lexically binds
the current values of variables outside a dynamic context change
(e.g. a closure intended to be run in a different dynamic scope, like
a thread), and defines a `macrolet` that can be expanded in another
context to rebind the saved bindings. In other words, with the
following code, `my-progn` is going to be bound to a `macrolet`,
expanded inside the anonymous function as `let` bindings:

    (with-captured-bindings (my-progn *standard-output* *error-output*)
      (make-thread (lambda () (my-progn (print "hello")))))

After cleaning it a bit for clarity, the above expands as follows:

    (LET ((G382 *STANDARD-OUTPUT*) (G383 *ERROR-OUTPUT*))
      (MAKE-THREAD
       (LAMBDA ()
         (LET ((*STANDARD-OUTPUT* G382) (*ERROR-OUTPUT* G383))
           (PRINT "hello")))))

The macro accepts a list of symbols, but currently if that list is
instead a symbol `T` or `:ALL`, then all 44 CL standard special
variables surrounded by earmuffs (ie. not REPL history variables) are
rebound locally in the thread. More precisely, this is expressed as
follows:

    (with-captured-bindings (rebind . :all)
      ...)

This syntax is not very satisfactory and is probably going to change.

# Destructuring events

All SDL2 events listed above are associated with a
corresponding macro which is used to destructure a given
event into its components.

For example, in definitions.lisp, the following line defines
a macro named `WITH-KEY-DOWN-EVENT` associated with the
`:KEYDOWN` event type:

    (define-event-macro with-key-down-event
      sdl2-event-loop.events.general:key-down
      :keydown)

The macro has the following signature:

    (with-key-down-event
      (event &key key timestamp window-id state repeat keysym)
      &body body)

Each keyword argument is used to name a local variable that
represents the event's field. Like in
`SDL2:WITH-EVENT-LOOP`, only those slots that are referenced
in the argument list are bound.

For example, here is a sample usage of this macro:

    (with-key-down-event (e :keysym k)
      (print (scancode-value k)))

Here below is one level of macroexapnsion:

    (LET ((K (PLUS-C:C-REF E SDL2-FFI:SDL-EVENT :KEY :KEYSYM)))
      (PRINT (SCANCODE-VALUE K)))

Unlike with `SDL2:WITH-EVENT-LOOP`, each event is associated
with a different macro, and thus a different signature.
That makes it easier to known which keyword arguments are
relevant for each event type.

# Window events

The `:WINDOWEVENT` type is associated with a macro named
`WITH-RAW-WINDOW-EVENT`. It is a generic window event that
covers a range of subtypes of window events, held in its
`:EVENT` slot. It also has two general-purpose slots named
`:DATA1` and `:DATA2`.

The `DEFINE-WINDOWEVENT-MACRO` defines one macro for each
subtype of window event, making it possible to directly
destructure an SDL2 event as a specific kind of window
event.

For example: `WITH-WINDOW-EVENT-MOVED` can be used as follows:

    (with-window-event-moved (e :x x :y y :window-id w)
      (print (list (get-window-title w) x y)))

One step of macroexpansion introduces
`WITH-RAW-WINDOW-EVENT`, where `:X` and `:Y` where replaced
respectively by `:DATA1` and `:DATA2`:

    (WITH-RAW-WINDOW-EVENT (E :DATA1 X :DATA2 Y :WINDOW-ID W)
      (PRINT (LIST (SDL2:GET-WINDOW-TITLE W) X Y)))

# EVENT-TYPE-CASE

The `DO-EVENTS` loop above iterate over events and for each
one, determines its type (a keyword). The `EVENT-TYPE-CASE`
macro provides a way to dispatch on event types using either
two kinds of clauses:

- a single keyword which identifies an event-type
- a form with the same syntax as the destructuring WITH- macros seen
  previously. That allows to reuse the same syntax for both dispatch
  and destructuring, and benefits from auto-completion facilities that
  are available for WITH- macros in the scope of a case.
- `T`, or `OTHERWISE`, for all other events.

For example, consider the following expression:

    (event-type-case (event event-type)
      (with-key-up-event (_ :repeat repeat)
        (print repeat))
      (with-window-event-shown (_ :window-id w)
        (print w))
      (with-window-event-maximized (_ :window-id w)
        (print w))
      (with-finger-down-event (_ :pressure p)
        (print p))
      (t (print "other")))

The above is macroexpanded as:

    (CASE EVENT-TYPE
      (:KEYUP
       (WITH-KEY-UP-EVENT (EVENT :REPEAT REPEAT)
         (PRINT REPEAT)))
      (:FINGERDOWN
       (WITH-FINGER-DOWN-EVENT (EVENT :PRESSURE P)
         (PRINT P)))
      (:WINDOWEVENT
       (WITH-RAW-WINDOW-EVENT (EVENT :EVENT #:G775)
         (CASE (WINDOWEVENT #:G775)
           (:WINDOWEVENT-SHOWN
            (WITH-WINDOW-EVENT-SHOWN (EVENT :WINDOW-ID W)
              (PRINT W)))
           (:WINDOWEVENT-MAXIMIZED
            (WITH-WINDOW-EVENT-MAXIMIZED (EVENT :WINDOW-ID W)
              (PRINT W))))))
      (T (PRINT "other")))

Notice that:

- Each `WITH-` form implicitly knows which event-type keyword to match
  against.

- Window events are grouped as a single clause, and further dispatched
  according to the `WINDOWEVENT` tag in that clause.

- The `EVENT` parameter of each `WITH-` form is redundant in
  the context of `EVENT-TYPE-CASE`; it can thus be omitted
  (either `NIL` or a symbol which is `STRING=` to
  `"_"`). If, however, you put a symbol here, it is locally
  bound to the current event (see `DO-MATCH-EVENTS`):

        (event-type-case (event event-type)
          (with-key-down-event (e)
            (print e)))

  After macroexpansion:

        (CASE EVENT-TYPE
          (:KEYDOWN
           (WITH-KEY-DOWN-EVENT (EVENT)
             (LET ((E EVENT))
               (PRINT E)))))

# DO-MATCH-EVENTS

`DO-MATCH-EVENTS` is a simple macro that combines
`DO-EVENTS` with `EVENT-TYPE-CASE`, in such a way that
neither EVENT nor EVENT-TYPE needs be explicitly named. For
example, consider:

     (do-match-events (:method :wait)
       (:quit (return))
       (:idle (redisplay))
       (with-window-event-moved (e :x x :y y)
         (print (list :moved e x y))))

The above is equivalent to the follwing form:

    (do-events (event
                :event-type event-type
                :method :wait)
      (event-type-case (event event-type)
        (:quit (return))
        (:idle (redisplay))
        (with-window-event-moved (e :x x :y y)
          (print (list :moved e x y)))))

The local binding `E` in `WITH-WINDOW-EVENT-MOVED` refers to
the implicit event being handled.
