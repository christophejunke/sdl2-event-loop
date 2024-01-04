(in-package :sdl2-event-loop.impl)

(defmacro with-captured-bindings ((rebinding-name &rest symbols) &body body)
  (when (member symbols '(t :all))
    (setf symbols '(*PACKAGE* *PRINT-GENSYM* *PRINT-BASE* *PRINT-RIGHT-MARGIN*
    *FEATURES* *PRINT-PRETTY* *COMPILE-FILE-PATHNAME* *GENSYM-COUNTER*
    *READ-EVAL* *QUERY-IO* *STANDARD-INPUT* *PRINT-LEVEL* *READTABLE*
    *PRINT-CIRCLE* *PRINT-RADIX* *ERROR-OUTPUT* *PRINT-LINES*
    *DEFAULT-PATHNAME-DEFAULTS* *TRACE-OUTPUT* *LOAD-TRUENAME* *LOAD-PATHNAME*
    *PRINT-CASE* *PRINT-ESCAPE* *PRINT-LENGTH* *READ-DEFAULT-FLOAT-FORMAT*
    *PRINT-PPRINT-DISPATCH* *PRINT-MISER-WIDTH* *TERMINAL-IO* *LOAD-PRINT*
    *DEBUG-IO* *COMPILE-PRINT* *STANDARD-OUTPUT* *MODULES* *LOAD-VERBOSE*
    *READ-SUPPRESS* *PRINT-ARRAY* *COMPILE-FILE-TRUENAME* *DEBUGGER-HOOK*
    *RANDOM-STATE* *PRINT-READABLY* *COMPILE-VERBOSE* *READ-BASE*
    *MACROEXPAND-HOOK* *BREAK-ON-SIGNALS*)))
  ;; maybe accept also package designators in the list
  (assert (every #'symbolp symbols))
  (alexandria:with-gensyms (inner-body)
    (if symbols
        (loop for s in symbols
              for c = (gensym)
              collect (list c s) into capture
              collect (list s c) into rebind
              finally
                 (return
                   `(let ,capture
                      (macrolet ((,rebinding-name (&body ,inner-body)
                                   `(let ,',rebind ,@,inner-body)))
                        ,@body))))
        `(macrolet ((,rebinding-name (&body ,inner-body)
                      `(progn ,@,inner-body)))
           ,@body))))
