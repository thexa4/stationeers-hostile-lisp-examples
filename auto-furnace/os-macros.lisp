; Set title
(write-char (code-char 27))
(write-string "]0;AutoFurnace v1")
(write-char (code-char 7))

(defun pwm (counter)
  (lambda (pct)
    (setq counter (+ counter 1))
    (if (>= counter 9)
      (setq counter 0)
    )
    (> (* pct 8.01) counter)
  )
)

(defun ensure-power (device currently-on should-on)
  (if (and currently-on (not should-on))
    (stationeers::syscall-device-store-async device logic-type:on 0)
  )
  (if (and (not currently-on) should-on)
    (stationeers::syscall-device-store-async device logic-type:on 1)
  )
)

(defmacro essential (module)
  `(let
    (
      (result ,module)
    )
    (unless result
      (error ,(concatenate 'string "Module " (symbol-name (car module)) " failed to load"))
    )
    result
  )
)

(defmacro prefetch (defs &rest forms)
  (let*
    (
      (precache-syms (loop for def in defs collect (cons (car def) (gensym (symbol-name (car def))))))
      (precache-vars (loop for lookup in precache-syms collect (cons (cdr lookup) (cons nil nil))))
      (precache-forms (loop for lookup in precache-syms for def in defs collect `(setq ,(cdr lookup) ,@(cdr def))))
      (real-vars (loop for lookup in precache-syms collect `(,(car lookup) (funcall ,(cdr lookup)))))
    )
    `(let
      (
        ,@precache-vars
      )
      (cons (lambda ()
        ; Prefetch
        ,@precache-forms
      ) (lambda ()
        ; Update
        (let
          (
            ,@real-vars
          )
          ,@forms
        )
      ))
    )
  )
)

(defmacro module (name args &rest forms)
  `(defun ,name ,args
    (block nil
      (handler-bind
        (
          (error #'(lambda (cnd)
            (write-string "ERR ")
            (write-string ,(symbol-name name))
            (write-string ": ")
            (write-line (slot-value cnd 'message))
            (return nil)
          ))
          (warning #'(lambda (cnd)
            (write-string "WRN ")
            (write-string ,(symbol-name name))
            (write-string ": ")
            (write-line (slot-value cnd 'message))
            nil
          ))
        )
        ,@forms
      )
    )
  )
)
(write-line "Loading modules")
