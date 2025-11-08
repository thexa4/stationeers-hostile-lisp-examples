
(defun warning-light (fail fetch update &key (name "WarningLight"))
  (funcall fetch (lambda () nil))

  (let
    (
      (light-id (device :name name))
      (light-state nil)
      (blink-state nil)
    )
    (if (null light-id)
      (return-from warning-light (funcall fail "warning light not found"))
    )
    (stationeers::syscall-device-store-async light-id logic-type:on 0)

    (funcall update (lambda ()
      (if *warn-state*
        (progn
          (setq light-state t)
          (setq blink-state (not blink-state))
          (stationeers::syscall-device-store-async light-id logic-type:on (if blink-state 1 0))
        )
        (when light-state
          (stationeers::syscall-device-store-async light-id logic-type:on 0)
          (setq light-state nil)
          (setq last-warn nil)
        )
      )
    ))
  )
)