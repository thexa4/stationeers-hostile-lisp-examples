
(defun reset-button (fail fetch update &key (name "BaseOS - Reset"))

  (let
    (
      (switch-id (device :name name))
      (switch-state-request nil)
      (switch-state nil)
    )
    (if (null switch-id)
      (return-from warning-light (funcall fail "Reset button not found" :panic t))
    )
  
    (funcall fetch (lambda ()
      (setq switch-state-request (stationeers::syscall-device-load-async switch-id logic-type:setting))
    ))

    (funcall update (lambda ()
      (block nil
        (setq switch-state (funcall switch-state-request))
        (if (null switch-state)
          (return (funcall fail "Reset button disappeared" :panic t))
        )

        (if (> switch-state 0.5)
          (funcall fail "Reset button pressed" :panic t)
        )
      )
    ))
  )
)
