
(defun printer-idle (fail fetch update name &key (power-threshold 0.05) (idle-time 60) (max-items 20))
  (let*
    (
      (printer-id (device :name name))
      (printer-on-request nil)
      (printer-export-count-request nil)
      (printer-activate-request nil)
      (printer-on nil)
      (export-count nil)
      (printer-active nil)
      (last-off *game-time*)
    )
    (if (null printer-id)
      (return-from printer-idle (funcall fail "printer not found"))
    )

    (funcall fetch (lambda ()
      (setq printer-on-request (stationeers::syscall-device-load-async printer-id logic-type:on))
      (setq printer-export-count-request (stationeers::syscall-device-load-async printer-id logic-type:export-count))
      (setq printer-activate-request (stationeers::syscall-device-load-async printer-id logic-type:activate))
    ))

    (funcall update (lambda ()
      (block nil
        (setq printer-on (funcall printer-on-request))
        (setq export-count (funcall printer-export-count-request))
        (setq printer-active (funcall printer-activate-request))
        (if (null (or printer-on export-count printer-active))
          (return (funcall fail "printer disappeared"))
        )
        (setq printer-on (> printer-on 0.5))
        (setq printer-active (> printer-active 0.5))

        (if power-threshold
          (if (<= *base-charge* power-threshold)
            (unless printer-on
              (stationeers::syscall-device-store-async printer-id logic-type:on 1)
            )
          )
        )

        (when idle-time
          (if (and printer-on (not printer-active))
            (when (> (- *game-time* last-off) idle-time)
              (stationeers::syscall-device-store-async printer-id logic-type:on 0)
              (stationeers::syscall-device-store-async printer-id logic-type:open 0)
            )
            (setq last-off *game-time*)
          )
        )

        (when max-items
          (unless (or printer-on (not printer-active))
            (if (> max-items 0)
              (stationeers::syscall-device-store-async printer-id logic-type:clear-memory 1)
            )
          )

          (when (>= export-count max-items)
            (stationeers::syscall-device-store-async printer-id logic-type:on 0)
            (stationeers::syscall-device-store-async printer-id logic-type:clear-memory 1)
          )
        )
      )
    ))
  )
)