
(defun arc-furnace-excess (fail fetch update &key (name "") (threshold 0.05))
  (let*
    (
      (arc-furnace (device :name name :prefab-name "StructureArcFurnace"))
      (input-amount-request nil)
      (turned-on-request nil)
      (input-amount nil)
      (turned-on nil)
    )
    (if (null arc-furnace)
      (return-from arc-furnace-excess (funcall fail "no arc furnace found"))
    )
    (stationeers::syscall-device-store-async arc-furnace logic-type:on 0)

    (funcall fetch (lambda ()
      (setq input-amount-request (stationeers::syscall-device-slot-load-async arc-furnace 0 logic-slot-type:quantity))
      (setq turned-on-request (stationeers::syscall-device-load-async arc-furnace logic-type:on))
    ))

    (funcall update (lambda ()
      (block nil
        (setq input-amount (funcall input-amount-request))
        (setq turned-on (funcall turned-on-request))
        (if (null (or input-amount turned-on))
          (funcall fail "arc furnace disappeared")
        )
        (setq turned-on (> turned-on 0.5))

        (if (< *base-charge* threshold)
          (if turned-on
            (stationeers::syscall-device-store-async arc-furnace logic-type:on 0)
          )
          (if turned-on
            (if (< input-amount 0.5)
              (stationeers::syscall-device-store-async arc-furnace logic-type:on 0)
            )
            (when (> input-amount 0.5)
              (stationeers::syscall-device-store-async arc-furnace logic-type:on 1)
              (stationeers::syscall-device-store-async arc-furnace logic-type:activate 1)
            )
          )
        )
      )
    ))
  )
)