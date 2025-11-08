

(defun auto-solid-generator (fail fetch update &key (name "") (threshold 0.1) (min-charge 0.2) (fuel-threshold 5))
  (let*
    (
      (coal-generator (device :name name :prefab-name "StructureSolidFuelGenerator"))
      (fuel-amount-request nil)
      (turned-on-request nil)
      (fuel-amount nil)
      (turned-on nil)
    )
    (if (null coal-generator)
      (return-from auto-solid-generator (funcall fail "no solid generator found"))
    )

    (funcall fetch (lambda ()
      (setq fuel-amount-request (stationeers::syscall-device-slot-load-async coal-generator 0 logic-slot-type:quantity))
      (setq turned-on-request (stationeers::syscall-device-load-async coal-generator logic-type:on))
    ))

    (funcall update (lambda ()
      (block nil
        (setq fuel-amount (funcall fuel-amount-request))
        (setq turned-on (funcall turned-on-request))
        (if (null (or fuel-amount turned-on))
          (funcall fail "solid generator disappeared")
        )
        (setq turned-on (> turned-on 0.5))

        (if (<= *base-charge* threshold)
          (unless turned-on
            (stationeers::syscall-device-store-async coal-generator logic-type:on 1)
          )
          (if (>= *base-charge* min-charge)
            (when turned-on
              (stationeers::syscall-device-store-async coal-generator logic-type:on 0)
            )
          )
        )

        (if (< fuel-amount fuel-threshold)
          (funcall fail "generator fuel low" :warn T)
        )
      )
    ))
  )
)