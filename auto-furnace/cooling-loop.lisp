
(module cooling-loop (&key (temperature-setpoint 283) (pressure-setpoint 20000) (temperature-min 273) (pressure-min 5) (pressure-bandwidth 1000) (temperature-management-min-pressure 10))
  (let
    (
      (vent (device :name "Cooling Loop - Active Vent"))
      (drain (device :name "Cooling Loop - Drain"))
    )
    (unless vent
      (error "Missing 'Cooling Loop - Active Vent'")
    )
    (unless drain
      (error "Missing 'Cooling Loop - Drain'")
    )
    (setf (device-logic vent logic-type:on) 0)
    (setf (device-logic vent logic-type:mode) 1)
    (setf (device-logic vent logic-type:lock) 1)
    (setf (device-logic drain logic-type:on) 0)
    (setf (device-logic drain logic-type:setting) 10)

    (prefetch
      (
        (temperature (stationeers::syscall-device-load-async vent logic-type:temperature-output))
        (pressure (stationeers::syscall-device-load-async vent logic-type:pressure-output))
        (vent-active (stationeers::syscall-device-load-async vent logic-type:on))
        (drain-active (stationeers::syscall-device-load-async drain logic-type:on))
      )

      (let
        (
          (should-pressurize nil)
          (should-drain nil)
          (is-draining nil)
          (is-pressurizing nil)
          (drain-speed 0)
        )

        (setq is-draining (> drain-active 0.5))
        (setq is-pressurizing (> vent-active 0.5))

        ; Checks
        (if (> pressure pressure-setpoint)
          (setq should-drain t)
          (setq drain-speed 10)
        )
        (when (and is-draining (> pressure (- pressure-setpoint pressure-bandwidth)))
          (setq should-drain t)
          (setq drain-speed 10)
        )

        (if (< pressure pressure-min)
          (setq should-pressurize t)
        )

        (when (> pressure temperature-management-min-pressure)
          (when (< temperature temperature-min)
            (setq should-drain t)
            (setq drain-speed 0.1)
          )

          (if (> temperature temperature-setpoint)
            (setq should-pressurize t)
          )
        )

        ; Execute
        (if should-drain
          (stationeers::syscall-device-store-async drain logic-type:setting drain-speed)
        )

        (ensure-power drain is-draining should-drain)
        (ensure-power vent is-pressurizing should-pressurize)
      )
    )
  )
)
