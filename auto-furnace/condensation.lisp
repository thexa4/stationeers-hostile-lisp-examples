
(module condensation (&key
    (temperature-bandwidth 17)
    (temperature-setpoint 276)
    (pressure-setpoint 3000)
    (pressure-bandwidth 200)
  )
  (let
    (
      (storage-sensor (device :name "Condenser - Storage Sensor"))
      (drain (device :name "Condenser - Drain"))
      (water-sensor (device :name "Condenser - Water Sensor"))
      (water-overflow (device :name "Condenser - Water Overflow"))
    )
    (unless storage-sensor
      (error "Missing 'Condenser - Storage Sensor'")
    )
    (unless drain
      (error "Missing 'Condenser - Drain'")
    )
    (unless water-sensor
      (error "Missing 'Condenser - Water Sensor'")
    )
    (unless water-overflow
      (error "Missing 'Condenser - Water Overflow'")
    )

    ; State reset
    (setf (device-logic drain logic-type:on) 0)
    (setf (device-logic drain logic-type:setting) 10)
    (setf (device-logic water-overflow logic-type:on) 0)
    (setf (device-logic water-overflow logic-type:setting) 90)
    (setf (device-logic storage-sensor logic-type:on) 0)
    (setf (device-logic storage-sensor logic-type:lock) 1)
    (setf (device-logic water-sensor logic-type:on) 0)
    (setf (device-logic water-sensor logic-type:lock) 1)

    (prefetch
      (
        (storage-temperature (stationeers::syscall-device-load-async storage-sensor logic-type:temperature-output))
        (storage-pressure (stationeers::syscall-device-load-async storage-sensor logic-type:pressure-output))
        (water-pressure (stationeers::syscall-device-load-async water-sensor logic-type:pressure-output))
        (water-temperature (stationeers::syscall-device-load-async water-sensor logic-type:temperature-output))
        (water-purity (stationeers::syscall-device-load-async water-sensor logic-type:ratio-water-output))
        (water-moles (stationeers::syscall-device-load-async water-sensor logic-type:total-moles-output))

        (drain-on (stationeers::syscall-device-load-async drain logic-type:on))
        (water-overflow-on (stationeers::syscall-device-load-async water-overflow logic-type:on))
      )

      (let*
        (
          (is-draining (> drain-on 0.5))
          (is-water-overflowing (> water-overflow-on 0.5))
          (is-storage-pressure-high (> storage-pressure pressure-setpoint))
          (is-storage-pressure-too-high (> storage-pressure (+ pressure-setpoint pressure-bandwidth)))
          (temperature-ratio (+ 1 (/ (- temperature-setpoint storage-temperature) temperature-bandwidth)))

          (should-drain (and is-storage-pressure-too-high *filtration-open* (> temperature-ratio 0)))
          (should-water-overflow (and (> water-moles 3240) (>= water-purity 1)))
        )

        (if (> temperature-ratio 1) (setq temperature-ratio 1))
        (if (< temperature-ratio 0) (setq temperature-ratio 0))

        (if is-storage-pressure-too-high (setq temperature-ratio 0))
        (setq *next-draining-rate* temperature-ratio)
        (if (= storage-pressure 0) (setq *next-draining-rate* 1))

        (unless (or *filtration-required* (eq *operation-state* 'hot))
          (setq should-drain (and *filtration-open* (> temperature-ratio 0) (> storage-pressure 0)))
        )

        (ensure-power drain is-draining should-drain)
        (ensure-power water-overflow is-water-overflowing should-water-overflow)
      )
    )
  )
)
