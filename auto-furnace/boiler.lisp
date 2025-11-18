
(module boiler (&key
    (steam-pressure-setpoint 1000)
    (steam-pressure-bandwidth 50)
    (steam-temperature-setpoint 2273.15)
    (exhaust-pressure-setpoint 2300)
    (exhaust-pressure-bandwidth 100)
    (condenser-max-pressure 3100)
    (max-fuel-pressure 100)
    (fuel-ratio 33.33333)
    (hot-state-threshold 500)
  )
  (let
    (
      (steam-feeder (device :name "Boiler - Steam Feeder"))
      (box-sensor (device :name "Boiler - Box Sensor"))
      (combustor (device :name "Boiler - Combustor"))
      (furnace (device :name "Boiler - Furnace"))
      (fuel-mixer (device :name "Fuel - Mixer"))
      (steam-drain (device :name "Boiler - Steam Drain"))
      (exhaust-drain (device :name "Boiler - Exhaust Drain"))
      (condenser-water-sensor (device :name "Condenser - Water Sensor"))
      (steam-drain-pwm (pwm 0))
      (exhaust-drain-pwm (pwm 3))
    )
    (unless steam-feeder
      (error "Missing 'Boiler - Steam Feeder'")
    )
    (unless box-sensor
      (error "Missing 'Boiler - Box Sensor'")
    )
    (unless combustor
      (error "Missing 'Boiler - Combustor'")
    )
    (unless furnace
      (error "Missing 'Boiler - Furnace'")
    )
    (unless steam-drain
      (error "Missing 'Boiler - Steam Drain'")
    )
    (unless exhaust-drain
      (error "Missing 'Boiler - Exhaust Drain'")
    )
    (unless fuel-mixer
      (error "Missing 'Fuel - Mixer'")
    )
    (unless condenser-water-sensor
      (error "Missing 'Condenser - Water Sensor'")
    )

    ; State reset
    (setf (device-logic combustor logic-type:on) 0)
    (setf (device-logic steam-feeder logic-type:on) 0)
    (setf (device-logic steam-feeder logic-type:setting) 1)
    (setf (device-logic fuel-mixer logic-type:on) 0)
    (setf (device-logic fuel-mixer logic-type:setting) fuel-ratio)
    (setf (device-logic steam-drain logic-type:on) 0)
    (setf (device-logic steam-drain logic-type:setting) 10)
    (setf (device-logic exhaust-drain logic-type:on) 0)
    (setf (device-logic exhaust-drain logic-type:setting) 10)

    ; Infer state
    (setq *operation-state* (if (< (device-logic box-sensor logic-type:temperature) hot-state-threshold) 'cold 'hot))

    (when (eq *operation-state* 'cold)
      (warn "Starting furnace drain")
    )

    (prefetch
      (
        (box-temperature (stationeers::syscall-device-load-async box-sensor logic-type:temperature))
        (box-pressure (stationeers::syscall-device-load-async box-sensor logic-type:pressure))
        (exhaust-temperature (stationeers::syscall-device-load-async furnace logic-type:temperature))
        (exhaust-pressure (stationeers::syscall-device-load-async furnace logic-type:pressure))
        (fuel-temperature (stationeers::syscall-device-load-async combustor logic-type:temperature-input))
        (fuel-pressure (stationeers::syscall-device-load-async combustor logic-type:pressure-input))
        (fuel-oxygen-ratio (stationeers::syscall-device-load-async combustor logic-type:ratio-oxigen-input))
        (fuel-volatiles-ratio (stationeers::syscall-device-load-async combustor logic-type:ratio-volatiles-input))

        (condenser-water-level (stationeers::syscall-device-load-async condenser-water-sensor logic-type:volume-of-liquid))
        (condenser-pollutant-level (stationeers::syscall-device-load-async condenser-water-sensor logic-type:ratio-liquid-pollutant))

        (combustor-on (stationeers::syscall-device-load-async combustor logic-type:on))
        (fuel-mixer-on (stationeers::syscall-device-load-async fuel-mixer logic-type:on))
        (steam-feeder-on (stationeers::syscall-device-load-async steam-feeder logic-type:on))
        (steam-drain-on (stationeers::syscall-device-load-async steam-drain logic-type:on))
        (exhaust-drain-on (stationeers::syscall-device-load-async exhaust-drain logic-type:on))
      )

      (let*
        (
          (is-combusting (> combustor-on 0.5))
          (is-feeding-steam (> steam-feeder-on 0.5))
          (is-mixing-fuel (> fuel-mixer-on 0.5))
          (is-draining-steam (> steam-drain-on 0.5))
          (is-draining-exhaust (> exhaust-drain-on 0.5))

          (box-pressure-ratio (/ box-pressure steam-pressure-setpoint))
          (box-temperature-ratio (/ box-temperature steam-temperature-setpoint))
          (is-fuel-available (> fuel-pressure 1))
          (is-fuel-overpressured (> fuel-pressure max-fuel-pressure))
          (is-steam-temperature-low (< box-temperature steam-temperature-setpoint))
          (is-exhaust-pressure-low (< exhaust-pressure (+ exhaust-pressure-setpoint exhaust-pressure-bandwidth)))
          (is-exhaust-pressure-high (>= exhaust-pressure exhaust-pressure-setpoint))
          (is-steam-pressure-high (> box-pressure (+ steam-pressure-setpoint steam-pressure-bandwidth)))

          (should-burn-fuel (and is-steam-temperature-low is-exhaust-pressure-low is-fuel-available))
          (should-mix-fuel (and is-steam-temperature-low is-exhaust-pressure-low (not is-fuel-overpressured)))
          (should-feed-steam (< box-pressure-ratio box-temperature-ratio))
          (steam-drain-pwm-outcome (funcall steam-drain-pwm *draining-rate*))
          (exhaust-drain-pwm-outcome (funcall exhaust-drain-pwm *draining-rate*))
          (should-drain-steam (and steam-drain-pwm-outcome is-steam-pressure-high))
          (should-drain-exhaust (and exhaust-drain-pwm-outcome is-exhaust-pressure-high))
        )

        (when (eq *operation-state* 'cold)
          (ensure-power combustor is-combusting nil)
          (ensure-power steam-feeder is-feeding-steam nil)
          (ensure-power fuel-mixer is-mixing-fuel nil)
          
          (setq *next-filtration-required* (or *next-filtration-required* (> box-pressure 0) (> exhaust-pressure 0)))
          
          (ensure-power steam-drain is-draining-steam (and steam-drain-pwm-outcome (> box-pressure 0)))
          (ensure-power exhaust-drain is-draining-exhaust (and exhaust-drain-pwm-outcome (> exhaust-pressure 0)))
        )

        (when (eq *operation-state* 'hot)
          (setq *next-filtration-required* t)
          (ensure-power combustor is-combusting should-burn-fuel)
          (ensure-power fuel-mixer is-mixing-fuel should-mix-fuel)
          (ensure-power steam-feeder is-feeding-steam should-feed-steam)

          (ensure-power steam-drain is-draining-steam should-drain-steam)
          (ensure-power exhaust-drain is-draining-exhaust should-drain-exhaust)
        )
      )
    )
  )
)
