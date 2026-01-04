(module room-pressure-control (&key
    (target-moles 0)
  )
  (let
    (
      (gas-sensor (device :name "Furnace - Gas Sensor"))
      (vent-in (device :name "Furnace - Box Outward"))
      (vent-out (device :name "Furnace - Box Inward"))
      
    )
    (unless gas-sensor
      (error "Missing 'Furnace - Gas Sensor'")
    )
    (unless vent-in
      (error "Missing 'Furnace - Smart Vent In'")
    )
    (unless vent-out
      (error "Missing 'Furnace - Smart Vent Out'")
    )
    
    ; State reset
    (setf (device-logic gas-sensor logic-type:power) 1)

    (setf (device-logic vent-in logic-type:mode) 0)
    (setf (device-logic vent-in logic-type:lock) 1)
    (setf (device-logic vent-in logic-type:power) 0)
    
    (setf (device-logic vent-out logic-type:mode) 1)
    (setf (device-logic vent-out logic-type:lock) 1)
    (setf (device-logic vent-out logic-type:power) 0)

    ; Infer state
    ; Ony assume pressurized state if room is at target pressure
    (setq *room-pressure-goal* (if (< (device-logic gas-sensor logic-type:total-moles) target-moles) 'vacuum 'pressurized))

    (prefetch
      (
        (room-moles (stationeers::syscall-device-load-async gas-sensor logic-type:total-moles))
        (room-pressure (stationeers::syscall-device-load-async gas-sensor logic-type:pressure))
        (vent-in-on (stationeers::syscall-device-load-async vent-in logic-type:on))
        (vent-out-on (stationeers::syscall-device-load-async vent-out logic-type:on))
      )

      (let*
        (
          (is-in-on (> vent-in-on 0.5))
          (is-out-on (> vent-out-on 0.5))

          (at-low-atmos (< room-moles target-moles))
          (is-pressurizing (and (eq *room-pressure-goal* 'pressurized) at-low-atmos))
          (is-not-vaccuum (/= room-pressure 0))
          (is-depressurizing (and (eq *room-pressure-goal* 'vacuum) is-not-vaccuum))
        )

        (when is-pressurizing 
          (stationeers::syscall-device-store-async vent-in logic-type:pressure-external (+ room-pressure 1))  
        )

        (ensure-power vent-in is-in-on is-pressurizing)
        (ensure-power vent-out is-out-on is-depressurizing)
      )
    )
  )
)
