
(module filtration
  (&key
    (buffer-max-pressure 1000)
    (buffer-min-pressure 50)
    (filtration-epsilon 0.01)
    (gas-max-pressure 19500)
  )
  (let
    (
      (drain (device :name "Filtration - Drain"))
      (filter-co2 (device :name "Filtration - CO2"))
      (filter-volatiles (device :name "Filtration - V"))
      (filter-nitrogen (device :name "Filtration - N"))

      (state 'buffering)
    )
    (unless drain
      (error "Missing 'Filtration - Drain'")
    )
    (unless filter-co2
      (error "Missing 'Filtration - CO2'")
    )
    (unless filter-nitrogen
      (error "Missing 'Filtration - N'")
    )
    (unless filter-volatiles
      (error "Missing 'Filtration - V'")
    )

    (setf (device-logic drain logic-type:on) 0)
    (setf (device-logic drain logic-type:setting) 10)
    (setf (device-logic filter-co2 logic-type:on) 0)
    (setf (device-logic filter-nitrogen logic-type:on) 0)
    (setf (device-logic filter-volatiles logic-type:on) 0)

    (prefetch
      (
        (buffer-pressure (stationeers::syscall-device-load-async filter-co2 logic-type:pressure-input))
        (drain-active (stationeers::syscall-device-load-async drain logic-type:on))
        (co2-pressure (stationeers::syscall-device-load-async filter-co2 logic-type:pressure-output))
        (co2-active (stationeers::syscall-device-load-async filter-co2 logic-type:on))
        (co2-filter0-remaining (stationeers::syscall-device-slot-load-async filter-co2 0 logic-slot-type:quantity))
        (co2-filter1-remaining (stationeers::syscall-device-slot-load-async filter-co2 1 logic-slot-type:quantity))
        (co2-ratio (stationeers::syscall-device-load-async filter-co2 logic-type:ratio-carbon-dioxide-input))
        (nitrogen-pressure (stationeers::syscall-device-load-async filter-nitrogen logic-type:pressure-output))
        (nitrogen-active (stationeers::syscall-device-load-async filter-nitrogen logic-type:on))
        (nitrogen-filter0-remaining (stationeers::syscall-device-slot-load-async filter-nitrogen 0 logic-slot-type:quantity))
        (nitrogen-filter1-remaining (stationeers::syscall-device-slot-load-async filter-nitrogen 1 logic-slot-type:quantity))
        (nitrogen-ratio (stationeers::syscall-device-load-async filter-co2 logic-type:ratio-nitrogen-input))
        (volatiles-pressure (stationeers::syscall-device-load-async filter-volatiles logic-type:pressure-output))
        (volatiles-active (stationeers::syscall-device-load-async filter-volatiles logic-type:on))
        (volatiles-filter0-remaining (stationeers::syscall-device-slot-load-async filter-volatiles 0 logic-slot-type:quantity))
        (volatiles-filter1-remaining (stationeers::syscall-device-slot-load-async filter-volatiles 1 logic-slot-type:quantity))
        (volatiles-ratio (stationeers::syscall-device-load-async filter-co2 logic-type:ratio-volatiles-input))
      )

      (let*
        (
          (has-buffer-space-available (< buffer-pressure buffer-max-pressure))
          (is-buffer-empty (< buffer-pressure buffer-min-pressure))
          (is-draining (> drain-active 0.5))

          (co2-has-filter (> (+ co2-filter0-remaining co2-filter1-remaining) 0))
          (co2-has-room (< co2-pressure gas-max-pressure))
          (is-filtering-co2 (> co2-active 0.5))

          (nitrogen-has-filter (> (+ nitrogen-filter0-remaining nitrogen-filter1-remaining) 0))
          (nitrogen-has-room (< nitrogen-pressure gas-max-pressure))
          (is-filtering-nitrogen (> nitrogen-active 0.5))
          
          (volatiles-has-filter (> (+ volatiles-filter0-remaining volatiles-filter1-remaining) 0))
          (volatiles-has-room (< volatiles-pressure gas-max-pressure))
          (is-filtering-volatiles (> volatiles-active 0.5))

          (highest-ratio (max co2-ratio nitrogen-ratio volatiles-ratio))

          (reference-ratio (- highest-ratio filtration-epsilon))
          (should-filter-co2 (and (> co2-ratio reference-ratio) co2-has-room co2-has-filter))
          (should-filter-nitrogen (and (> nitrogen-ratio reference-ratio) nitrogen-has-room nitrogen-has-filter))
          (should-filter-volatiles (and (> volatiles-ratio reference-ratio) volatiles-has-room volatiles-has-filter))

          (filtration-done (< reference-ratio 0))
        )

        (case state
          (buffering
            (if has-buffer-space-available
              (setq *next-filtration-open* t)
              (setq state 'filtering)
            )
          )
          (filtering
            (ensure-power filter-co2 is-filtering-co2 should-filter-co2)
            (ensure-power filter-nitrogen is-filtering-nitrogen should-filter-nitrogen)
            (ensure-power filter-volatiles is-filtering-volatiles should-filter-volatiles)

            (when filtration-done
              (ensure-power filter-co2 is-filtering-co2 nil)
              (ensure-power filter-nitrogen is-filtering-nitrogen nil)
              (ensure-power filter-volatiles is-filtering-volatiles nil)

              (setq state 'draining)
            )
          )
          (draining
            (ensure-power drain is-draining (not is-buffer-empty))
            
            (if is-buffer-empty (setq state 'buffering))
          )
        )
      )
    )
  )
)
