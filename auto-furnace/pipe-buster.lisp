
(module pipe-burster (&key (target-pressure 10000) (box-pressure-max 100))
  (let*
    (
      (pressurizer (or (device :name "Boiler - Pipe Buster") (device :name "Boiler - Pipe Buster - Done")))
      (pressurizer-name (device-name pressurizer))
      (box-pressure-sensor (device :name "Boiler - Box Sensor"))
      (box-pressure (device-logic box-pressure-sensor logic-type:pressure))
    )
    (unless pressurizer
      (error "Missing 'Boiler - Pipe Buster'")
    )
    (unless box-pressure-sensor
      (error "Missing 'Boiler - Box Sensor'")
    )
    (setf (device-logic pressurizer logic-type:on) 0)
    (setf (device-logic pressurizer logic-type:setting) 0)

    (unless (string= pressurizer-name "Boiler - Pipe Buster - Done")
      (if (> box-pressure box-pressure-max)
        (error "Box pressure is too high")
      )

      (warn "Starting pipe burst")
      (let
        (
          (cooling-manager (cooling-loop))
        )
        (setf (device-logic pressurizer logic-type:setting) target-pressure)
        (setf (device-logic pressurizer logic-type:on) 1)

        (tagbody
        start
          ; Prefetch
          (funcall (car cooling-manager))
          (setq box-pressure (device-logic box-pressure-sensor logic-type:pressure))

          ; Run
          (funcall (cdr cooling-manager))
          
          (if (< box-pressure box-pressure-max)
            (go start)
          )
        )

        (setf (device-name pressurizer) "Boiler - Pipe Buster - Done")
        (setf (device-logic pressurizer logic-type:on) 0)
        (setf (device-logic pressurizer logic-type:setting) 0)
        (warn "Done")
      )
    )
  )
)
