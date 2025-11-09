
(defun room-lights (fail fetch update name)
  (let*
    (
      (room-state t)
      (occupancy-sensor (device :name name :prefab-name "StructureOccupancySensor"))
      (quantity-request nil)
      (quantity nil)
    )
    (if (null occupancy-sensor)
      (return-from room-lights (funcall fail "occupancy sensor not found"))
    )

    (funcall fetch (lambda ()
      (setq quantity-request (stationeers::syscall-device-load-async occupancy-sensor logic-type:quantity))
    ))

    (funcall update (lambda ()
      (block nil
        (setq quantity (funcall quantity-request))
        (if (null (or quantity))
          (funcall fail "occupancy sensor disappeared")
        )

        (if (> quantity 0.5)
          (unless room-state
            (loop for id in (devices :name name) do
              (stationeers::syscall-device-store-async id logic-type:on 1)
            )
            (setq room-state t)
          )
          (when room-state
            (loop for id in (devices :name name) do
              (stationeers::syscall-device-store-async id logic-type:on 0)
            )
            (setq room-state nil)
          )
        )
      )
    ))
  )
)
