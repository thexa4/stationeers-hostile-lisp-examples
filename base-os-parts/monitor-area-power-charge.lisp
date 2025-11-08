

(defun monitor-area-power-charge (fail fetch update &key (name ""))
  (let*
    (
      (power-control (or (device :name name :prefab-name "StructureAreaPowerControl") (device :name name :prefab-name "StructureAreaPowerControlReversed")))
      (charge-request nil)
    )

    (if (null power-control)
      (return-from monitor-area-power-charge (funcall fail "Area Power Control not found" :panic T))
    )

    (funcall fetch (lambda ()
      (setq charge-request (stationeers::syscall-device-load-async power-control logic-type:ratio))
    ))

    (funcall update (lambda ()
      (setq *base-charge* (funcall charge-request))
      (if (null *base-charge*)
        (funcall fail "Power controller disappeared" :panic T)
      )
    ))
  )
)