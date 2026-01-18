          (print-warnings) ; Prints warnings to the screen
          (warning-light) ; Enables a light if there is an active warning
          (reset-button) ; Allows rescanning for devices by pressing the "BaseOS - Reset" button
          (monitor-area-power-charge) ; Fetches the charge of the base, needed for power saving modules
          (auto-solid-generator :fuel-threshold 10) ; Turns on a solid generator when power goes low
          ;(printer-idle "Autolathe") ; Idles printer and limits printing to 20 items
          ;(printer-idle "Electronics Printer")
          ;(printer-idle "Hydraulic Pipe Bender")
          ;(printer-idle "Tool Printer")
          (arc-furnace-excess)
          ;(room-lights "Room 1")
          ;(room-lights "Room 2")
          ;(room-lights "Room 3")
          (airlock "AirLock1")