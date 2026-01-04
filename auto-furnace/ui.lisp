
(defun graphics-mode-on ()
  "Switch to graphics screen buffer"
  (write-char (code-char 27))
  (write-string "[?1049h")
)

(defun set-cursor-pos (x y)
  "Set console cursor pos"
  (write-char (code-char 27))
  (write-char #\[)
  (write-string (write-to-string y))
  (write-char #\;)
  (write-string (write-to-string x))
  (write-char #\H)
)

(defun hide-cursor ()
  (write-char (code-char 27))
  (write-string "[?25l")
)

(module ui ()
  (let
    (
      (box-sensor (device :name "Boiler - Box Sensor"))
      (furnace (device :name "Boiler - Furnace"))
      (text-buffer-active t)
      (has-char nil)
      (last-char nil)
    )
    (unless box-sensor
      (error "Missing 'Boiler - Box Sensor'")
    )

    (prefetch
      (
        (box-temperature (stationeers::syscall-device-load-async box-sensor logic-type:temperature))
        (box-pressure (stationeers::syscall-device-load-async box-sensor logic-type:pressure))
        (furnace-temperature (stationeers::syscall-device-load-async furnace logic-type:temperature))
        (furnace-pressure (stationeers::syscall-device-load-async furnace logic-type:pressure))
        (furnace-content (stationeers::syscall-device-load-async furnace logic-type:recipe-hash))
        (furnace-open-state (stationeers::syscall-device-load-async furnace logic-type:open))
      )

      (setq has-char (listen))
      (tagbody
        start
        (when has-char
          (setq last-char (read-char))
          (setq has-char (listen))
          (go start)
        )
      )

      (when text-buffer-active
        (setq text-buffer-active nil)
        (hide-cursor)
        (graphics-mode-on)
      )

      (when (eql last-char #\!)
        (setq last-char nil)
        (setq *operation-state* (if (eq *operation-state* 'cold)
          'hot
          'cold
        ))
      )

      (when (eql last-char #\e)
        (setq last-char nil)
        (if (> furnace-open-state 0.5)
          (stationeers::syscall-device-store-async furnace logic-type:open 0)
          (stationeers::syscall-device-store-async furnace logic-type:open 1)
        )
      )

      (set-cursor-pos 1 1)
      (write-string "Operating mode: ")
      (if (eq *operation-state* 'cold)
        (write-line "Cold   ")
        (write-line "Hot    ")
      )

      (write-line "Steam box:")
      (write-string (write-to-string (round box-temperature)))
      (write-line " K       ")
      (write-string (write-to-string (round box-pressure)))
      (write-line " kPa      ")
      (terpri)
      
      (write-line "Boiler furnace:")
      (write-string (write-to-string (round furnace-temperature)))
      (write-line " K       ")
      (write-string (write-to-string (round furnace-pressure)))
      (write-line " kPa      ")
      (terpri)

      (write-string "Furnace content: ")
      (write-string (write-to-string furnace-content))
      (if (> furnace-open-state 0.5)
        (write-line " (Open)            ")
        (write-line " (Close)           ")
      )
      (terpri)
      
      (write-string "Press ! to switch modes: ")
      (if last-char
        (write-char last-char)
      )
      (write-line "      ")
    )
  )
)
