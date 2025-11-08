;; Modular base operating system.
;; Devices are configured in the block below.

(defvar *device-config* '(

;; =====================
;;  Device config below
;; =====================

          (print-warnings) ; Prints warnings to the screen
          (warning-light) ; Enables a light if there is an active warning
          (reset-button) ; Allows rescanning for devices by pressing the "BaseOS - Reset" button
          ;(monitor-area-power-charge) ; Fetches the charge of the base, needed for power saving modules
          ;(auto-solid-generator :fuel-threshold 10) ; Turns on a solid generator when power goes low
          ;(printer-idle "Autolathe") ; Idles printer and limits printing to 20 items
          ;(printer-idle "Electronics Printer")
          ;(printer-idle "Hydraulic Pipe Bender")
          ;(printer-idle "Tool Printer")
          ;(arc-furnace-excess)
          (room-lights "Room 1")
          (room-lights "Room 2")
          (room-lights "Room 3")
          (airlock "AirLock1")

;; =====================
;;  Device config above
;; =====================
))


; Set title
(write-char (code-char 27))
(write-string "]0;BaseOS v1")
(write-char (code-char 7))

(defvar *game-time* (game-time))
(defvar *base-charge* nil)
(defvar *idle-mode* t)
(defvar *warn-state* nil)

(write-line "Loading modules")

;; ===================
;;  Paste parts below
;; ===================



(defun airlock (fail fetch update prefix)
  (let
    (
      (outer-door (device :name (concatenate 'string prefix " - Outer Door")))
      (inner-door (device :name (concatenate 'string prefix " - Inner Door")))
      (outer-vent (device :name (concatenate 'string prefix " - Outer Vent")))
      (inner-vent (device :name (concatenate 'string prefix " - Inner Vent")))
      (gas-sensor (device :name prefix :prefab-name "StructureGasSensor"))
      (occupancy-sensor (device :name prefix :prefab-name "StructureOccupancySensor"))
      (proximity-sensor (device :name prefix :prefab-name "StructureProximitySensor"))
      (lamps (devices :name (concatenate 'string prefix " - Light")))

      (output-progress (devices :name (concatenate 'string prefix " - Progress")))
      (output-outer (devices :name (concatenate 'string prefix " - Is Outer")))
      (output-inner (devices :name (concatenate 'string prefix " - Is Inner")))
      (output-closed (devices :name (concatenate 'string prefix " - Is Closed")))
      (input-switches (devices :name (concatenate 'string prefix " - Switch")))
      (input-switches-inverted (devices :name (concatenate 'string prefix " - Switch Outer")))
      (input-locks (devices :name (concatenate 'string prefix " - Lock")))
      (input-poweroff (devices :name (concatenate 'string prefix " - PowerOff")))

      (state-current 'unknown)
      (state-atmosphere 'unknown)
      (state-outer-pressure 0)
      (state-inner-pressure 0)
      (state-venting nil)
      (state-last-door-change-time *game-time*)
      (state-power t)
      (state-locked nil)

      (fetched-outer-door-open nil)
      (fetched-outer-door-setting nil)
      (fetched-inner-door-open nil)
      (fetched-inner-door-setting nil)
      (fetched-inner-vent-mode nil)
      (fetched-inner-vent-on nil)
      (fetched-inner-vent-pressure-output nil)
      (fetched-outer-vent-mode nil)
      (fetched-outer-vent-on nil)
      (fetched-outer-vent-pressure-output nil)
      (fetched-gas-sensor-pressure nil)
      (fetched-occupancy-sensor-activate nil)
      (fetched-proximity-sensor-activate nil)
      (fetched-inputs-power nil)
      (fetched-inputs-lock nil)
      (fetched-inputs-switches nil)
      (fetched-inputs-switches-inverted nil)
    )
    (if (not outer-door) (return-from airlock (funcall fail "Missing outer door")))
    (if (not inner-door) (return-from airlock (funcall fail "Missing inner door")))
    (if (not outer-vent) (return-from airlock (funcall fail "Missing outer vent")))
    (if (not inner-vent) (return-from airlock (funcall fail "Missing inner vent")))
    (if (not gas-sensor) (return-from airlock (funcall fail "Missing gas sensor")))

    (if occupancy-sensor (write-line "Found occupancy sensor"))
    (if proximity-sensor (write-line "Found proximity sensor"))
    (if lamps (write-line "Found lamps"))
    (if output-progress (write-line "Found progress output"))
    (if output-outer (write-line "Found outer output"))
    (if output-inner (write-line "Found inner output"))
    (if output-closed (write-line "Found closed output"))
    (if input-switches (write-line "Found input switches"))
    (if input-switches-inverted (write-line "Found inverted input switches"))
    (if input-locks (write-line "Found input locks"))
    (if input-poweroff (write-line "Found input poweroff switches"))
    (write-line "Finishing intialization, can take up to 60 seconds.")

    (labels
      (
        (filter-nils (lst)
          (loop for x in lst when x collect x)
        )
        (initialize ()
          ; Set defaults if devices are not installed
          (if (not occupancy-sensor)
            (setq fetched-occupancy-sensor-activate (lambda () 0))
          )
          (if (not proximity-sensor)
            (setq fetched-proximity-sensor-activate (lambda () 1))
          )
        )
        (update-fetched ()
          (setq fetched-inputs-power (loop for id in input-poweroff collect (stationeers::syscall-device-load-async id logic-type:setting)))
          (setq fetched-inputs-lock (loop for id in input-locks collect (stationeers::syscall-device-load-async id logic-type:setting)))
          (setq fetched-inputs-switches (loop for id in input-switches collect (stationeers::syscall-device-load-async id logic-type:setting)))
          (setq fetched-inputs-switches-inverted (loop for id in input-switches-inverted collect (stationeers::syscall-device-load-async id logic-type:setting)))
          
          (setq fetched-outer-door-open (stationeers::syscall-device-load-async outer-door logic-type:open))
          (setq fetched-outer-door-setting (stationeers::syscall-device-load-async outer-door logic-type:setting))
          (setq fetched-inner-door-open (stationeers::syscall-device-load-async inner-door logic-type:open))
          (setq fetched-inner-door-setting (stationeers::syscall-device-load-async inner-door logic-type:setting))
          (setq fetched-inner-vent-mode (stationeers::syscall-device-load-async inner-vent logic-type:mode))
          (setq fetched-inner-vent-on (stationeers::syscall-device-load-async inner-vent logic-type:on))
          (setq fetched-inner-vent-pressure-output (stationeers::syscall-device-load-async inner-vent logic-type:pressure-output))
          (setq fetched-outer-vent-mode (stationeers::syscall-device-load-async outer-vent logic-type:mode))
          (setq fetched-outer-vent-on (stationeers::syscall-device-load-async outer-vent logic-type:on))
          (setq fetched-outer-vent-pressure-output (stationeers::syscall-device-load-async outer-vent logic-type:pressure-output))
          (setq fetched-gas-sensor-pressure (stationeers::syscall-device-load-async gas-sensor logic-type:pressure))
          (if occupancy-sensor (setq fetched-occupancy-sensor-activate (stationeers::syscall-device-load-async occupancy-sensor logic-type:quantity)))
          (if proximity-sensor (setq fetched-proximity-sensor-activate (stationeers::syscall-device-load-async proximity-sensor logic-type:quantity)))
        )
        (switch-state (state)
          (setq state-current state)

          (case state
            (inner
              (setq state-atmosphere 'inner)
              (setq state-last-door-change-time *game-time*)
              (setf (device-logic output-closed logic-type:on) 0)
              (setf (device-logic output-inner logic-type:on) 1)
              (setf (device-logic output-outer logic-type:on) 0)
              
              (setf (device-logic input-switches logic-type:open) 0)
              (setf (device-logic input-switches-inverted logic-type:open) 1)
              (setf (device-logic input-switches logic-type:lock) 0)
              (setf (device-logic input-switches-inverted logic-type:lock) 0)
              
              (setf (device-logic inner-door logic-type:mode) 0)

              (setf (device-logic output-progress logic-type:setting) 0)
            )
            (outer
              (setq state-atmosphere 'outer)
              (setq state-last-door-change-time *game-time*)
              (setf (device-logic output-closed logic-type:on) 0)
              (setf (device-logic output-inner logic-type:on) 0)
              (setf (device-logic output-outer logic-type:on) 1)
              
              (setf (device-logic input-switches logic-type:open) 1)
              (setf (device-logic input-switches-inverted logic-type:open) 0)
              (setf (device-logic input-switches logic-type:lock) 0)
              (setf (device-logic input-switches-inverted logic-type:lock) 0)
              
              (setf (device-logic outer-door logic-type:mode) 0)
              
              (setf (device-logic output-progress logic-type:setting) 1)
            )
            (closed
              (setq state-atmosphere 'outer)
              (setq state-last-door-change-time *game-time*)
              (setf (device-logic output-closed logic-type:on) 1)
              (setf (device-logic output-inner logic-type:on) 0)
              (setf (device-logic output-outer logic-type:on) 0)
              
              (setf (device-logic output-progress logic-type:setting) 0.5)
            )
            (depressurize-inner
              (setf (device-logic inner-vent logic-type:mode) 1)
              (setf (device-logic inner-vent logic-type:on) 1)
              (setf (device-logic inner-door logic-type:open) 0)
              (setf (device-logic inner-door logic-type:mode) 1)
              (setf (device-logic output-closed logic-type:on) 1)
              (setf (device-logic output-inner logic-type:on) 0)
              (setf (device-logic output-outer logic-type:on) 0)
              
              (setf (device-logic input-switches logic-type:open) 1)
              (setf (device-logic input-switches-inverted logic-type:open) 0)
              (setf (device-logic input-switches logic-type:lock) 1)
              (setf (device-logic input-switches-inverted logic-type:lock) 1)
              
              (setf (device-logic inner-door logic-type:mode) 1)
            )
            (depressurize-outer
              (setf (device-logic outer-vent logic-type:mode) 1)
              (setf (device-logic outer-vent logic-type:on) 1)
              (setf (device-logic outer-door logic-type:open) 0)
              (setf (device-logic outer-door logic-type:mode) 1)
              (setf (device-logic output-closed logic-type:on) 1)
              (setf (device-logic output-inner logic-type:on) 0)
              (setf (device-logic output-outer logic-type:on) 0)
              
              (setf (device-logic input-switches logic-type:open) 0)
              (setf (device-logic input-switches-inverted logic-type:open) 1)
              (setf (device-logic input-switches logic-type:lock) 1)
              (setf (device-logic input-switches-inverted logic-type:lock) 1)
              
              (setf (device-logic outer-door logic-type:mode) 1)
            )
            (pressurize-inner
              (setf (device-logic inner-vent logic-type:mode) 0)
              (setf (device-logic inner-vent logic-type:on) 1)
              (setf (device-logic outer-vent logic-type:on) 0)
              (setf (device-logic output-closed logic-type:on) 1)
              (setf (device-logic output-inner logic-type:on) 0)
              (setf (device-logic output-outer logic-type:on) 0)
            )
            (pressurize-outer
              (setf (device-logic outer-vent logic-type:mode) 0)
              (setf (device-logic outer-vent logic-type:on) 1)
              (setf (device-logic inner-vent logic-type:on) 0)
              (setf (device-logic output-closed logic-type:on) 1)
              (setf (device-logic output-inner logic-type:on) 0)
              (setf (device-logic output-outer logic-type:on) 0)
            )
          )

          (fresh-line)
          (write-string prefix)
          (write-string " - State ")
          (print state)
        )
        (handle-power ()
          (let*
            (
              (power-switches (await-all fetched-inputs-power))
              (power-sum (apply #'+ (loop for x in power-switches when x collect x)))
              (should-poweroff (> power-sum 0))
              (lock-switches (await-all fetched-inputs-lock))
              (lock-sum (apply #'+ (loop for x in lock-switches when x collect x)))
              (should-lock (> lock-sum 0))
              (proximity-value (funcall fetched-proximity-sensor-activate))
              (occupied-value (funcall fetched-occupancy-sensor-activate))
              (nearby (> (or proximity-value 1) 0.5))
              (occupied (> (or occupied-value 0) 0.5))
            )

            (if (not nearby) (setq should-poweroff t))
            (if state-locked (setq should-poweroff t))

            (if should-poweroff
              (when state-power
                (setf (device-logic lamps logic-type:on) 0)
                (when (or (eq state-current 'inner) (eq state-current 'outer) (eq state-current 'closed))
                  (write-string prefix)
                  (write-line " - Powering down")
                  (setq state-power nil)
                  (setf (device-logic outer-door logic-type:on) 0)
                  (setf (device-logic inner-door logic-type:on) 0)
                )
              )
              (when (not state-power)
                (write-string prefix)
                (write-line " - Powering up")
                (setq state-power t)
                (setf (device-logic lamps logic-type:on) 1)
                (setf (device-logic outer-door logic-type:on) 1)
                (setf (device-logic inner-door logic-type:on) 1)
              )
            )

            (if should-lock
              (when (not occupied)
                (unless state-locked
                  (write-string prefix)
                  (write-line " - Locking")
                  (stationeers::syscall-device-store-async outer-door logic-type:open 0)
                  (stationeers::syscall-device-store-async inner-door logic-type:open 0)
                  (setq state-current 'closed)
                  (setf (device-logic output-closed logic-type:on) 1)
                  (setf (device-logic output-inner logic-type:on) 0)
                  (setf (device-logic output-outer logic-type:on) 0)
                  (setf (device-logic outer-door logic-type:lock) 1)
                  (setf (device-logic inner-door logic-type:lock) 1)
                  
                  (setq state-locked t)
                )
              )
              (when state-locked
                (write-string prefix)
                (write-line " - Unlocking")
                (setf (device-logic outer-door logic-type:lock) 0)
                (setf (device-logic inner-door logic-type:lock) 0)
                (setf (device-logic outer-door logic-type:mode) 1)
                (setf (device-logic inner-door logic-type:mode) 1)
                (setq state-locked nil)
              )
            )
          )
        )
        (handle-state-unknown ()
          ; We just booted and don't know in what state our doors are
          
          (stationeers::syscall-device-store-async outer-door logic-type:setting 0)
          (stationeers::syscall-device-store-async inner-door logic-type:setting 0)

          (let
            (
              (inner-open (= 1 (funcall fetched-inner-door-open)))
              (outer-open (= 1 (funcall fetched-outer-door-open)))
            )

            (setf (device-logic outer-door logic-type:lock) 0)
            (setf (device-logic inner-door logic-type:lock) 0)
            (setf (device-logic outer-door logic-type:on) 1)
            (setf (device-logic inner-door logic-type:on) 1)
            (setf (device-logic outer-door logic-type:mode) 1)
            (setf (device-logic inner-door logic-type:mode) 1)
            (setf (device-logic outer-vent logic-type:lock) 1)
            (setf (device-logic inner-vent logic-type:lock) 1)
            (setf (device-logic outer-vent logic-type:on) 0)
            (setf (device-logic inner-vent logic-type:on) 0)
            (setf (device-logic lamps logic-type:on) 1)

            (if inner-open
              (switch-state 'inner)
              (if outer-open
                (switch-state 'outer)
                (switch-state 'closed)
              )
            )

            (if (eq state-current 'inner)
              (setf (device-logic outer-door logic-type:open) 0)
            )
            (if (eq state-current 'outer)
              (setf (device-logic inner-door logic-type:open) 0)
            )
          )
        )
        (handle-state-inner ()
          (let*
            (
              (inner-setting (- 1 (or (funcall fetched-inner-door-open) 1)))
              (outer-setting (funcall fetched-outer-door-setting))
              (door-pressed (/= 0 (or inner-setting 0) (or outer-setting 0)))
              (inner-vent-pressure (funcall fetched-inner-vent-pressure-output))
              (gas-pressure (funcall fetched-gas-sensor-pressure))
              (should-vent-pressure (> inner-vent-pressure (* 2 state-inner-pressure)))
              (switch-inputs (filter-nils (await-all fetched-inputs-switches)))
              (switch-inputs-inverted (filter-nils (await-all fetched-inputs-switches-inverted)))
              (switch-changed (or (apply #'/= 0 0 switch-inputs) (apply #'/= 1 1 switch-inputs-inverted)))
            )

            ; Empty active vent pipes when we're idle
            (if state-venting
              (progn
                (unless should-vent-pressure
                  (write-string prefix)
                  (write-line " - venting done")
                  (setf (device-logic inner-vent logic-type:on) 0)
                  (setq state-venting nil)
                )
                (return-from handle-state-inner nil)
              )
              (when should-vent-pressure
                (write-string prefix)
                (write-line " - releasing inner vent pressure")
                (setf (device-logic inner-vent logic-type:mode) 0)
                (setf (device-logic inner-vent logic-type:on) 1)
                (setq state-venting t)
                (return-from handle-state-inner nil)
              )
            )
            
            (if switch-changed (setq door-pressed t))

            ; Handle doors
            (if door-pressed
              (let
                (
                  (door-interval (- *game-time* state-last-door-change-time))
                )

                (if (> door-interval 30)
                  (setq state-inner-pressure (or gas-pressure state-inner-pressure))
                )

                (switch-state 'depressurize-inner)
              )
            )
          )
        )
        (handle-state-outer ()
          (let*
            (
              (inner-setting (funcall fetched-inner-door-setting))
              (outer-setting (- 1 (or (funcall fetched-outer-door-open) 1)))
              (door-pressed (/= 0 (or inner-setting 0) (or outer-setting 0)))
              (outer-vent-pressure (funcall fetched-outer-vent-pressure-output))
              (gas-pressure (funcall fetched-gas-sensor-pressure))
              (should-vent-pressure (> outer-vent-pressure (* 2 state-outer-pressure)))
              (switch-inputs (filter-nils (await-all fetched-inputs-switches)))
              (switch-inputs-inverted (filter-nils (await-all fetched-inputs-switches-inverted)))
              (switch-changed (or (apply #'/= 1 1 switch-inputs) (apply #'/= 0 0 switch-inputs-inverted)))
            )

            ; Empty active vent pipes when we're idle
            (if state-venting
              (progn
                (unless should-vent-pressure
                  (write-string prefix)
                  (write-line " - venting done")
                  (setf (device-logic outer-vent logic-type:on) 0)
                  (setq state-venting nil)
                )
                (return-from handle-state-inner nil)
              )
              (when should-vent-pressure
                (write-string prefix)
                (write-line " - releasing outer vent pressure")
                (setf (device-logic outer-vent logic-type:mode) 0)
                (setf (device-logic outer-vent logic-type:on) 1)
                (setq state-venting t)
                (return-from handle-state-inner nil)
              )
            )

            (if switch-changed (setq door-pressed t))

            ; Handle doors
            (if door-pressed
              (let
                (
                  (door-interval (- *game-time* state-last-door-change-time))
                )

                (if (> door-interval 30)
                  (setq state-outer-pressure (or gas-pressure state-outer-pressure))
                )

                (switch-state 'depressurize-outer)
              )
            )
          )
        )
        (handle-state-closed ()
          (let*
            (
              (inner-setting (funcall fetched-inner-door-setting))
              (outer-setting (funcall fetched-outer-door-setting))
              (inner-pressed (/= 0 inner-setting))              
              (outer-pressed (/= 0 outer-setting))              
            )

            (when inner-pressed
              (if (eq state-atmosphere 'inner)
                (progn
                  (setf (device-logic inner-door logic-type:open) 1)
                  (switch-state 'inner)
                )
                (switch-state 'depressurize-outer)
              )
            )
            (when outer-pressed
              (if (eq state-atmosphere 'inner)
                (switch-state 'depressurize-inner)
                (progn
                  (setf (device-logic outer-door logic-type:open) 1)
                  (switch-state 'outer)
                )
              )
            )
          )
        )
        (handle-state-depressurize-inner ()
          (stationeers::syscall-device-store-async outer-door logic-type:setting 0)
          (stationeers::syscall-device-store-async inner-door logic-type:setting 0)
          (let*
            (
              (pressure-orig (funcall fetched-gas-sensor-pressure))
              (pressure (or pressure-orig 1000))
              (progress (/ pressure (+ state-inner-pressure 0.1)))
            )

            (if (null pressure-orig)
              (print "Null pressure reading, did gas sensor disappear?")
            )
            ; 0 -> 0.5
            (setf (device-logic output-progress logic-type:setting) (- 0.5 (* progress 0.5)))

            (when (= 0 pressure)
              (switch-state 'pressurize-outer)
            )
          )
        )
        (handle-state-depressurize-outer ()
          (stationeers::syscall-device-store-async outer-door logic-type:setting 0)
          (stationeers::syscall-device-store-async inner-door logic-type:setting 0)
          (let*
            (
              (pressure-orig (funcall fetched-gas-sensor-pressure))
              (pressure (or pressure-orig 1000))
              (progress (/ pressure (+ state-outer-pressure 0.1)))
            )

            (if (null pressure-orig)
              (print "Null pressure reading, did gas sensor disappear?")
            )
            ; 1 -> 0.5
            (setf (device-logic output-progress logic-type:setting) (+ 0.5 (* progress 0.5)))

            (when (= 0 pressure)
              (switch-state 'pressurize-inner)
            )
          )
        )
        (handle-state-pressurize-inner ()
          (stationeers::syscall-device-store-async outer-door logic-type:setting 0)
          (stationeers::syscall-device-store-async inner-door logic-type:setting 0)
          (let*
            (
              (pressure-target (- state-inner-pressure 30))
              (current-pressure (funcall fetched-gas-sensor-pressure))
              (available-pressure (funcall fetched-inner-vent-pressure-output))
              (progress (/ current-pressure (+ state-inner-pressure 0.1)))
            )
            
            ; 0.5 -> 0
            (setf (device-logic output-progress logic-type:setting) (- 0.5 (* progress 0.5)))
            
            (when (or (= available-pressure 0) (> current-pressure pressure-target))
              (setf (device-logic inner-vent logic-type:on) 0)
              (setf (device-logic inner-door logic-type:open) 1)
              (switch-state 'inner)
            )
          )
        )
        (handle-state-pressurize-outer ()
          (stationeers::syscall-device-store-async outer-door logic-type:setting 0)
          (stationeers::syscall-device-store-async inner-door logic-type:setting 0)
          (let*
            (
              (pressure-target (- state-outer-pressure 30))
              (current-pressure (funcall fetched-gas-sensor-pressure))
              (available-pressure (funcall fetched-outer-vent-pressure-output))
              (progress (/ current-pressure (+ state-inner-pressure 0.1)))
            )
            
            ; 0.5 -> 1
            (setf (device-logic output-progress logic-type:setting) (+ (* progress 0.5) 0.5))
            
            (when (or (= available-pressure 0) (> current-pressure pressure-target))
              (setf (device-logic outer-vent logic-type:on) 0)
              (setf (device-logic outer-door logic-type:open) 1)
              (switch-state 'outer)
            )
          )
        )
      )
      (initialize)

      (funcall fetch #'update-fetched)
      (funcall update (lambda ()
        (handle-power)
        (if state-power
          (case state-current
            (unknown (handle-state-unknown))
            (closed (handle-state-closed))
            (inner (handle-state-inner))
            (outer (handle-state-outer))
            (depressurize-inner (handle-state-depressurize-inner))
            (depressurize-outer (handle-state-depressurize-outer))
            (pressurize-inner (handle-state-pressurize-inner))
            (pressurize-outer (handle-state-pressurize-outer))
          )
        )
      ))
    )
  )
)


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
          (funcall fail "arc furnacek disappeared")
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


(defun arc-furnace-excess (fail fetch update &key (name "") (threshold 0.7))
  (let*
    (
      (arc-furnace (device :name name :prefab-name "StructureArcFurnace"))
      (input-amount-request nil)
      (turned-on-request nil)
      (input-amount nil)
      (turned-on nil)
    )
    (if (null arc-furnace)
      (return-from arc-furnace-excess (funcall fail "no arc furnace found"))
    )
    (stationeers::syscall-device-store-async arc-furnace logic-type:on 0)

    (funcall fetch (lambda ()
      (setq input-amount-request (stationeers::syscall-device-slot-load-async arc-furnace 0 logic-slot-type:quantity))
      (setq turned-on-request (stationeers::syscall-device-load-async arc-furnace logic-type:on))
    ))

    (funcall update (lambda ()
      (block nil
        (setq input-amount (funcall input-amount-request))
        (setq turned-on (funcall turned-on-request))
        (if (null (or input-amount turned-on))
          (funcall fail "arc furnace disappeared")
        )
        (setq turned-on (> turned-on 0.5))

        (if (< *base-charge* threshold)
          (if turned-on
            (stationeers::syscall-device-store-async arc-furnace logic-type:on 0)
          )
          (if turned-on
            (if (< input-amount 0.5)
              (stationeers::syscall-device-store-async arc-furnace logic-type:on 0)
            )
            (when (> input-amount 0.5)
              (stationeers::syscall-device-store-async arc-furnace logic-type:on 1)
              (stationeers::syscall-device-store-async arc-furnace logic-type:activate 1)
            )
          )
        )
      )
    ))
  )
)

(defun reset-button (fail fetch update &key (name "BaseOS - Reset"))

  (let
    (
      (switch-id (device :name name))
      (switch-state-request nil)
      (switch-state nil)
    )
    (if (null switch-id)
      (return-from warning-light (funcall fail "Reset button not found" :panic t))
    )
  
    (funcall fetch (lambda ()
      (setq switch-state-request (stationeers::syscall-device-load-async switch-id logic-type:setting))
    ))

    (funcall update (lambda ()
      (setq switch-state (funcall switch-state-request))
      (if (null switch-state)
        (return (funcall fail "Reset button disappeared" :panic t))
      )

      (if (> switch-state 0.5)
        (funcall fail "Reset button pressed" :panic t)
      )
    ))
  )
)


(defun printer-idle (fail fetch update name &key (power-threshold 0.05) (idle-time 60) (max-items 20))
  (let*
    (
      (printer-id (device :name name))
      (printer-on-request nil)
      (printer-export-count-request nil)
      (printer-activate-request nil)
      (printer-on nil)
      (export-count nil)
      (printer-active nil)
      (last-off *game-time*)
    )
    (if (null printer-id)
      (return-from printer-idle (funcall fail "printer not found"))
    )

    (funcall fetch (lambda ()
      (setq printer-on-request (stationeers::syscall-device-load-async printer-id logic-type:on))
      (setq printer-export-count-request (stationeers::syscall-device-load-async printer-id logic-type:export-count))
      (setq printer-activate-request (stationeers::syscall-device-load-async printer-id logic-type:activate))
    ))

    (funcall update (lambda ()
      (block nil
        (setq printer-on (funcall printer-on-request))
        (setq export-count (funcall printer-export-count-request))
        (setq printer-active (funcall printer-activate-request))
        (if (null (or printer-on export-count printer-active))
          (return (funcall fail "printer disappeared"))
        )
        (setq printer-on (> printer-on 0.5))
        (setq printer-active (> printer-active 0.5))

        (if power-threshold
          (if (<= *base-charge* power-threshold)
            (unless printer-on
              (stationeers::syscall-device-store-async printer-id logic-type:on 1)
            )
          )
        )

        (when idle-time
          (if (and printer-on (not printer-active))
            (when (> (- *game-time* last-off) idle-time)
              (stationeers::syscall-device-store-async printer-id logic-type:on 0)
              (stationeers::syscall-device-store-async printer-id logic-type:open 0)
            )
            (setq last-off *game-time*)
          )
        )

        (when max-items
          (unless (or printer-on (not printer-active))
            (if (> max-items 0)
              (stationeers::syscall-device-store-async printer-id logic-type:clear-memory 1)
            )
          )

          (when (>= export-count max-items)
            (stationeers::syscall-device-store-async printer-id logic-type:on 0)
            (stationeers::syscall-device-store-async printer-id logic-type:clear-memory 1)
          )
        )
      )
    ))
  )
)


(defun warning-light (fail fetch update &key (name "WarningLight"))
  (funcall fetch (lambda () nil))

  (let
    (
      (light-id (device :name name))
      (light-state nil)
      (blink-state nil)
    )
    (if (null light-id)
      (return-from warning-light (funcall fail "warning light not found"))
    )
    (stationeers::syscall-device-store-async light-id logic-type:on 0)

    (funcall update (lambda ()
      (if *warn-state*
        (progn
          (setq light-state t)
          (setq blink-state (not blink-state))
          (stationeers::syscall-device-store-async light-id logic-type:on (if blink-state 1 0))
        )
        (when light-state
          (stationeers::syscall-device-store-async light-id logic-type:on 0)
          (setq light-state nil)
          (setq last-warn nil)
        )
      )
    ))
  )
)

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


(defun print-warnings (fail fetch update)
  (funcall fetch (lambda () nil))

  (let
    (
      (last-warn nil)
    )
    (funcall update (lambda ()
      (if *warn-state*
        (unless (string= (cdr *warn-state*) (cdr last-warn))
          (setq last-warn *warn-state*)
          (print (car last-warn))
          (write-string " warn: ")
          (write-line (cdr last-warn))          
        )
        (setq last-warn nil)
      )
    ))
  )
)


(defun auto-solid-generator (fail fetch update &key (name "") (threshold 0.05) (min-charge 0.1) (fuel-threshold 5))
  (let*
    (
      (coal-generator (device :name name :prefab-name "StructureSolidFuelGenerator"))
      (fuel-amount-request nil)
      (turned-on-request nil)
      (fuel-amount nil)
      (turned-on nil)
    )
    (if (null coal-generator)
      (return-from auto-solid-generator (funcall fail "no solid generator found"))
    )

    (funcall fetch (lambda ()
      (setq fuel-amount-request (stationeers::syscall-device-slot-load-async coal-generator 0 logic-slot-type:quantity))
      (setq turned-on-request (stationeers::syscall-device-load-async coal-generator logic-type:on))
    ))

    (funcall update (lambda ()
      (block nil
        (setq fuel-amount (funcall fuel-amount-request))
        (setq turned-on (funcall turned-on-request))
        (if (null (or fuel-amount turned-on))
          (funcall fail "solid generator disappeared")
        )
        (setq turned-on (> turned-on 0.5))

        (if (<= *base-charge* threshold)
          (unless turned-on
            (stationeers::syscall-device-store-async coal-generator logic-type:on 1)
          )
          (if (>= *base-charge* min-charge)
            (when turned-on
              (stationeers::syscall-device-store-async coal-generator logic-type:on 0)
            )
          )
        )

        (if (< fuel-amount fuel-threshold)
          (funcall fail "generator fuel low" :warn T)
        )
      )
    ))
  )
)



;; ===================
;;  Paste parts above
;; ===================

(tagbody
load-devices
  (write-line "Loading devices")
  (let*
    (
      (device-enumeration *device-config*)
      (failed nil)
      (current-module nil)
      (current-update nil)
      (current-fetch nil)
      (module-list (cons nil nil))
      (update-list (cons nil nil))
      (fetch-list (cons nil nil))
      (module-tail module-list)
      (update-tail update-list)
      (fetch-tail fetch-list)
      (iteration-warn nil)
      (is-init t)
      (init-func nil)
    )

    (labels
      (
        (fail-func (msg &key panic warn)
          (when panic
            (write-char (code-char 27))
            (write-string "[0m")
            (write-char (code-char 27))
            (write-string "[1;31m")
            
            (fresh-line)
            (write-string (symbol-name (car current-module)))
            (write-string " panic: ")
            (write-line msg)

            (write-char (code-char 27))
            (write-string "[0m")
            (fresh-line)
            (write-line "Retrying intialization in 5 seconds")
            (sleep 5)
            (go load-devices)
          )

          (when warn
            (setq iteration-warn (or iteration-warn (cons current-module msg)))
            
            (return-from fail-func nil)
          )

          (setq failed t)
          (when is-init
            (write-string "Failed, skipping: ")
            (write-line msg)
            (return-from fail-func nil)
          )
          (write-line "Module failed, restarting")
          (sleep 1)
          (go load-devices)
        )
        (fetch-func (func)
          (rplacd fetch-tail (cons func nil))
          (setq fetch-tail (cdr fetch-tail))
          (hostile-lisp:function-compile func)
        )
        (update-func (func)
          (rplacd update-tail (cons func nil))
          (setq update-tail (cdr update-tail))
          (hostile-lisp:function-compile func)
        )
      )
      (loop for device in device-enumeration do
        (rplacd module-tail (cons device nil))
        (setq module-tail (cdr module-tail))
        (setq failed nil)

        (setq init-func (symbol-function (car device)))
        (write-string "Initializing ")
        (print device)
        (setq current-module device)
        (apply init-func #'fail-func #'fetch-func #'update-func (cdr device))
        (when failed
          (fetch-func (lambda () nil))
          (update-func (lambda () nil))
        )
      )
      (nconc fetch-list (list nil nil))
      (loop
        (setq module-tail (cdr module-list))
        (setq fetch-tail (cdr fetch-list))
        (setq update-tail (cdr update-list))
        (setq iteration-warn nil)

        (loop repeat 2 do
          (if fetch-tail
            (funcall (car fetch-tail))
          )
          (setq fetch-tail (cdr fetch-tail))
        )

        (loop
          for module in module-tail
          for fetch in fetch-tail
          for update in update-tail
          do
          (setq current-module module)
          (setq failed nil)
          (funcall update)
          (if fetch (funcall fetch))
        )
        (setq *warn-state* iteration-warn)
        (when is-init
          (setq is-init nil)
          (write-line "Initialization completed")
        )
        (setq *game-time* (game-time))
      )
    )
  )
)
