;; Unpowered airlock
;;
;; Only consumes power when people are nearby.
;; Automatically releases pressure behind the active vents.
;; Only requires a few pipes connected to the active vent, no need for a cowl or passive vent.
;;
;; Mandatory configuration:
;;  - Name your outer door "AirLock1 - Outer Door"
;;  - Name your inner door "AirLock1 - Inner Door"
;;  - Name your inner vent "AirLock1 - Inner Vent"
;;  - Name your outer vent "AirLock1 - Outer Vent"
;;  - Name your gas sensor "AirLock1"
;;
;; Optional configuration:
;;  - Name your proximity sensor "AirLock1"
;;  - Name your occupancy sensor "AirLock1"
;;  - Name your lamp sensor "AirLock1 - Light"
;;
;; Optional input / output:
;;  - "AirLock1 - Switch": Controls the direction of the airlock
;;  - "AirLock1 - Switch Outer": Controls the direction of the airlock (inverted)
;;  - "AirLock1 - Progress": Displays airlock switch progress
;;  - "AirLock1 - Is Outer": Is on when airlock is outside
;;  - "AirLock1 - Is Inner": Is on when airlock is inside
;;  - "AirLock1 - Is Closed": Is on when both airlock doors are closed
;;  - "AirLock1 - PowerOff": If any device has setting == 1, will keep power off with doors unlocked
;;  - "AirLock1 - Lock": If any device has setting == 1, will close both doors and power down 


(defparameter *atmosphere-settle-interval* 30)
(defparameter *allowed-pressure-delta* 30)


(defvar *game-time* (game-time))
(defvar *idle-mode* t)

(defun filter-nils (lst)
  (loop for x in lst when x collect x)
)
(hostile-lisp:function-compile #'filter-nils)

(defun new-airlock (prefix)
  (if (null (device :name prefix)) (return-from new-airlock nil))
  (write-string "Initializing ")
  (write-line prefix)
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
    (if (not outer-door) (error "Missing outer door"))
    (if (not inner-door) (error "Missing inner door"))
    (if (not outer-vent) (error "Missing outer vent"))
    (if (not inner-vent) (error "Missing inner vent"))
    (if (not gas-sensor) (error "Missing gas sensor"))

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
                (setq state-locked nil)
              )
            )
          )
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
        (handle-state-unknown ()
          ; We just booted and don't know in what state our doors are

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

                (if (> door-interval *atmosphere-settle-interval*)
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

                (if (> door-interval *atmosphere-settle-interval*)
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
          (let*
            (
              (pressure-target (- state-inner-pressure *allowed-pressure-delta*))
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
          (let*
            (
              (pressure-target (- state-outer-pressure *allowed-pressure-delta*))
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
      (lambda ()
        (update-fetched)

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
        (setf (device-logic outer-door logic-type:setting) 0)
        (setf (device-logic inner-door logic-type:setting) 0)

        (setf *idle-mode* (and *idle-mode* (not state-power)))
      )
    )
  )
)

(let
  (
    (airlock1 (new-airlock "AirLock1"))
    ;(airlock2 (new-airlock "AirLock2"))
    ;...
  )
  (loop
    (setq *game-time* (game-time))
    (setq *idle-mode* t)
    (funcall airlock1)
    ;(funcall airlock2)
    ;...

    (if *idle-mode*
      (sleep 0.3)
    )
  )
)
