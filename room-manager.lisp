;; Room Manager
;;
;; Automatically turns devices on and off depending on occupancy
;;
;; Mandatory configuration:
;;  - Name your occupancy sensor "Room 1"
;;  - Name your lights "Room 1"
;;
;; Optional configuration:
;;  - Add devices in your room that you call "Room 1 - NoOn", those will only be turned off, not on automatically
;;


(defvar *game-time* (game-time))
(defvar *idle-mode* t)

(defun turn-room-on (prefix)
  (let*
    (
      (room-device-ids (devices :name prefix))
      (room-device-names (device-name room-device-ids))
      (no-noon-device-ids (loop for id in room-device-ids for name in room-device-names when (string= name prefix) collect id))
    )
    (setf (device-logic no-noon-device-ids logic-type:on) 1)
  )
)

(defun new-room (prefix)
  (let*
    (
      (occupancy-sensor (device :name prefix :prefab-name "StructureOccupancySensor"))
      (room-state t)
    )
    (if (null occupancy-sensor) (error (concatenate 'string "Unable to find room occupancy sensor named '" prefix "'")))

    (write-string "Initializing ")
    (write-line prefix)
    (turn-room-on prefix)

    (lambda ()
      (let*
        (
          (occupied-value (or (device-logic occupancy-sensor logic-type:quantity) 1))
          (is-occupied (> occupied-value 0.5))
        )

        (if room-state
          (when (not is-occupied)
            (setf (device-logic (devices :name prefix) logic-type:on) 0)
            (setq room-state nil)
            (write-string prefix)
            (write-line " is empty")
          )
          (when is-occupied
            (turn-room-on prefix)
            (setq room-state t)
            (write-string prefix)
            (write-line " is occupied")
          )
        )
      )
    )
  )
)

(let
  (
    (room1 (new-room "Room 1"))
    ;(room2 (new-room "Room 2"))
    ;(room3 (new-room "Room 3"))
  )
  (loop
    (setq *game-time* (game-time))
    (setq *idle-mode* t)
    (funcall room1)
    ;(funcall room2)
    ;(funcall room3)

    (if *idle-mode*
      (sleep 0.3)
    )
  )
)
