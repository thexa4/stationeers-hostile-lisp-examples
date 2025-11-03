;; Power Saving
;; 
;; - Automatically shuts down idle printers
;; - Automatic load shedding once battery becomes critical
;; - Will automatically enable coal generator if battery is too low


(write-line "Power Control v0.1")
(write-string "Device enumeration")

(defvar coal-generator (device :prefab-name "StructureSolidFuelGenerator"))
(if (null coal-generator) (print "No coal generator found."))
(write-char #\.)

(defvar autolathe (cons (device :prefab-name "StructureAutolathe") nil))
(if (null (car autolathe)) (print "No autolathe found."))
(write-char #\.)

(defvar tool-printer (cons (device :prefab-name "StructureToolManufactory") nil))
(if (null (car tool-printer)) (print "No tool printer found."))
(write-char #\.)

(defvar electronics-printer (cons (device :prefab-name "StructureElectronicsPrinter") nil))
(if (null (car electronics-printer)) (print "No electronics printer found."))
(write-char #\.)

(defvar pipe-bender (cons (device :prefab-name "StructureHydraulicPipeBender") nil))
(if (null (car pipe-bender)) (print "No pipe bender found."))
(write-char #\.)

(defvar power-control (or (device :prefab-name "StructureAreaPowerControl") (device :prefab-name "StructureAreaPowerControlReversed")))
(if (null power-control) (error "No area power control found."))
(write-char #\.)

(defvar arc-furnace (device :prefab-name "StructureArcFurnace"))
(if (null arc-furnace) (error "No arc furnace found."))
(write-char #\.)

(defvar ice-crusher (device :prefab-name "StructureIceCrusher"))
(if (null arc-furnace) (error "No ice crusher found."))
(write-char #\.)

(terpri)
(write-line "Discovering inputs and outputs:")
(defvar display-battery-level (devices :name "AutoPower - Charge"))
(write-string "Battery level outputs found: ")
(print (length display-battery-level))
(defvar display-shed-10 (devices :name "AutoPower - Shed 10"))
(write-string "Shed 10 outputs found: ")
(print (length display-shed-10))
(defvar display-shed-50 (devices :name "AutoPower - Shed 50"))
(write-string "Shed 50 outputs found: ")
(print (length display-shed-50))
(defvar display-excess-90 (devices :name "AutoPower - Excess 90"))
(write-string "Excess 90 outputs found: ")
(print (length display-excess-90))
(defvar display-arc-furnace (devices :name "AutoPower - Arc Furnace"))
(write-string "Arc furnace outputs found: ")
(print (length display-arc-furnace))
(defvar display-backup-power (devices :name "AutoPower - Backup Power"))
(write-string "Backup power outputs found: ")
(print (length display-backup-power))
(defvar display-ice-crusher (devices :name "AutoPower - Ice Crusher"))
(write-string "Ice crusher outputs found: ")
(print (length display-ice-crusher))
(defvar input-interrupt (device :name "AutoPower - Interrupt"))
(if input-interrupt
  (write-line "Found interrupt button")
)


(terpri)
(defvar shed-devices-50 (devices :name "Shed 50 -"))
(if shed-devices-50 (write-line "Found 50% shed devices"))
(defvar shed-devices-10 (devices :name "Shed 10 -"))
(if shed-devices-10 (write-line "Found 10% shed devices"))
(defvar excess-devices-90 (devices :name "Excess 90 -"))
(if excess-devices-90 (write-line "Found 90% excess devices"))

(nconc shed-devices-10 display-shed-10)
(nconc shed-devices-50 display-shed-50)
(nconc excess-devices-90 display-excess-90)

; Precache setf power
(defun set-power (ids value)
  (if (null ids)
    (return-from set-power nil)
  )
  (setf (device-logic ids logic-type:on) value)
)
(set-power (device) 1)
(write-char #\.)


(defun shutdown-printers (states)
  (set-power (mapcar #'car states) 0)
)
(shutdown-printers nil)
(write-char #\.)


(defun shutdown-idle-printers (states time)
  (let*
    (
      (machines (mapcar #'car states))
      (off-states (= 0 (device-logic machines logic-type:on)))
      (active-states (= 1 (device-logic machines logic-type:activate)))
      (null-states (null (mapcar #'cdr states)))
    )
    (loop for machine-state in states 
      for is-off in off-states
      for is-active in active-states
      for is-null in null-states do 
      if (or is-active is-off is-null)
        (rplacd  machine-state time)
        (when (< 30 (- time (cdr  machine-state)))
          (device-store (car machine-state) logic-type:on 0)
          (rplacd machine-state time)
        )
    )
  )
)
;(shutdown-idle-printers nil 0)
(write-char #\.)

; REPL
(defun raw-read-line ()
  "Read a line on a raw terminal."
  (let ((buf nil)
        (done nil))
    (tagbody
     start
       (let* (
         (char (read-char *standard-input*))
         (byte (char-code char))
        )
         (cond
           ;; Enter (CR or LF) ends the line
           ((or (= byte 13) (= byte 10))
            (fresh-line)
            (setq done t))
           ;; DEL or ^H : backspace
           ((= byte 8)
              ;; remove last char from buffer
              (setq buf (cdr buf))
              ;; erase on screen
              (write-string "\b \b"))
           ;; Ctrl-C / Ctrl-D abort
           ((or (= byte 3) (= byte 4))
             (write-char #\c)
            (fresh-line)
            (setq done t))
           ;; Anything printable goes into the buffer
           ((>= byte 32)
            (setq buf (cons char buf))
            (write-char char)
           )
         )
       )
       (unless done (go start))
    )

    (apply (function concatenate) (cons 'string (reverse (mapcar (function string) buf))))
  )
)

(defun repl ()
  (let 
    (
      (- nil)
      (* nil)
    )
    (tagbody
    start
      (handler-bind
        (
          (condition (lambda (condition)
            (fresh-line)
            (print (slot-value condition 'message))
            (go on-error)
          ))
        )
        (setq - (raw-read-line))
        (if (string= - "continue") (go exit))
        (setq - (make-string-input-stream -))
        (setq - (read -))
        (setq * (eval -))
        (print *)
        (go start)
      )
    on-error
      (print "Error during eval")
      (go start)
    exit
    )
  )
)
(hostile-lisp:function-compile #'repl)
(write-char #\.)


(terpri)

(let
  (
    (base-charge 1)
    (time 0)
    (printer-states (loop for state in (list autolathe tool-printer electronics-printer pipe-bender) when (car state) collect state))
  )

  (loop
    (setq time (game-time))
    (setq base-charge (device-logic power-control logic-type:ratio))

    (if (< base-charge 0.2)
      (shutdown-printers printer-states)
    )

    (when coal-generator
      (when (< base-charge 0.1)
        (set-power coal-generator 1)
        (set-power display-backup-power 1)
      )
      (when (> base-charge 0.2)
        (set-power coal-generator 0)
        (set-power display-backup-power 0)
      )
    )
    (if shed-devices-10
      (set-power shed-devices-10 (if (< base-charge 0.1) 1 0))
    )
    (if shed-devices-50
      (set-power shed-devices-50 (if (< base-charge 0.5) 1 0))
    )
    (if excess-devices-90
      (set-power excess-devices-90 (if (< base-charge 0.9) 0 1))
    )

    (when arc-furnace
      (if (< base-charge 0.6)
        (setf (device-logic arc-furnace logic-type:on) 0)
        (setf (device-logic display-arc-furnace logic-type:on) 0)
      )
      (if (> base-charge 0.7)
        (if (< 0 (device-slot-logic arc-furnace 0 logic-slot-type:occupied))
          (progn
            (setf (device-logic arc-furnace logic-type:on) 1)
            (setf (device-logic arc-furnace logic-type:activate) 1)
            (setf (device-logic display-arc-furnace logic-type:on) 1)
          )
          (progn
            (setf (device-logic arc-furnace logic-type:on) 0)
            (setf (device-logic display-arc-furnace logic-type:on) 0)
          )
        )
      )
    )
    (write-string "Base charge: ")
    (print base-charge)
    (setf (device-logic display-battery-level logic-type:setting) base-charge)
    (terpri)

    (when (> (or (device-logic input-interrupt logic-type:activate) 0) 0)
      (write-line "Interrupted, type a command or type 'continue' to continue:")
      (repl)
    )
  )
)
