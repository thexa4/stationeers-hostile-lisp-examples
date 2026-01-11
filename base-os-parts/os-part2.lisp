

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
(defvar *warn-state* nil)
(defvar *idle-mode* t)

(write-line "Loading modules")

;; ===================
;;  Paste parts below
;; ===================