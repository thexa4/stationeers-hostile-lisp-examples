

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
