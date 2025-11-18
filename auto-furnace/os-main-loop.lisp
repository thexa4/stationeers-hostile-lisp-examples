
(defvar *prefetch-depth* 3)

(write-line "Starting control loop")

; TODO: handler-bind
(setq *loaded-modules* (loop for module in *loaded-modules* if module collect module))
(loop
  (let
    (
      (cur-prefetch *loaded-modules*)
    )

    ; Prefetch a few modules
    (loop repeat *prefetch-depth* do
      (when cur-prefetch
        (funcall (car (car cur-prefetch)))
        (setq cur-prefetch (cdr cur-prefetch))
      )
    )

    (loop for module in *loaded-modules* do
      ; Update
      (funcall (cdr module))

      ; Do next prefetch
      (when cur-prefetch
        (funcall (car (car cur-prefetch)))
        (setq cur-prefetch (cdr cur-prefetch))
      )
    )
    (setq *draining-rate* (if (> *next-draining-rate* 1) 0 *next-draining-rate*))
    (setq *next-draining-rate* 999)
    (setq *filtration-open* *next-filtration-open*)
    (setq *next-filtration-open* nil)
    (setq *filtration-required* *next-filtration-required*)
    (setq *next-filtration-required* nil)
  )
)

(write-string "Bye")