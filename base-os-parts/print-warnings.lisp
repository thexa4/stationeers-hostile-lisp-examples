
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