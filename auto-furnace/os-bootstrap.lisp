
; Fetch code stream
(defvar *code-stream* nil)
(set-dispatch-macro-character #\# #\% #'(lambda (s c p) (setq *code-stream* s) (values)))
#%
