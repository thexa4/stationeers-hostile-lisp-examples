;; Modular base operating system.
;; Devices are configured in the block below.

(defvar *code-stream* nil)
(set-dispatch-macro-character #\# #\% #'(lambda (s c p) (setq *code-stream* s) (values)))
#%


(defvar *device-config* '(

;; =====================
;;  Device config below
;; =====================