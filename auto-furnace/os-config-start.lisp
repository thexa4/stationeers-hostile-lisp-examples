(defvar *operation-state* 'unknown)
(defvar *draining-rate* 0)
(defvar *next-draining-rate* 999)
(defvar *filtration-open* nil)
(defvar *next-filtration-open* nil)
(defvar *filtration-required* t)
(defvar *next-filtration-required* nil)

(write-line "Configuring modules")

(defvar *loaded-modules* (list