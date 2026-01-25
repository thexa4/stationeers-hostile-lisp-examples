(defvar *operation-state* 'unknown)
(defvar *draining-rate* 0)
(defvar *next-draining-rate* 999)
(defvar *filtration-open* nil)
(defvar *next-filtration-open* nil)
(defvar *filtration-required* t)
(defvar *next-filtration-required* nil)
(defvar *room-pressure-goal*  10)
(defvar *operation-state* 'cold)

(write-line "Configuring modules")

(defvar *loaded-modules* (list