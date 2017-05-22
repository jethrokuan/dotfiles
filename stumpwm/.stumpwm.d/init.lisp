(in-package :stumpwm)

(defvar *stumpwm-config-dir*  "~/.stumpwm.d/"
  "StumpWM configuration directory.")
(set-module-dir (concat *stumpwm-config-dir* "contrib"))

(load (concat *stumpwm-config-dir* "utils.lisp"))

(load-mods '("quickload" "startup" "theme" "modeline" "keys" "groups")) ;; missing swank

