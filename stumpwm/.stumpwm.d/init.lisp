(in-package :stumpwm)

(defvar *stumpwm-config-dir*  "~/.stumpwm.d/"
  "StumpWM configuration directory.")
(set-module-dir (concat *stumpwm-config-dir* "contrib"))

(load (concat *stumpwm-config-dir* "utils.lisp"))

(load-mods '("quickload" "startup" "theme" "modeline")) ;; missing swank

(set-prefix-key (kbd "s-t"))

(defcommand reinit () ()
            (run-commands "reload" "loadrc"))

(defcommand firefox () ()
            (run-or-raise "firefox" '(:class "firefox")))

(defcommand emacs () ()
            "raise emacs if there is a running instance, otherwise start it"
            (run-or-raise "emacsclient -c -a emacs" '(:title "emacs"))) 

(defvar *applications-map* nil
  "keymap for applications")

(defkeys *root-map*
    ("f" "colon1 exec firefox http://www.")
  ("r" "reinit")
  ("R" "resize"))

(defkeys *applications-map*
    ("f" "firefox")
  ("t" "exec urxvtc"))

(defkeys *top-map*
    ("s-TAB" "fnext")
  ("M-TAB" "next-in-frame")
  ("s-m" "mode-line")
  ("s-e" "emacs")
  ("s-space" "exec rofi -show run")
  ("s-p" '*applications-map*)
  ("s-s" "exec scrot -s ~/foo.png && xclip ~/foo.png && rm ~/foo.png"))
