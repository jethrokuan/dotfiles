(in-package :stumpwm)

(load-module "amixer")

(set-prefix-key (kbd "s-t"))

(defcommand urxvt () ()
            (run-commands "exec urxvtc"))

(defcommand reinit () ()
            (run-commands "reload" "loadrc"))

(defcommand firefox () ()
            (run-or-raise "firefox" '(:class "Firefox")))

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
  ("t" "urxvt"))

(defkeys *top-map*
    ("XF86AudioLowerVolume" "exec amixer -q sset Master 3%-")
  ("XF86AudioRaiseVolume" "exec amixer -q sset Master 3%+")
  ("XF86AudioMute" "exec amixer -q sset Master toggle")
  ("s-TAB" "fnext")
  ("s-ISO_Left_Tab" "gnext")
  ("M-TAB" "next-in-frame")
  ("s-m" "mode-line")
  ("s-e" "emacs")
  ("s-space" "exec rofi -show run")
  ("s-p" '*applications-map*)
  ("s-s" "exec scrot -s ~/tmp.png && xclip -selection c -t image/png -i ~/tmp.png && rm ~/tmp.png"))
