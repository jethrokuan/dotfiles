(in-package :stumpwm)

(load-module "amixer")

(set-prefix-key (kbd "s-z"))

(defcommand urxvt () ()
            (run-or-raise "urxvtc" '(:class "URxvt")))

(defcommand reinit () ()
            (run-commands "reload" "loadrc"))

(defcommand firefox () ()
            (run-or-raise "firefox" '(:class "Firefox")))

(defcommand emacs () ()
            "raise emacs if there is a running instance, otherwise start it"
            (run-or-raise "emacsclient -c -a emacs" '(:title "emacs")))

(defcommand spotify () ()
            (run-or-raise "spotify" '(:class "Spotify")))

(defvar *applications-map* nil
  "keymap for applications")

(defkeys *root-map*
    ("r" "reinit")
  ("R" "resize"))

(defkeys *applications-map*
    ("f" "firefox")
  ("t" "urxvt")
  ("s" "spotify"))

(defkeys *top-map*
    ("XF86AudioLowerVolume" "exec amixer -q sset Master 3%-")
  ("XF86AudioRaiseVolume" "exec amixer -q sset Master 3%+")
  ("XF86AudioMute" "exec amixer -q sset Master toggle")
  ("s-1" "gselect code")
  ("s-2" "gselect www")
  ("s-3" "gselect media")
  ("s-4" "gselect chat")
  ("s-TAB" "fnext")
  ("s-ISO_Left_Tab" "gnext")
  ("M-TAB" "next-in-frame")
  ("M-ISO_Left_Tab" "prev-in-frame")
  ("s-m" "mode-line")
  ("s-e" "emacs")
  ("s-x" "urxvt")
  ("s-space" "exec rofi -show run")
  ("s-a" '*applications-map*)
  ("SunPrint_Screen" "exec scrot -s ~/tmp.png && xclip -selection c -t image/png -i ~/tmp.png && rm ~/tmp.png")
  ("C-SunPrint_Screen" "scrot -m ~/scrot.png")
  
  ("s-h" "move-focus left")
  ("s-t" "move-focus down")
  ("s-n" "move-focus up")
  ("s-s" "move-focus right")

  ("s-H" "move-window left")
  ("s-T" "move-window down")
  ("s-N" "move-window up")
  ("s-S" "move-window right")
  )
