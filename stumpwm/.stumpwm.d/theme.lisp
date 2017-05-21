;; Sarcasm personnal theme for StumpWM
;; usage (with user-module):
;; (load-user-module "sarcasm-theme")

(in-package :stumpwm)

(defvar *wallpaper-command* "nitrogen --restore"
  "Wallpaper command.")

;; Use Xcursor theme
(run-shell-command "xsetroot -cursor_name left_ptr -fg white -bg black")

(run-shell-command *wallpaper-command*)
(ql:quickload :clx-truetype)

(load-module "ttf-fonts")

(setq clx-truetype:*font-dirs* '("/run/current-system/sw/share/X11-fonts/"))
(clx-truetype:cache-fonts)

(set-font (make-instance 'xft:font :family "Droid Sans Mono" :subfamily
                         "Regular" :size 10))

(set-focus-color "#01FF70")
(set-unfocus-color "#3D9970")


(setf *maxsize-border-width* 2)
(setf *transient-border-width* 2)
(setf *normal-border-width* 2)

(set-fg-color "#EEEEEE")
(set-bg-color "#3D9970")
(set-msg-border-width 0)
(set-border-color "#000000")

(setf *message-window-padding* 50)
(setf *message-window-gravity* :top
      *input-window-gravity* :top)
