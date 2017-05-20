;; Sarcasm personnal theme for StumpWM
;; usage (with user-module):
;; (load-user-module "sarcasm-theme")

(in-package :stumpwm)

(defvar *wallpaper-command* "nitrogen --restore"
  "Wallpaper command.")

;; Use Xcursor theme
(run-shell-command "xsetroot -cursor_name left_ptr -fg white -bg black")

(run-shell-command *wallpaper-command*)

;; Font found with the command xfontsel(1)
;; When a font is installed, add is directory to the fontPath then run xfontsel
;; xset +fp /usr/share/fonts/X11/misc/
(set-font "-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859-*")

(set-fg-color "grey95")
(set-bg-color "grey15")
(set-win-bg-color "black")
(set-border-color "grey10")
(set-focus-color "grey72")
(set-unfocus-color "grey30")
