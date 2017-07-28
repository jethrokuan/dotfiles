(in-package :stumpwm)

(setq zenburn-fg+1      "#FFFFEF")
(setq zenburn-fg        "#DCDCCC")
(setq zenburn-fg-1      "#656555")
(setq zenburn-bg-2      "#000000")
(setq zenburn-bg-1      "#2B2B2B")
(setq zenburn-bg-05     "#383838")
(setq zenburn-bg        "#3F3F3F")
(setq zenburn-bg+05     "#494949")
(setq zenburn-bg+1      "#4F4F4F")
(setq zenburn-bg+2      "#5F5F5F")
(setq zenburn-bg+3      "#6F6F6F")
(setq zenburn-red+1     "#DCA3A3")
(setq zenburn-red       "#CC9393")
(setq zenburn-red-1     "#BC8383")
(setq zenburn-red-2     "#AC7373")
(setq zenburn-red-3     "#9C6363")
(setq zenburn-red-4     "#8C5353")
(setq zenburn-orange    "#DFAF8F")
(setq zenburn-yellow    "#F0DFAF")
(setq zenburn-yellow-1  "#E0CF9F")
(setq zenburn-yellow-2  "#D0BF8F")
(setq zenburn-green-1   "#5F7F5F")
(setq zenburn-green     "#7F9F7F")
(setq zenburn-green+1   "#8FB28F")
(setq zenburn-green+2   "#9FC59F")
(setq zenburn-green+3   "#AFD8AF")
(setq zenburn-green+4   "#BFEBBF")
(setq zenburn-cyan      "#93E0E3")
(setq zenburn-blue+1    "#94BFF3")
(setq zenburn-blue      "#8CD0D3")
(setq zenburn-blue-1    "#7CB8BB")
(setq zenburn-blue-2    "#6CA0A3")
(setq zenburn-blue-3    "#5C888B")
(setq zenburn-blue-4    "#4C7073")
(setq zenburn-blue-5    "#366060")
(setq zenburn-magenta   "#DC8CC3")


;; Use Xcursor theme
(run-shell-command "xsetroot -cursor_name left_ptr -fg white -bg black")

(ql:quickload :clx-truetype)

(load-module "ttf-fonts")

(setq clx-truetype:*font-dirs* '("/run/current-system/sw/share/X11-fonts/"))
(clx-truetype:cache-fonts)

(set-font (make-instance 'xft:font :family "SauceCodePro Nerd Font" :subfamily
                         "Regular" :size 11))

(set-focus-color zenburn-green)
(set-unfocus-color zenburn-red)


(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :thin
      *message-window-padding* 10
      *maxsize-border-width* 0
      *normal-border-width* 0
      *transient-border-width* 0
      stumpwm::*float-window-border* 0
      stumpwm::*float-window-title-height* 3
      *mouse-focus-policy* :click)

(set-fg-color zenburn-fg)
(set-bg-color zenburn-bg+1)
(set-msg-border-width 1)
(set-border-color zenburn-orange)
