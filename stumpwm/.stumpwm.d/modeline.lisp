(in-package :stumpwm)

(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")

(setf *bar-med-color* "^B^3")
(setf *bar-hi-color* "^B^3")
(setf *bar-crit-color* "^B^1")

(setf *colors*
      `(,zenburn-fg
        ,zenburn-orange
        ,zenburn-green 
        ,zenburn-cyan
        ,zenburn-blue
        ,zenburn-red
        ,zenburn-magenta
        ,zenburn-yellow
        ))

(update-color-map (current-screen))

(setf *group-format* " %t ")
;;(setf *window-format* "%m%50t ")
(setf *window-format* "%m%n%s%20t ")
(setf *mode-line-timeout* 1)

(setf *time-modeline-string* "^B^3 %e %b %H:%M ^n")

(defun get-dropbox-status ()
  (run-shell-command "bash ~/.scripts/dropbox.sh | tr -d '[:cntrl:]'" t))

(defun get-current-song ()
  (run-shell-command "bash ~/.scripts/get-current-song | tr -d '[:cntrl:]'" t))

(defun get-ssid ()
  (run-shell-command "bash ~/.scripts/get-ssid.sh | tr -d '[:cntrl:]'" t))

(setf *screen-mode-line-format*
      (list "^B^3 %g ^n^b %W ^>  "
            ;; '(:eval (get-current-song))
            ;; " | "
            '(:eval (get-ssid))
            "  %B  %d "
            '(:eval (get-dropbox-status)) 
            ))

(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* zenburn-bg)
(setf *mode-line-foreground-color* zenburn-fg)

(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))
