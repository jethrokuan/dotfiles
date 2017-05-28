(load-module "battery-portable")
(load-module "wifi")

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
(setf *window-format* "%m%n%s%10t ")
(setf *mode-line-timeout* 1)

(setf *time-modeline-string* "^B^3 %e %b %H:%M ^n")

(defun get-dropbox-status ()
  (run-shell-command "bash ~/.scripts/dropbox.sh" t))

(setf *screen-mode-line-format*
      (list "^B^3 %g ^n^b %W ^> " 
            "^B^3^n^b ^n %B | %d | " 
            '(:eval (get-dropbox-status))
            ))

(setf *mode-line-border-width* 1)
(setf *mode-line-background-color* zenburn-bg)
(setf *mode-line-foreground-color* zenburn-fg)

(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))
