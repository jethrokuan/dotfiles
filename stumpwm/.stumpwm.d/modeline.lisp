(load-module "battery-portable")
(load-module "wifi")
;; "[^B%n^b] %W"
;; "%n%s%t"
;; "%m%n%s%50t"
;; "%a %b %e %k:%M:%S"

(setf *bar-med-color* "^B^3")
(setf *bar-hi-color* "^B^3")
(setf *bar-crit-color* "^B^1")

(setf *colors*
      '("black"
        "#FFDC00"
        "#01FF70"
        "#DDDDDD"
        "#DDDDDD"
        "#DDDDDD"
        "#DDDDDD"
        "#DDDDDD"
        "#DDDDDD"
        "#DDDDDD"
        "#DDDDDD"
        "#DDDDDD"
        ))

(update-color-map (current-screen))

(setf *group-format* " %t ")
;;(setf *window-format* "%m%50t ")
(setf *window-format* "%m%n%s%10t ")
(setf *mode-line-timeout* 2)

(setf *time-modeline-string* "^3 %e %b^n^B %l:%M ^b")

(defun get-date-modeline ()
  (run-shell-command
   (format nil "date +\"~A\""
           *time-modeline-string*) t))

(defun get-layout-modeline ()
  (if (= 0 (get-current-layout *display*))
      "^2 en ^n"))

(setf *screen-mode-line-format*
      (list "^B^3 %g ^n^b %W ^> "
            '(:eval (get-layout-modeline))
            "  "
            "^B^3^n^b%B "
            '(:eval (get-date-modeline))
            ))

(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* "#000809")
(setf *mode-line-foreground-color* "#01FF70")

(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))
