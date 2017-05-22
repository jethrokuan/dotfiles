(in-package :stumpwm)

(clear-window-placement-rules)

(define-frame-preference "code"
    ;; frame raise lock (lock AND raise == jumpto)
    (0 t   t :title "emacs")
  (0 t   t :class "urxvt"))

(define-frame-preference "www"
    ;; frame raise lock (lock AND raise == jumpto)
    (0 t   t :class "Firefox"))
