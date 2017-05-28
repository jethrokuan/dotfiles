(in-package :stumpwm)

(clear-window-placement-rules)

(define-frame-preference "code"
    ;; frame raise lock (lock AND raise == jumpto)
    (0 t   t :title "emacs")
  (0 t   t :title "urxvt"))

(define-frame-preference "www"
    ;; frame raise lock (lock AND raise == jumpto)
    (0 t   t :class "Firefox"))

(define-frame-preference "media"
    (0 t   t :class "smplayer")
  (0 t t :class "Spotify" :instance "spotify"))

(define-frame-preference "chat"
    (0 t   t :class "Franz"))
