(in-package :stumpwm)

(clear-window-placement-rules)

(define-frame-preference "code"
    ;; frame raise lock (lock AND raise == jumpto)
    (0 t   t :title "emacs")
  (0 t   t :instance "urxvt"))

(define-frame-preference "www"
    ;; frame raise lock (lock AND raise == jumpto)
    (0 t   t :class "Firefox"))

(define-frame-preference "media"
    (0 t   t :class "smplayer")
  (0 t t :class "Spotify"))

(define-frame-preference "chat"
    (0 t   t :class "Franz")
  (0 t   t :class "Slack")
  (0 t   t :class "Telegram")
  (0 t   t :class "Whatsapp"))

(setq *mouse-focus-policy* :ignore)
