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
  (0 t t :class "Spotify"))

(define-frame-preference "chat"
    (0 t   t :class "Franz")
  (0 t   t :class "Slack")
  (0 t   t :class "Telegram"))

(load-module "winner-mode")

(defvar *winner-map* nil
  "keymap for winner-mode")

(defkeys *winner-map*
    ("h" "winner-undo")
  ("s" "winner-redo"))

(add-hook *post-command-hook* (lambda (command)
                                (when (member command winner-mode:*default-commands*)
                                  (winner-mode:dump-group-to-file))))

(define-key *top-map* (kbd "s-w") *winner-map*)
