(in-package :stumpwm)

(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
    `(run-commands ,@ns)))

(run-commands "grename code")
(make-groups-bg "www" "media" "chat")
