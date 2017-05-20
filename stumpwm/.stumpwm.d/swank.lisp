(ql:quickload :swank)
(swank-loader:init)

(defun swank ()
  "Launch swank server for SLIME." 
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)  )

(defcommand swank-start () ()
            "Interactive starting of swank, display the usage on screen."
            (swank)
            (message "Starting swank. M-x slime-connect RET RET,
  then (in-package stumpwm).") )

(swank)
