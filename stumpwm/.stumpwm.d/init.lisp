(in-package :stumpwm)

(defvar *stumpwm-config-dir*  "~/.stumpwm.d/"
  "StumpWM configuration directory.")
(set-module-dir (concat *stumpwm-config-dir* "contrib"))

(setf *startup-message* NIL
      *suppress-abort-messages* t)

(defvar *message-filters* '("Group dumped")
  "Don't show these messages.")

(defun message (fmt &rest args)
  "Overwritten message function to allow filters"
  (let ((msg-string (apply 'format nil fmt args)))
    (unless (member msg-string *message-filters* :test #'string=)
      (echo-string (current-screen) msg-string))))

(load (concat *stumpwm-config-dir* "utils.lisp"))

(load-mods '("quickload" "theme" "modeline" "keys" "groups" "windows")) ;; missing swank

(add-hook *post-command-hook* (lambda (command)
                                (when (member command winner-mode:*default-commands*)
                                  (winner-mode:dump-group-to-file))))
