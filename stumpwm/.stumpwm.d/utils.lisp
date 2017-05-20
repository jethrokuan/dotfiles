;; Utility functions
;; usage:
;; (load "utils.lisp")

(in-package :stumpwm)

(export '(global-set-key))

(defun global-set-key (key command)
  "Define a global keybinding (use `*top-map*')."
  (define-key *top-map* key command)
  )

(defun run-shell-commands (commands)
  "Run a list of shell commands."
  (dolist (command commands)
    (run-shell-command command)))

(defcommand colon1 (&optional (initial "")) (:rest)
            (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
              (when cmd
                (eval-command cmd t))))


(defun load-mod (module)
  (load (concat *stumpwm-config-dir* module ".lisp")))

(defun load-mods (modules)
  (dolist (module modules)
    (load-mod module)))

(defun load-contribs (mods)
  (dolist (contrib mods)
    (let ((group (car contrib))
          (modules (cdr contrib)))
      (dolist (module modules)
        (load (format nil "~acontrib/~a/~a/~a.lisp"*stumpwm-config-dir* group module module))))))

(defmacro defkeys (map &rest couples)
  `(if ,map
       (progn ,@(loop for i in couples collect
                     `(define-key ,map (kbd ,(first i)) ,(second i))))
       (setf ,map
             (let ((map (make-sparse-keymap)))
               ,@(loop for i in couples collect
                      `(define-key map (kbd ,(first i)) ,(second i)))
               map))))
