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


(defmacro without-windows-placement-rules (&rest body)
  `(let ((rules *window-placement-rules*))
     (clear-window-placement-rules)
     ,@body
     (run-with-timer 1 nil (lambda ()
                             (setf *window-placement-rules* rules)))))

(defun popup ()
  "Split vertically or horizontally depending on the context."
  (let* ((group (current-group))
         (frame (tile-group-current-frame group)))
    (if (> (frame-width frame) (frame-height frame))
        (hsplit)
        (vsplit))
    (fnext)))

(defmacro with-popup (&rest body)
  "Split depending on the context and evaluate BODY."
  `(progn
     (popup)
     ,@body))
