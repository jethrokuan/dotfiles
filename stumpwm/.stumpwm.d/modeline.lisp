(in-package :stumpwm)
(load-module "battery-portable")
(load-module "cpu")

(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-7")

(defun color-ping (s)
  (if (equal s "")
      ""
      (let* ((words (cl-ppcre:split "\\s+" s))
             (ping (nth 5 words))
             (color (bar-zone-color (read-from-string ping)
                                    300 700 1000))t
             (colored-ping (format nil "^[~A~3D^]" color ping)))
        (cl-ppcre:regex-replace ping s colored-ping))))

(defun colour (key)
  (let ((colours '(:base03 #x002b36
                   :base02 #x073642
                   :base01 #x586e75
                   :base00 #x657b83
                   :blue0 #x373b43
                   :ypnose #x1c2027
                   :ypnosebl #x3e7ef3
                   :ypnosecy #x30a8e0
                   :blue1 #x242931
                   :base2 #xeee8d5
                   :base3 #xfdf6e3
                   :yellow #x99ad6a
                   :orange #xcb4b16
                   :red #xdc322f
                   :magenta #xd33682
                   :violet #x6c71c4
                   :blue #x268bd2
                   :cyan #x87ceeb 
                   :dfx #x14db49
                   :pnevma #x000000
                   :green #x8ae234)))
    (getf colours key)))

(setf *time-modeline-string* "%a %m-%d ^5*^B%l:%M^b^n")

(setq stumpwm:*mode-line-position* :top)
(setf stumpwm:*screen-mode-line-format*
      (list
       "^B[^b"
       "^B%n^n]" 
       "^B/^n ^7*^B%W^b^n "
       "^>" ; right align
       "^B«^b^n %B " ; battery
       "^B«^b^n %c " ; cpu
       "^B«^n "
       '(:eval (string-right-trim '(#\Newline) (run-shell-command
                                                "date +'%a %m-%d ^B^B%l:%M^b^n %p'|tr -d '\\n'"                                                
                                                t)))
       ))


(dolist (head
          ;; (list (screen-heads (current-screen))) ; first
          (screen-heads (current-screen)) ; all
         )
  (enable-mode-line (current-screen) head 
                    t *screen-mode-line-format*))
