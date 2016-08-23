;;; Package --- Summary
;;; Commentary:
;;  startup screen

;;; Code:
;; Custom splash screen
(defvar startscreen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Keymap for startscreen mode.")

(define-derived-mode startscreen-mode special-mode "Startscreen"
  "Startscreen major mode for startup screen.
\\<startscreen-mode-map>
"
  :group 'startscreen
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defun jethro/insert-startupify-lists ()
  (interactive)
  (with-current-buffer (get-buffer-create "*startscreen*")
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))
      (page-break-lines-mode)

      (recentf-mode)
      (when (jethro//insert-file-list "Recent Files:" (recentf-elements 5))
        (jethro//insert--shortcut "r" "Recent Files:")
        (insert list-separator))

      (ivy-mode)
      (require 'bookmark)
      (when (jethro//insert-bookmark-list "Bookmarks:" (bookmark-all-names))
        (jethro//insert--shortcut "m" "Bookmarks:")
        (insert list-separator))

      (projectile-mode)
      (when (jethro//insert-project-list "Projects:" (subseq (projectile-relevant-known-projects) 0 5))
        (jethro//insert--shortcut "p" "Projects:")
        (insert list-separator)))
    (startscreen-mode)))

(defun jethro//insert-file-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun jethro//insert-project-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun jethro//insert-bookmark-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (bookmark-jump ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (format "%s - %s" el (abbreviate-file-name
                                                 (bookmark-get-filename el)))))
          list)))

(defun jethro/insert-page-break ()
  "Insert a page break line in startscreen buffer."
  (jethro/append "\n\n\n"))

(defun jethro/append (msg &optional messagebuf)
  "Append MSG to startscreen buffer. If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*startscreen*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(My Emacs) %s" msg)))))

(defmacro jethro//insert--shortcut (shortcut-char search-label &optional no-next-line)
  `(define-key startscreen-mode-map ,shortcut-char (lambda ()
                                                     (interactive)
                                                     (unless (search-forward ,search-label (point-max) t)
                                                       (search-backward ,search-label (point-min) t))
                                                     ,@(unless no-next-line
                                                         '((forward-line 1)))
                                                     (back-to-indentation))))


(defun jethro/goto-link-line ()
  "Move the point to the beginning of the link line."
  (interactive)
  (with-current-buffer "*startscreen*"
    (goto-char (point-min))
    (re-search-forward "Homepage")
    (beginning-of-line)
    (widget-forward 1)))

(defun jethro/setup-startup-hook ()
  "Add post init processing."
  (add-hook 'emacs-startup-hook (lambda ()
                                  ;; Display useful lists of items
                                  (jethro/insert-startupify-lists))
            (redisplay))
  (add-hook 'after-init-hook '(lambda () (switch-to-buffer "*startscreen*"))))

(provide 'startscreen)
;;; startscreen ends here
