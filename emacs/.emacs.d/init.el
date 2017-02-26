(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t) 
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (setq use-package-always-ensure t))

(use-package validate
  :demand t)

(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

(add-to-list 'initial-frame-alist
             '(font . "Source Code Pro for Powerline-12"))
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro for Powerline-12"))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode +1)

(setq sentence-end-double-space nil)

(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)
(defun trunc-lines-hook ()
  (setq truncate-lines nil))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

(load "~/.emacs.d/secrets.el" t)

(use-package exec-path-from-shell
  :demand t
  :init (exec-path-from-shell-initialize))

(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

(load-theme 'adwaita t)

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
   See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when 'newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when 'newline-and-indent
    (indent-according-to-mode)))

(bind-key* "C-o" 'open-next-line)
(bind-key* "M-o" 'open-previous-line)

(defun jethro/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(bind-key* "C-c !" 'jethro/nuke-all-buffers)

(bind-key* "C-x m" 'eshell)

(bind-key* "M-p" 'mark-paragraph)

(bind-key* "<f9>" (lambda ()
                    (interactive)
                    (setq-local compilation-read-command nil)
                    (call-interactively 'compile)))

(use-package hydra)

(use-package notmuch
  :bind (("<f10>" . notmuch)))

(require 'notmuch-address)
(setq notmuch-address-command "~/.emacs.d/goobook")
(notmuch-address-message-insinuate)

(use-package flx)

(use-package counsel
  :demand t
  :bind*
  (("C-c C-r" . ivy-resume)
   ("M-a" . counsel-M-x)
   ("C-M-i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-x j" . counsel-dired-jump)
   ("C-x l" . counsel-locate)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-rg)
   ("M-y" . counsel-yank-pop))
  :bind (:map help-map
              ("f" . counsel-describe-function)
              ("v" . counsel-describe-variable)
              ("l" . counsel-info-lookup-symbol))
  :config
  (ivy-mode 1)
  (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))

(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(define-key ivy-minibuffer-map (kbd "C-:") 'ivy-dired)
(define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(ivy-set-actions
 t
 '(("I" insert "insert")))

(defun ivy-dired ()
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
       (dired ivy--directory)
       (when (re-search-forward
              (regexp-quote
               (substring ivy--current 0 -1)) nil t)
         (goto-char (match-beginning 0))))
    (user-error
     "Not completing files currently")))

(define-key ivy-minibuffer-map (kbd "C-:") 'ivy-dired)

(use-package neotree)

(use-package crux 
  :bind* (("C-c o" . crux-open-with)
          ("C-c n" . crux-cleanup-buffer-or-region)
          ("C-c D" . crux-delete-file-and-buffer)
          ("C-a" . crux-move-beginning-of-line)
          ("M-o" . crux-smart-open-line)
          ("C-c r" . crux-rename-file-and-buffer)
          ("M-d" . crux-duplicate-current-line-or-region)
          ("M-D" . crux-duplicate-and-comment-current-line-or-region)
          ("s-o" . crux-smart-open-line-above)))

(bind-key* "<f8>" 'browse-url-of-buffer)

(use-package anzu
  :diminish anzu-mode 
  :config
  (global-anzu-mode +1)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2))
  :config
  (setq avy-keys '(?h ?t ?n ?s)))

(use-package windmove
  ;; :defer 4
  :ensure t
  :config
  ;; use command key on Mac
  (windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

(defun jethro/dired-mode-setup-hook ()
  "hook for dired-mode"
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook 'jethro/dired-mode-setup-hook)

(setq dired-listing-switches "-aBhl  --group-directories-first")

(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(require 'dired-x)

(save-place-mode 1)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "mw" 'avy-goto-word-1)   
  (key-chord-define-global "jk" 'avy-goto-char)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "FF" 'counsel-find-file)
  (key-chord-define-global "xx" 'execute-extended-command)
  (key-chord-define-global "yy" 'counsel-yank-pop)
  (key-chord-define-global ",." 'neotree-toggle))

(use-package visual-regexp
  :bind* (("C-M-%" . vr/query-replace)
          ("C-c m" . vr/mc-mark)))

(add-hook 'after-init-hook (lambda () (electric-pair-mode 1)))

(use-package electric-align
  :ensure f
  :load-path "elisp/"
  :diminish electric-align-mode
  :config (add-hook 'prog-mode-hook 'electric-align-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package iedit)

(use-package smartparens
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)

              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)

              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)

              ("C-M-d" . delete-sexp)

              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)
              ([remap sp-backward-kill-word] . backward-kill-word)

              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)

              ("C-x C-t" . sp-transpose-hybrid-sexp)

              ("M-c ("  . wrap-with-parens)
              ("M-c ["  . wrap-with-brackets)
              ("M-c {"  . wrap-with-braces)
              ("M-c '"  . wrap-with-single-quotes)
              ("M-c \"" . wrap-with-double-quotes)
              ("M-c _"  . wrap-with-underscores)
              ("M-c `"  . wrap-with-back-quotes)) 
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (defmacro def-pairs (pairs)
    `(progn
       ,@(loop for (key . val) in pairs
               collect
               `(defun ,(read (concat
                               "wrap-with-"
                               (prin1-to-string key)
                               "s"))
                    (&optional arg)
                  (interactive "p")
                  (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren        . "(")
              (bracket      . "[")
              (brace        . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote   . "`"))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(bind-key* "M-z" 'zap-up-to-char)

(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

(use-package flycheck
  :config
  (global-set-key (kbd "C-c f")
                  (defhydra hydra-flycheck
                    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
                          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
                          :hint nil)
                    "Errors"
                    ("f"  flycheck-error-list-set-filter                            "Filter")
                    ("n"  flycheck-next-error                                       "Next")
                    ("p"  flycheck-previous-error                                   "Previous")
                    ("<" flycheck-first-error                                      "First")
                    (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
                    ("q"  nil)))
  (use-package flycheck-pos-tip
    :config (flycheck-pos-tip-mode))
  (add-hook 'prog-mode-hook 'global-flycheck-mode))

(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

(use-package company
  :diminish company-mode
  :init (progn
          (add-hook 'after-init-hook 'global-company-mode)
          (setq company-dabbrev-ignore-case nil
                company-dabbrev-code-ignore-case nil
                company-dabbrev-downcase nil
                company-idle-delay 0
                company-begin-commands '(self-insert-command)
                company-transformers '(company-sort-by-occurrence))
          (use-package company-quickhelp
            :config (company-quickhelp-mode 1))))

(use-package flyspell 
  :ensure f 
  :diminish flyspell-mode
  :init
  (setenv "DICTIONARY" "en_GB")
  :config   
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config (progn
            (add-hook 'go-mode-hook 'compilation-auto-quit-window)
            (add-hook 'go-mode-hook (lambda ()
                                      (set (make-local-variable 'company-backends) '(company-go))
                                      (company-mode)))
            (add-hook 'go-mode-hook (lambda ()
                                      (add-hook 'before-save-hook 'gofmt-before-save)
                                      (local-set-key (kbd "M-.") 'godef-jump)))
            (add-hook 'go-mode-hook
                      (lambda ()
                        (unless (file-exists-p "Makefile")
                          (set (make-local-variable 'compile-command)
                               (let ((file (file-name-nondirectory buffer-file-name)))
                                 (format "go build %s"
                                         file))))))
            (use-package go-dlv
              :config (require 'go-dlv))
            (use-package golint
              :config
              (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
              (require 'golint))
            (use-package gorepl-mode
              :config (add-hook 'go-mode-hook #'gorepl-mode))
            (use-package company-go
              :config (add-hook 'go-mode-hook (lambda ()
                                                (set (make-local-variable 'company-backends) '(company-go))
                                                (company-mode))))))

(add-hook 'c++-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "g++ -Wall -s -pedantic-errors %s -o %s --std=c++14"
                             file
                             (file-name-sans-extension file)))))))

(use-package eclim
  :bind (("<f9>" . eclim-run-class))
  :config 
  (global-eclim-mode)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer))

(use-package company-emacs-eclim
  :config
  (company-emacs-eclim-setup))

(use-package gradle-mode
  :diminish gradle-mode
  :config
  (add-hook 'java-mode-hook (lambda ()
                              (gradle-mode 1))))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :init (add-hook 'fish-mode-hook
                  (lambda () (aggressive-indent-mode -1))))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(use-package elpy
  :mode ("\\.py\\'" . elpy-mode)
  :init
  (add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))
  :config
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (elpy-enable)
  (setq elpy-rpc-backend "jedi"))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :diminish emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'vue-mode-hook 'emmet-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode))

(use-package scss-mode
  :mode "\\.scss\\'" 
  :config (progn
            (setq scss-compile-at-save nil)))

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(use-package skewer-mode  
  :bind (:map skewer-mode-map
              ("C-c C-k" . skewer-load-buffer))
  :config
  (add-hook 'js2-mode-hook 'skewer-mode))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (use-package tern
    :diminish tern-mode
    :config    
    (add-hook 'js2-mode-hook 'tern-mode)
    (use-package company-tern
      :config
      (add-to-list 'company-backends 'company-tern))))

(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-j"))

(use-package jade)

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :config (add-hook 'json-mode-hook (lambda ()
                                      (make-local-variable 'js-indent-level)
                                      (setq js-indent-level 2))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (progn
            (setq markdown-command "multimarkdown")
            (add-hook 'markdown-mode-hook #'trunc-lines-hook)))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljs\\.hl\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-show-error-buffer nil
        cider-overlays-use-font-lock t
        cider-repl-result-prefix ";; => ")
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  (cider-repl-toggle-pretty-printing))

(use-package clj-refactor
  :defines cljr-add-keybindings-with-prefix
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-j"))

(use-package flycheck-clojure
  :config
  (flycheck-clojure-setup))

(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")
                                ("qpdfview" "qpdfview %o#%(outpage)")))
  (setq TeX-view-program-selection '((output-pdf "qpdfview")
                                     (output-pdf "Evince")))
  (when latex-enable-auto-fill
    (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
  (when latex-enable-folding
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))
(use-package auctex-latexmk)

(use-package company-auctex
  :defer t)

(global-hl-line-mode 1)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package page-break-lines)

(setq display-time-24hr-format t)
(display-time-mode 1)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out"))

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 10))

(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config (progn
            (add-to-list 'golden-ratio-extra-commands 'ace-window)
            (golden-ratio-mode 1)))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package git-gutter-fringe+
  :config
  (global-git-gutter+-mode)
  (set-face-foreground 'git-gutter+-modified "gold1")
  (set-face-foreground 'git-gutter+-added    "SeaGreen")
  (set-face-foreground 'git-gutter+-deleted  "IndianRed")
  (setq git-gutter-fr+-side 'left-fringe))

(use-package org-plus-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)))

(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode 1)))

(setq org-agenda-files (file-expand-wildcards "~/.org/gtd/*.org"))

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)  
(global-set-key (kbd "<f2> h") 'bh/hide-other)
(global-set-key (kbd "<f2> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f2> o") 'bh/make-org-scratch)
(global-set-key (kbd "<f2> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f2> v") 'visible-mode)
(global-set-key (kbd "<f2> l") 'org-toggle-link-display)
(global-set-key (kbd "M-<f2>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "<f3>") 'org-clock-in)
(global-set-key (kbd "<f3>") 'org-clock-out)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/.org/gtd/")
(setq org-default-notes-file "~/.org/gtd/inbox.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(defvar jethro/org-basic-inbox-template "* TODO %^{Task}
:PROPERTIES:
:EFFORT: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%a
%i
%?")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      `(("t" "todo" entry (file "~/.org/gtd/inbox.org")
         ,jethro/org-basic-inbox-template)
        ("r" "respond" entry (file "~/.org/gtd/inbox.org")
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t) 
        ("w" "org-protocol" entry (file "~/.org/gtd/inbox.org")
         "* TODO Review %c\n%U\n" :immediate-finish t)))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-completion-use-ido t)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks))) 
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil)))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end) 
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond 
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline) 
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end) 
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline) 
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end) 
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(setq org-global-properties (quote (("Effort_ALL" . "0 0:10 0:20 0:30 1:00 1:30 2:00 3:00 4:00 6:00 8:00 10:00 20:00"))))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

(use-package htmlize
  :config
  (require 'htmlize))

(require 'org-notmuch)

(defun jethro/auto-git-commit-and-push (dir)
  (shell-command (format "cd %s && git add -A && git commit -m \"%s\" && git push origin master" dir "New changes: $(date)")))

(setq jethro/emacsd-site-dir "~/Documents/Code/emacsd_site/")

(setq org-publish-project-alist
      '(("emacs.d"
         :publishing-function org-html-publish-to-html
         :publishing-directory jethro/emacsd-site-dir
         :base-directory "~/.emacs.d/"
         :exclude ".*"
         :include ["init.org"]
         :completion-function (lambda () (let ((htmlfile (concat jethro/emacsd-site-dir
                                                                 "init.html")))
                                           (if (file-exists-p htmlfile)
                                               (progn
                                                 (rename-file htmlfile
                                                              (concat jethro/emacsd-site-dir
                                                                      "index.html") t)
                                                 (jethro/auto-git-commit-and-push jethro/emacsd-site-dir)))))
         :with-emphasize t
         :with-title nil
         :with-toc t
         :html-head "<link rel=\"stylesheet\" href=\"/css/emacsd.css\" type=\"text/css\">"
         :html-preamble t)))

(use-package ox-reveal
  :config
  (require 'ox-reveal))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode %f"
        "pdflatex -shell-escape -interaction nonstopmode %f"))
(require 'ox-latex)
(setq org-latex-tables-booktabs t)
(setq org-latex-listings 'minted)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
(setq org-latex-minted-options
      '(("frame" "lines")
        ("linenos")
        ("numbersep" "5pt")
        ("framesep" "2mm")
        ("fontfamily" "tt")))
(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass[10pt]{memoir}
                      \\usepackage{charter}
                      \\usepackage[T1]{fontenc}
                      \\usepackage{booktabs}
                      \\usepackage{amsmath}
                      \\usepackage{minted}
                      \\usemintedstyle{borland}
                      \\usepackage{color}
                      \\usepackage{epigraph}
                      \\usepackage{enumitem}
                      \\setlist{nosep}
                      \\setlength\\epigraphwidth{13cm}
                      \\setlength\\epigraphrule{0pt}
                      \\usepackage{fontspec}
                      \\usepackage{graphicx}
                      \\usepackage{hyperref}
                      \\hypersetup {colorlinks = true, allcolors = red}
                      \\title{}
                      [NO-DEFAULT-PACKAGES]
                      [NO-PACKAGES]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("books"
               "\\documentclass[oneside]{tufte-book}
                      \\usepackage{charter}
                      \\usepackage{booktabs}
                      \\usepackage{minted}
                      \\usemintedstyle{bw}
                      \\usepackage{graphicx}

                      \\usepackage{hyperref}
                      \\hypersetup{colorlinks}

                      \\setlength{\\parskip}{1em}

                      \\title{}
                      [NO-DEFAULT-PACKAGES]
                      [NO-PACKAGES]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package org-download
  :config
  (setq-default org-download-image-dir "./pictures")
  (setq-default org-download-heading-lvl nil)
  (require 'org-download))

(defun jethro/org-sort-books ()
    (interactive)
    (let ((old-point (point)))
      (beginning-of-buffer)
      (org-sort-entries t ?a)
      (beginning-of-buffer)
      (org-sort-entries t ?o)
      (show-all)
      (org-global-cycle)
      (goto-char old-point)))

(defun jethro/org-after-save-books ()
  (org-publish "books"))

(use-package deft
  :bind* (("C-c d" . deft)
          ("C-x C-g" . deft-find-file)) 
  :bind (("C-c C-r" . deft-rename-file))
  :config
  (setq deft-directory "~/.org/deft/"
        deft-extensions '("org")
        deft-use-filename-as-title t
        deft-default-extension "org"
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "_")
                                 (nospace . "_")
                                 (case-fn . downcase))))

(use-package epresent
  :bind ("<f5>"))

(use-package smerge-mode
  :functions smerge-next smerge-prev smerge-keep-all smerge-keep-mine smerge-keep-other
  :config
  (progn
    (global-set-key (kbd "C-c s")
                    (defhydra hydra-smerge (:body-pre (smerge-mode 1) :color red)
                      "Smerge mode"
                      ("<down>" smerge-next        "Next conflict")
                      ("<up>"   smerge-prev        "Previous conflict")
                      ("M-a"    smerge-keep-all    "Keep all")
                      ("M-m"    smerge-keep-mine   "Keep mine")
                      ("M-o"    smerge-keep-other  "Keep other")))))

(use-package magit  
  :bind (("s-g" . magit-status)
         ("s-G" . magit-blame))
  :init (setq magit-auto-revert-mode nil)
  :config
  ;; (use-package magithub)
  (add-hook 'magit-mode-hook 'hl-line-mode))

(use-package projectile
  :demand t
  :init (projectile-global-mode 1)
  :bind-keymap* ("C-x p" . projectile-command-map)
  :config
  (require 'projectile)
  (use-package counsel-projectile 
    :bind (("s-f" . counsel-projectile-find-file)
           ("s-b" . counsel-projectile-switch-to-buffer))
    :config
    (counsel-projectile-on))
  (setq projectile-use-git-grep t)
  (setq projectile-create-missing-test-files t)
  (setq projectile-completion-system 'ivy))

(setq projectile-switch-project-action
      #'projectile-commander)
(def-projectile-commander-method ?s
  "Open a *eshell* buffer for the project."
  (projectile-run-eshell))
(def-projectile-commander-method ?c
  "Run `compile' in the project."
  (projectile-compile-project nil))
(def-projectile-commander-method ?\C-?
  "Go back to project selection."
  (projectile-switch-project))
(def-projectile-commander-method ?d
  "Open project root in dired."
  (projectile-dired))
(def-projectile-commander-method ?F
  "Git fetch."
  (magit-status)
  (call-interactively #'magit-fetch-all))
(def-projectile-commander-method ?j
  "Jack-in."
  (let* ((opts (projectile-current-project-files))
         (file (ivy-read
                "Find file: " 
                opts)))
    (find-file (expand-file-name
                file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)
    (cider-jack-in)))

(use-package esup
  :defer t)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config (add-hook 'after-init-hook 'which-key-mode))

(use-package nameless
  :diminish nameless-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'nameless-mode-from-hook)
  (setq nameless-global-aliases
        '(("fl" . "font-lock")
          ("s" . "seq")
          ("me" . "macroexp")
          ("c" . "cider")
          ("q" . "queue"))))

(use-package firestarter
  :bind ("C-c M s" . firestarter-mode)
  :init (put 'firestarter 'safe-local-variable 'identity))

(use-package paradox
  :commands (paradox-list-packages)
  :config
  (setq paradox-github-token jethro/paradox-user-token))

(use-package focus
  :diminish focus-mode
  :bind ("C-c M f" . focus-mode))

(use-package artbollocks-mode
  :bind (("C-c M a" . artbollocks-mode))
  :config
  (add-hook 'text-mode-hook 'artbollocks-mode))

(use-package darkroom
  :bind (("C-c M d" . darkroom-mode)
         ("C-c M t" . darkroom-tentative-mode)))

(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))
