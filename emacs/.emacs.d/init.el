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

(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'default-frame-alist
             '(font . "Fira Code-12"))

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

(bind-key* "C-o" 'bookmark-jump)

(use-package tao-theme
  :init
  (load-theme 'tao-yang t))

(defun jethro/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(bind-key* "C-c !" 'jethro/nuke-all-buffers)

(bind-key* "C-x m" 'eshell)

(bind-key* "M-p" 'mark-paragraph)

(bind-key* "<f5>" (lambda ()
                    (interactive)
                    (setq-local compilation-read-command nil)
                    (call-interactively 'compile)))

(use-package counsel
  :demand t
  :bind*
  (("C-c C-r" . ivy-resume)
   ("M-a" . counsel-M-x)
   ("C-M-i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-c u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . counsel-locate))
  :bind (:map help-map
          ("f" . counsel-describe-function)
          ("v" . counsel-describe-variable)
          ("l" . counsel-info-lookup-symbol))
  :config
  (ivy-mode 1)
  (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (ivy-set-actions
   'counsel-find-file
   '(("d" (lambda (x) (delete-file (expand-file-name x)))
      "delete"
      )))
  (ivy-set-actions
   'ivy-switch-buffer
   '(("k"
      (lambda (x)
        (kill-buffer x)
        (ivy--reset-state ivy-last))
      "kill")
     ("j"
      ivy--switch-buffer-other-window-action
     "other window"))))

(use-package swiper
  :bind*
  (("C-s" . swiper)
   ("C-r" . swiper))
  :bind
  (:map read-expression-map
    ("C-r" . counsel-expression-history)))

(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2)))

(use-package dumb-jump
  :diminish dumb-jump-mode
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

(use-package ace-window
  :bind (("M-'" . ace-window)))

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

(use-package paredit
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package zzz-to-char
  :bind (("M-z" . zzz-up-to-char)))

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

(use-package flycheck
  :config (progn
            (use-package flycheck-pos-tip
              :config (flycheck-pos-tip-mode))
            (add-hook 'prog-mode-hook 'global-flycheck-mode)))

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
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

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

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(use-package elpy
  :mode ("\\.py\\'" . elpy-mode)
  :init
  (add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))
  (defun set-newline-and-indent ()
    "Map the return key with `newline-and-indent'"
    (local-set-key (kbd "RET") 'newline-and-indent))
  (add-hook 'python-mode-hook 'set-newline-and-indent)
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

(defun jethro/locate-dominating-file (regexp)
  "Locate a directory with a file matching REGEXP."
  (locate-dominating-file
   default-directory
   (lambda (directory)
     (> (length (directory-files directory nil regexp t)) 0))))
(defconst jethro/jshint-regexp
  (concat "\\`" (regexp-quote ".jshintrc") "\\'"))
(defconst jethro/eslint-regexp
  (concat "\\`" (regexp-quote ".eslintrc") "\\(\\.\\(js\\|ya?ml\\|json\\)\\)?\\'"))

(defun jethro/js2-mode-hook ()
  (cond
   ((jethro/locate-dominating-file jethro/jshint-regexp)
    (flycheck-select-checker 'javascript-jshint))
   ((jethro/locate-dominating-file jethro/eslint-regexp)
    (flycheck-select-checker 'javascript-eslint))))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (add-hook 'js2-mode-hook #'jethro/js2-mode-hook)
  (use-package tern
    :diminish tern-mode
    :config    
    (add-hook 'js2-mode-hook 'tern-mode)
    (use-package company-tern
                  :config
                  (add-to-list 'company-backends 'company-tern))))

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :config (add-hook 'json-mode-hook (lambda ()
                                      (make-local-variable 'js-indent-level)
                                      (setq js-indent-level 2))))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljs\\.hl\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package cider
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            
        cider-overlays-use-font-lock t)         
  (cider-repl-toggle-pretty-printing))

(use-package clj-refactor
  :defines cljr-add-keybindings-with-prefix
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-j"))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (progn
            (setq markdown-command "multimarkdown")
            (add-hook 'markdown-mode-hook #'trunc-lines-hook)))

(use-package beacon
  :diminish beacon-mode
  :config (progn
            (beacon-mode 1)
            (setq beacon-push-mark 10)))

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

(use-package git-gutter+
  :init (global-git-gutter+-mode)
  :diminish git-gutter+-mode
  :defer 5
  :config (progn
            (setq git-gutter+-modified-sign "==")
            (setq git-gutter+-added-sign "++")
            (setq git-gutter+-deleted-sign "--")))

(use-package htmlize)

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :mode ("\\.org\\'" . org-mode)
  :init
  (add-hook 'org-mode-hook #'trunc-lines-hook)
  (setq org-ellipsis "⤵")
  (setq org-directory "~/.org")
  (setq org-default-notes-directory (concat org-directory "/notes.org"))
  (setq org-export-in-background t)
  (setq org-hide-emphasis-markers t)
  (setq org-src-tab-acts-natively t)
  (font-lock-add-keywords 'org-mode
                  '(("^ +\\([-*]\\) "
                     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))) 
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-capture-templates
        '(("b" "Book" entry (file "~/.org/books.org")
           "* TO-READ %(org-set-tags)%? %i\n"))))

(defvar jethro/org-agenda-files
  (append
   ;;(file-expand-wildcards "~/.org/*.org")
   (file-expand-wildcards "~/.org/calendars/*.org")
   (file-expand-wildcards "~/.org/gtd/*.org"))
  "Files to include in org-agenda-files")

(setq org-agenda-files jethro/org-agenda-files)

(setq org-agenda-custom-commands 
      '(("w" todo "WAITING" nil) 
        ("n" todo "NEXT" nil)
        ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
      )

(setq org-agenda-dim-blocked-tasks t)

(setq org-icalendar-combined-agenda-file (concat org-directory "/org.ics"))
(setq org-icalendar-include-todo '(all))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
(setq org-agenda-default-appointment-duration 60)

;; this hook saves an ics file once an org-buffer is saved
(defun jethro/org-ical-export()
  (org-icalendar-combine-agenda-files))

(setq org-publish-project-alist
      '(("books"
         ;; Path to your org files.
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/Documents/Code/jethrokuan.github.io/"
         :base-directory "~/.org/"
         :exclude ".*"
         :include ["books.org"]
         :with-emphasize t
         :with-todo-keywords t
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"/css/org.css\" type=\"text/css\">"
         :html-preamble t)
        ("emacs.d"
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/Documents/Code/jethrokuan.github.io/"
         :base-directory "~/.emacs.d/"
         :exclude ".*"
         :include ["init.org"]
         :with-emphasize t
         :with-title nil         
         :with-toc t
         :html-head "<link rel=\"stylesheet\" href=\"/css/emacsd.css\" type=\"text/css\">"
         :html-preamble t)))

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode %f"
        "xelatex -shell-escape -interaction nonstopmode %f"))
(require 'ox-latex)
(setq org-latex-tables-booktabs t)
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("frame" "lines")
        ("linenos")
        ("numbersep" "5pt")
        ("framesep" "2mm")
        ("fontfamily" "tt")))
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("org-article"
               "\\documentclass[11pt,a4paper]{article}
                      \\usepackage[default]{droidserif}
                      \\usepackage[T1]{fontenc}
                      \\usepackage{booktabs}
                      \\usepackage{minted}
                      \\usemintedstyle{borland}
                      \\usepackage{color}
                      \\setcounter{tocdepth}{2}
                      \\usepackage{xcolor}
                      \\usepackage{soul}
                      \\definecolor{Light}{gray}{.90}
                      \\sethlcolor{Light}
                      \\let\\OldTexttt\\texttt
                      \\renewcommand{\\texttt}[1]{\\OldTexttt{\\hl{#1}}}
                      \\usepackage{epigraph}
                      \\usepackage{enumitem}
                      \\setlist{nosep}
                      \\setlength\\epigraphwidth{13cm}
                      \\setlength\\epigraphrule{0pt}
                      \\usepackage{fontspec}
                      \\usepackage{graphicx} 
                      \\usepackage{parskip}
                      \\let\\oldsection\\section
                      \\renewcommand\\section{\\clearpage\\oldsection}
                      \\setlength{\\parskip}{1em}
                      \\usepackage{geometry}
                      \\usepackage{hyperref}
                      \\hypersetup {colorlinks = true, allcolors = red}
                      \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                                  marginparsep=7pt, marginparwidth=.6in}
                      \\pagestyle{empty}
                      \\title{}                  
                      [NO-DEFAULT-PACKAGES]
                      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package ox-reveal
    :config (require 'ox-reveal))

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

(defun jethro/org-after-save-init ()
  (org-babel-tangle)
  (org-publish "emacs.d"))

(use-package gtd-mode
  :bind (("C-c x" . gtd-clear-inbox)
         ("C-c i". gtd-into-inbox))
  :ensure f
  :load-path "elisp/"
  :config
  (gtd-mode 1))

(use-package magit  
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-blame))
  :init (setq magit-auto-revert-mode nil)
  :config (add-hook 'magit-mode-hook 'hl-line-mode))

(use-package projectile
  :demand t
  :init (projectile-global-mode 1)
  :bind-keymap* ("C-x p" . projectile-command-map)
  :config
  (require 'projectile)
  (use-package counsel-projectile 
    :bind (("s-P" . counsel-projectile)
           ("s-f" . counsel-projectile-find-file)
           ("s-b" . counsel-projectile-switch-to-buffer)))
  (setq projectile-use-git-grep t)
  (setq projectile-switch-project-action
        #'projectile-commander)
  (setq projectile-create-missing-test-files t)
  (setq projectile-completion-system 'ivy)
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
    (call-interactively #'magit-fetch-current))
  (def-projectile-commander-method ?j
    "Jack-in."
    (let* ((opts (projectile-current-project-files))
           (file (ido-completing-read
                  "Find file: "
                  opts
                  nil nil nil nil
                  (car (cl-member-if
                        (lambda (f)
                          (string-match "core\\.clj\\'" f))
                        opts)))))
      (find-file (expand-file-name
                  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (cider-jack-in))))

(use-package esup
  :defer t)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package pivotal-tracker
  :config
  (setq pivotal-api-token jethro/pivotal-api-token))

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
  :bind ("C-c m s" . firestarter-mode)
  :init (put 'firestarter 'safe-local-variable 'identity))

(use-package paradox
  :commands (paradox-list-packages)
  :config
  (setq paradox-github-token jethro/paradox-user-token))

(use-package focus
  :diminish focus-mode
  :bind ("C-c m f" . focus-mode))

(use-package artbollocks-mode
  :bind (("C-c m a" . artbollocks-mode))
  :config
  (add-hook 'text-mode-hook 'artbollocks-mode))

(use-package darkroom
  :bind (("C-c m d" . darkroom-mode)
         ("C-c m t" . darkroom-tentative-mode)))

(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))
