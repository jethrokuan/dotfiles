;;; init.el --- Emacs configuration of Jethro Kuan -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016 Jethro Kuan <jethrokuan95@gmail.com>
;;
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://gihub.com/jethrokuan/dotfiles
;; Keywords: emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Emacs configuration of Jethro Kuan

;;; Add MELPA and Org
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

;;; `use-package'
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

;;; Sensible Emacs Defaults:

;;; y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; replace highlighted text when typed over
(delete-selection-mode +1)

;;; Debugging
(setq message-log-max 10000)

;;; Base font
(add-to-list 'default-frame-alist
             '(font . "Fira Code-12"))

;;; Use single-space for end of sentence
(setq sentence-end-double-space nil)

;;; Use 2 spaces instead of tabs at all times
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

;;; Don't wrap lines
(setq-default truncate-lines t)

;;; Don't load old byte code
(setq load-prefer-newer t)

;;; User-details
(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

;;; Remove UI cruft
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Truncate lines
(defun trunc-lines-hook ()
  (setq truncate-lines nil))

;;; Change backup directory to prevent littering of working dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Purge old backup files
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;;Setting up shell path
(use-package exec-path-from-shell
  :demand t
  :init (exec-path-from-shell-initialize))

;; Paradox
;; Better package listings
(use-package paradox
  :commands (paradox-list-packages))

;;; Setting Emacs registers
;;; Eg. C-o i to jump to init.el
(bind-key* "C-o" 'bookmark-jump)

;;; Break lines
;;; Small utility to change page breaks to look like <hr>s
(use-package page-break-lines
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;;; Browse Kill Ring
(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;;; Keybindings
;;; Eshell                 C-x m
;;; Mark-paragraph         M-p
;;; zzz-up-to-char         M-z
;;; Compile files          F5
;;; Nuke all buffers       C-c n
;;; Split and move right   C-x 3
;;; Toggle split           C-|
(require 'compile)

;;;;   Useful functions
(defun split-and-move-right ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-unset-key (kbd "M-a"))
(bind-key* "C-x |" 'toggle-window-split)
(bind-key* "C-x m" 'eshell)
(bind-key* "M-p" 'mark-paragraph)
(bind-key* "C-c !" 'nuke-all-buffers)
(bind-key* "C-x 3" 'split-and-move-right)
(bind-key* "<f5>" (lambda ()
                    (interactive)
                    (setq-local compilation-read-command nil)
                    (call-interactively 'compile)))

(use-package zzz-to-char
  :bind (("M-z" . zzz-up-to-char)))

;; Emacs profiling tool
(use-package esup
  :defer t)

;;; Theming
;;; Monokai theme
(use-package darktooth-theme
  :disabled t
  :init
  (load-theme 'darktooth t))

;;; Tao theme: Black and White
(use-package tao-theme
  :init
  (load-theme 'tao-yang t))

;;; Shows cursor location when jumping around
(use-package beacon
  :diminish beacon-mode
  :config (progn
            (beacon-mode 1)
            (setq beacon-push-mark 10)))

;;; Uses more screen estate when editing current file
(use-package golden-ratio
  :diminish golden-ratio-mode
  :config (progn
            (add-to-list 'golden-ratio-extra-commands 'ace-window)
            (golden-ratio-mode 1)))

;;;   Show parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;;; Firestarter
;;; Runs commands like shell commands on save
(use-package firestarter
  :bind ("C-c m s" . firestarter-mode)
  :init (put 'firestarter 'safe-local-variable 'identity))

;;; Flycheck
;;; Syntax checker for Emacs
;;; Flycheck-pos-tip pops up the warnings and errors below
;;; warning text
(use-package flycheck
  :config (progn
            (use-package flycheck-pos-tip
              :config (flycheck-pos-tip-mode))
            (add-hook 'prog-mode-hook 'global-flycheck-mode)))

;;; Minor Modes

;;; Visual Upgrades
;;; Anzu
(use-package anzu
  :disabled t
  :diminish anzu-mode
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config (progn
            (setq anzu-cons-mode-line-p t)            
            (set-face-attribute 'anzu-mode-line nil
                                :foreground "#FFF" :weight 'bold)
            (global-anzu-mode +1)))

;;; Highlights copy/paste changes
(use-package volatile-highlights
  :defer 5
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;;   Aggressive-indent mode
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;;   Electric Align Mode
(use-package electric-align
  :ensure f
  :load-path "elisp/"
  :diminish electric-align-mode
  :config (add-hook 'prog-mode-hook 'electric-align-mode))

;;    keyfreq
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
;;;   Which-key
(use-package which-key
  :diminish which-key-mode
  :config (add-hook 'after-init-hook 'which-key-mode))

;;;; Movement
(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-n" . mc/mark-next-lines)))

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

;;   Expand Region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;; Paredit
(use-package paredit
  :diminish paredit-mode
  :config  (progn
             (add-hook 'emacs-lisp-mode-hook #'paredit-mode)))

;;; Nameless
(use-package nameless
  :diminish nameless-mode
  :config
  (add-hook 'emacs-lisp-mode #'nameless-mode))

;;; Git
;;; Magit
(use-package magit  
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-blame))
  :init (setq magit-auto-revert-mode nil)
  :config (add-hook 'magit-mode-hook 'hl-line-mode))

;;; Git Gutter
(use-package git-gutter+
  :init (global-git-gutter+-mode)
  :diminish git-gutter+-mode
  :defer 5
  :config (progn
            (setq git-gutter+-modified-sign "==")
            (setq git-gutter+-added-sign "++")
            (setq git-gutter+-deleted-sign "--")))

;;;; Code/Text Completion
;;   Yasnippet - snippets
(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :defer 5
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

;;   Company - code completions
(use-package company
  :defer 5
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

;;; Project Management
;;; FFIP
(use-package find-file-in-project
  :ensure ag
  :disabled t
  :bind (("s-f" . find-file-in-project)
         ("s-F". find-file-in-current-directory)
         ("M-s-f" . find-file-in-project-by-selected)))

;;; Projectile
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

(use-package counsel)
(use-package swiper
  :bind*
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-a" . counsel-M-x)
   ("C-M-i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . counsel-locate))
  :config
  (progn
    (ivy-mode 1)
    (setq counsel-find-file-at-point t)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
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
        "other window")))))

;;; Org Mode
(defvar jk/org-agenda-files
  (append
   ;;(file-expand-wildcards "~/.org/*.org")
   (file-expand-wildcards "~/.org/calendars/*.org")
   (file-expand-wildcards "~/.org/gtd/*.org"))
  "Files to include in org-agenda-files")

(use-package org-plus-contrib
  :bind* (("C-c c" . org-capture)
          ("C-c a" . org-agenda)
          ("C-c l" . org-store-link))
  :mode ("\\.org\\'" . org-mode)
  :init
  (progn
    (add-hook 'org-mode-hook #'trunc-lines-hook)
    (setq org-ellipsis "⤵")
    (setq org-directory "~/.org")
    (setq org-default-notes-directory (concat org-directory "/notes.org"))          
    (setq org-agenda-dim-blocked-tasks t) ;;clearer agenda
    (setq org-agenda-files jk/org-agenda-files)
    (setq org-hide-emphasis-markers t)
    (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))) 
    (setq org-refile-targets
          '((nil :maxlevel . 3)
            (org-agenda-files :maxlevel . 3)))
    (setq org-use-fast-todo-selection t)
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/.org/someday.org" "Inbox")
             "* TODO %? %i\n")
            ("b" "Book" entry (file "~/.org/books.org")
             "* TO-READ %(org-set-tags) %? %i\n")))
    (setq org-publish-project-alist
          '(("org-books"
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
            ("framesep" "2mm")))
    (add-to-list 'org-latex-classes
                 '("org-article"
                   "\\documentclass[11pt,a4paper]{article}
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
                  \\defaultfontfeatures{Mapping=tex-text}
                  \\let\\oldsection\\section
                  \\renewcommand\\section{\\clearpage\\oldsection}
                  \\setlength{\\parskip}{1em}
                  \\setromanfont{Bitter}
                  \\setromanfont         [BoldFont={Bitter Bold},
                                 ItalicFont={Bitter Italic}]{Bitter}
                  \\setmonofont[Scale=1.0]{mononoki}
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
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
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
  :config 
  (use-package ox-reveal
    :config (require 'ox-reveal)))

(use-package org-gcal
  :defer 30
  :config
  (require 'org-gcal)
  (setq org-gcal-client-id "1025518578318-89os2t4n2ghd8105038u8b84hr90tqee.apps.googleusercontent.com"
        org-gcal-client-secret "govgKiWUCZmNSMHEm76YyNSB"
        org-gcal-file-alist '(("jethrokuan95@gmail.com" .  "~/.org/calendars/jethro_gmail.org"))))

(use-package gtd-mode
  :disabled t
  :ensure f
  :load-path "elisp/"
  :config
  (gtd-mode 1))

;;;; Writing
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (progn
            (setq markdown-command "multimarkdown")
            (add-hook 'markdown-mode-hook #'trunc-lines-hook)))

(use-package focus
  :diminish focus-mode
  :bind ("C-c m f" . focus-mode))

;;;; Clojure
;;;   Clojure-mode
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljs\\.hl\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

;;; Cider
(use-package cider
  :ensure t
  :defer t
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

;;; Clj-refactor
(use-package clj-refactor
  :defines cljr-add-keybindings-with-prefix
  :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))

;;; Web
;;; Included Packages
;;; Web-mode, SCSS-mode, Rainbow mode, Emmet-mode, Vue mode, JSON-mode

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

(use-package vue-mode
  :mode "\\.vue\\'")

;; Rainbow-mode
;; Colours hex and rgb values
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode)
  (add-hook 'sass-mode-hook 'rainbow-mode))

(use-package kurecolor
  :disabled t
  :bind (("<f6>" . kurecolor-decrease-saturation-by-step)
         ("<f7>" . kurecolor-increase-saturation-by-step)
         ("<f8>" . kurecolor-decrease-hue-by-step)
         ("<f9>" . kurecolor-increase-hue-by-step)
         ("<f1>" . kurecolor-decrease-brightness-by-step)
         ("<f2>" . kurecolor-increase-brightness-by-step)))

;;; SCSS-mode
;;; Turn on rainbow-mode when scss-mode is active
(use-package scss-mode
  :mode "\\.scss\\'" 
  :config (progn
            (setq scss-compile-at-save nil)))

;;; JSON
;;; Set JSON indent level to 2 spaces
(use-package json-mode
  :mode "\\.json\\'"
  :config (add-hook 'json-mode-hook (lambda ()
                                      (make-local-variable 'js-indent-level)
                                      (setq js-indent-level 2))))

;;; Systems progamming
;;; Included languages: Go, C++

;;; Helper that closes compilation buffer if no error is made
(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code))

(setq compilation-exit-message-function 'compilation-exit-autoclose)

(add-hook 'c++-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "g++ -Wall -s -pedantic-errors %s -o %s --std=c++14"
                             file
                             (file-name-sans-extension file)))))))

;;; Go
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

(provide 'init.el)

;;; Fish
(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))


;;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))


;;; Python
(use-package elpy
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
