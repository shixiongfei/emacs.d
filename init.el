;;; init.el --- ShiXiongfei's Emacs configuration
;;
;; Copyright (c) 2021 Xiongfei Shi
;;
;; Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
;; License: Apache-2.0
;;
;; https://github.com/shixiongfei/emacs.d
;;
;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "Xiongfei Shi"
      user-mail-address "xiongfei.shi@icloud.com")

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;; terminal mouse support
(unless window-system
  (require 'mwheel)
  (require 'mouse)
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (global-set-key [mouse-4] 'scroll-down-line)
  (global-set-key [mouse-5] 'scroll-up-line))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Don't compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

(defconst user-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p user-savefile-dir)
  (make-directory user-savefile-dir))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(unless (and (eq system-type 'darwin)
             window-system)
  (menu-bar-mode -1))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line numbers at the beginning of each line
(when (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode t))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; maximize the initial frame automatically
(when window-system
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 100 characters
(setq-default fill-column 100)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; show the name of the current function definition in the modeline
(which-function-mode 1)

;; enable winner-mode to manage window configurations
(winner-mode +1)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") #'proced)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; delete matching parentheses, usually used with C-M-u
(global-set-key (kbd "C-M-p") #'delete-pair)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; Shift click to extend marked region
(when (eq system-type 'windows-nt)
  (define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; enable some commands that are disabled by default
(put 'erase-buffer 'disabled nil)

(defun start-or-switch-to (function buffer-name)
  "Start or switch to the BUFFER-NAME of the specified FUNCTION."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;;; built-in packages
(use-package paren
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-indent-mode +1)
  (electric-pair-mode +1)
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package eshell
  :custom
  (eshell-directory-name (expand-file-name "eshell" user-savefile-dir)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" user-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" user-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package lisp-mode
  :config
  (defun visit-ielm ()
    "Switch to default `ielm' buffer. Start `ielm' if it's not already running."
    (interactive)
    (start-or-switch-to 'ielm "*ielm*"))

  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

;;; third-party packages
(use-package base16-theme
  :ensure t
  :init
  (unless (display-graphic-p)
    (setq base16-theme-256-color-source 'colors))
  :config
  (if (display-graphic-p)
      (load-theme 'base16-ocean t)
    (load-theme 'base16-default-dark t)))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

(use-package magit
  :ensure t
  :bind (("C-x g g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind (("C-x g t" . git-timemachine)))

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (global-set-key (kbd "C-c a") 'ag-project))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode))
  (diminish 'elisp-slime-nav-mode))

(use-package paredit
  :ensure t
  :config
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (diminish 'paredit-mode "()"))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook #'rainbow-mode)
  (add-hook 'html-mode-hook #'rainbow-mode)
  (add-hook 'nxml-mode-hook #'rainbow-mode)
  (diminish 'rainbow-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  ;; When nil, set the value to `fill-column'
  (setq whitespace-line-column nil)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package selectrum
  :ensure t
  :config
  (setq selectrum-count-style 'current/matches)
  (selectrum-mode +1)
  (diminish 'selectrum-mode))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  (diminish 'marginalia-mode))

(use-package consult
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
  (global-set-key (kbd "C-c i") 'consult-imenu)
  (global-set-key (kbd "C-c I") 'consult-imenu-multi)
  (global-set-key (kbd "M-g g") 'consult-goto-line)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-project-root-function #'vc-root-dir))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  (diminish 'company-mode))

(use-package yasnippet
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (diminish 'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package lsp-mode
  :ensure t
  :config
  ;; Customize prefix for key-bindings
  (setq lsp-keymap-prefix "C-l")
  ;; Enable logging for lsp-mode
  (setq lsp-log-io t)
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (diminish 'lsp-mode))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom)
  (diminish 'lsp-ui))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eldev
  :ensure t)

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (diminish 'super-save-mode))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-c w" . crux-swap-windows)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1)
  (diminish 'which-key-mode))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1)
  (diminish 'undo-tree-mode))

;; super useful for demos
(use-package keycast
  :ensure t)

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1)
  (diminish 'volatile-highlights-mode))

;; automatic highlighting current symbol minor mode
(use-package auto-highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)
  (diminish 'auto-highlight-symbol-mode))

;; highlight numbers in source code
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (diminish 'highlight-numbers-mode))

;;; programming language
(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-auto-fill-only-comments) t)))

;; C/C++
(use-package cc-mode
  :config
  ;; this will affect all modes derived from cc-mode, like
  ;; java-mode, php-mode, etc
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq c-default-style "k&r"
                                        c-basic-offset 2)
                                  (c-set-offset 'substatement-open 0)))

  (add-hook 'makefile-mode-hook (lambda ()
                                  (whitespace-toggle-options '(tabs))
                                  (setq indent-tabs-mode t))))

(use-package clang-format
  :ensure t
  :custom
  (clang-format-fallback-style "LLVM")
  :config
  (defun clang-format-on-save ()
    (add-hook 'before-save-hook #'clang-format-buffer nil 'local))

  (add-hook 'c-mode-hook 'clang-format-on-save)
  (add-hook 'c++-mode-hook 'clang-format-on-save))

;; Erlang
(use-package erlang
  :ensure t
  :config
  (require 'erlang-start)
  (require 'erlang-flymake)

  (add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))

  (setq inferior-erlang-machine "rebar3")
  (setq inferior-erlang-machine-options '("shell" "--sname=emacs"))
  (setq inferior-erlang-shell-type nil)

  (defun rebar-inferior-erlang-compile-outdir (orig &rest args)
    (concat (projectile-project-root) "_build/default/lib/"
            (projectile-project-name) "/ebin"))
  (advice-add 'inferior-erlang-compile-outdir
              :around 'rebar-inferior-erlang-compile-outdir)

  (add-hook 'erlang-mode-hook
            (lambda ()
              (erlang-tags-init)
              (setq erlang-indent-level 2
                    erlang-indent-guard 2
                    erlang-argument-indent 2)
              (setq erlang-electric-commands '(erlang-electric-comma
                                               erlang-electric-semicolon
                                               erlang-electric-newline))))
  (add-hook 'erlang-shell-mode-hook
            (lambda ()
              (unless (file-exists-p (rebar-inferior-erlang-compile-outdir nil))
                (make-directory (rebar-inferior-erlang-compile-outdir nil) t))))

  ;; Enable LSP for Erlang files
  (add-hook 'erlang-mode-hook #'lsp))

;; LFE
  (use-package lfe-mode
    :ensure t
    :after (erlang paredit rainbow-delimiters)
    :config
    (add-hook 'lfe-mode-hook #'paredit-mode)
    (add-hook 'lfe-mode-hook #'rainbow-delimiters-mode))

;; Swift
(use-package swift-mode
  :ensure t)

(use-package flycheck-swift
  :ensure t
  :after swift-mode
  :config
  (add-hook 'swift-mode-hook #'flycheck-swift-setup))

(use-package lsp-sourcekit
  :ensure t
  :after (lsp-mode swift-mode)
  :config
  (add-hook 'swift-mode-hook #'lsp))

;; Lua
(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil)
  (setq lua-indent-string-contents t)

  (add-hook 'lua-mode-hook
            (lambda ()
              (define-key lua-mode-map (kbd "C-c C-b") 'lua-send-buffer)
              (define-key lua-mode-map (kbd "C-c C-l") 'lua-send-current-line)
              (define-key lua-mode-map (kbd "C-c C-f") 'lua-send-defun)
              (define-key lua-mode-map (kbd "C-c C-r") 'lua-send-region)
              (define-key lua-mode-map (kbd "C-c C-z") 'lua-show-process-buffer)))

  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;; Web
(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode)))

;; Org-Mode
(use-package org
  :init
  (unless (version< (org-version) "9.2")
    (require 'org-tempo))
  (setq org-log-done t)
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-a") 'org-beginning-of-line)
              (make-local-variable 'minor-mode-overriding-map-alist)))

  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (calc .t)
       (emacs-lisp . t)))
    (setq org-confirm-babel-evaluate nil))

  (defun org-publish-sitemap-time-entry (entry style project)
    (format "%s %s"
            (format-time-string
             "[%Y.%m.%d]"
             (org-publish-find-date entry project))
            (org-publish-sitemap-default-entry entry style project)))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-html-validation-link nil)
              (setq org-export-with-creator t)
              (setq org-publish-project-alist
                    `(("org-post"
                       :base-directory "~/Codes/org/post"
                       :base-extension "org"
                       :publishing-directory "~/Codes/website/public"
                       :recursive t
                       :exclude "private*\\|.*\.private\.org"
                       :publishing-function org-html-publish-to-html
                       :section-numbers nil
                       :with-sub-superscript nil
                       :author "shixiongfei"
                       :html-validation-link nil
                       :html-doctype "html5"
                       :html-link-home "/"
                       :html-link-up "/"
                       ;; :html-home/up-format ""
                       :html-head ,(concat
                                    "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>\n"
                                    "<script src=\"https://hm.baidu.com/hm.js?4dbc75a8d627e17a8714e4c8b2e9afa8\"></script>")
                       :html-head-include-default-style nil
                       :with-creator t
                       :auto-preamble t
                       ;; :html-preamble ""
                       :auto-sitemap t
                       :sitemap-style list
                       :sitemap-title "{im}shixiongfei"
                       :sitemap-filename "index.org"
                       :sitemap-sort-files anti-chronologically
                       :sitemap-format-entry org-publish-sitemap-time-entry)
                      ("org-static"
                       :base-directory "~/Codes/org/static"
                       :base-extension "css\\|js"
                       :publishing-directory "~/Codes/website/public"
                       :recursive t
                       :publishing-function org-publish-attachment)
                      ("org-images"
                       :base-directory "~/Codes/org/post"
                       :base-extension "jpg\\|jpeg\\|png\\|gif"
                       :publishing-directory "~/Codes/website/public"
                       :recursive t
                       :publishing-function org-publish-attachment)
                      ("org" :components ("org-post" "org-static" "org-images"))))
              (global-set-key (kbd "C-c x") 'org-publish-current-project))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; YAML
(use-package yaml-mode
  :ensure t)

;; Fonts setting
(when (fboundp 'set-fontset-font)
  (when (eq system-type 'windows-nt)
    ;; https://files.ax86.net/terminus-ttf/
    (set-face-attribute 'default nil :font "Terminus (TTF) for Windows" :height 120)
    (set-fontset-font t 'han "SimSun")
    (set-fontset-font t 'han "SimSun-ExtB" nil 'append))

  (when (eq system-type 'darwin)
    ;; brew install font-terminus
    (set-face-attribute 'default nil :font "Terminus (TTF)" :height 140)
    (set-fontset-font t 'han "STSong")
    ;; Enable emoji, and stop the UI from freezing when trying to display them.
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
