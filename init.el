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
  (setq mac-command-modifier 'meta)

  ;; Iterm2 mouse support
  (unless window-system
    (require 'mwheel)
    (require 'mouse)
    (xterm-mouse-mode t)
    (mouse-wheel-mode t)
    (global-set-key [mouse-4] 'scroll-down-line)
    (global-set-key [mouse-5] 'scroll-up-line)))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

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
(if (or (eq system-type 'darwin)
        (eq system-type 'windows-nt))
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

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; Shift click to extend marked region
(when (eq system-type 'windows-nt)
  (define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; enable some commands that are disabled by default
(put 'erase-buffer 'disabled nil)


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
  :init
  (setq eshell-directory-name (expand-file-name "eshell" user-savefile-dir)))

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
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
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
  (load-theme 'base16-default-dark t))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

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
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
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
         ("C-k" . crux-smart-kill-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-line] . crux-smart-kill-line)
         ([remap kill-whole-line] . crux-kill-whole-line)))

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


;;; programming language

;; Common Lisp
(use-package slime
  :ensure t
  :config
  ;; the SBCL configuration file is in Common Lisp
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
  ;; Open files with .cl extension in lisp-mode
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

  (with-eval-after-load "slime"
    ;; a list of alternative Common Lisp implementations that can be
    ;; used with SLIME. Note that their presence render
    ;; inferior-lisp-program useless. This variable holds a list of
    ;; programs and if you invoke SLIME with a negative prefix
    ;; argument, M-- M-x slime, you can select a program from that list.
    (setq slime-lisp-implementations
          '((ccl64 ("ccl64")
                   :coding-system utf-8-unix)
            (sbcl ("sbcl" "--noinform" "--dynamic-space-size" "8192")
                  :coding-system utf-8-unix)))

    ;; select the default value from slime-lisp-implementations
    (if (and (eq system-type 'darwin)
             (executable-find "ccl64"))
        ;; default to Clozure CL on macOS
        (setq slime-default-lisp 'ccl64)
      ;; default to SBCL on Linux and Windows
      (setq slime-default-lisp 'sbcl))

    ;; Add fancy slime contribs
    (setq slime-contribs '(slime-fancy slime-cl-indent))

    (setq slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
          slime-net-coding-system 'utf-8-unix
          slime-enable-evaluate-in-emacs t)

    (add-hook 'slime-repl-mode-hook (lambda ()
                                      (whitespace-mode -1)))

    (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)

    (when (eq system-type 'darwin)
      (setq common-lisp-hyperspec-root
            "/usr/local/share/doc/hyperspec/HyperSpec/")
      (setq common-lisp-hyperspec-symbol-table
            (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
      (setq common-lisp-hyperspec-issuex-table
            (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))))


;; Fonts setting
(when (fboundp 'set-fontset-font)
  ;; Setting Default Font
  (set-face-attribute 'default nil :font "Inconsolata")

  ;; Setting Unicode Font
  (set-fontset-font t 'unicode "STIX Two Math")

  ;; Setting CJK Font
  (set-fontset-font t 'han "Noto Serif CJK SC")
  (set-fontset-font t 'han "Noto Serif CJK TC" nil 'append)
  (set-fontset-font t 'kana "Noto Serif CJK JP")
  (set-fontset-font t 'hangul "Noto Serif CJK KR")

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (when (eq system-type 'darwin)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))


;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
