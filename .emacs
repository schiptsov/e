;;; -*- mode: emacs-lisp; coding: utf-8; -*-

(setq-default load-prefer-newer t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; we use straight.el
(setq package-enable-at-startup nil)

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

(set-charset-priority 'unicode)
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; We use the 'straight bloatware
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package will use 'straight
(straight-use-package 'diminish)
(straight-use-package '(use-package :type built-in))

;; Use the latest version
(straight-use-package 'org)

;; we will use this DSLs (a set of macros)
(setq
 use-package-always-defer nil   ;; should be nil for :defer to work
 use-package-always-ensure t    ;; should be t for straight
 use-package-compute-statistics nil
 use-package-verbose nil)

(straight-use-package 'async)
(straight-use-package 'org)
(straight-use-package 'ob-async)

(menu-bar-mode t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

(setq select-enable-clipboard t)
(setq select-enable-primary t)

(add-to-list 'default-frame-alist '(font . "SF Mono Light 16"))

(setq-default font-use-system-font t)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)

(set-face-font 'default  (font-spec :family "SF Mono" :foundry "APPL" :weight 'light :size 22 :height 158))

(set-face-font 'fixed-pitch  (font-spec :family "SF Mono" :foundry "APPL" :weight 'light :size 22 :height 158))
(set-face-font 'fixed-pitch-serif (font-spec :family "SF Pro Display" :foundry "APPL" :weight 'light :size 22 :height 158))
(set-face-font 'variable-pitch (font-spec :family "SF Pro Text" :foundry "APPL" :weight 'light :size 22 :height 158))

(set-face-attribute 'font-lock-constant-face nil :weight 'normal)
(set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil :italic t)
(set-face-attribute 'font-lock-doc-face nil :italic t)

(defface variable-pitch-serif
  '((t (:family "serif")))
  "A variable-pitch face with serifs."
  :group 'basic-faces)

(defcustom variable-pitch-serif-font (font-spec :family "SF Pro Text" :weight 'light)
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
         (set-face-attribute 'variable-pitch-serif nil :font value)
         (set-default-toplevel-value symbol value)))

(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-y") 'yank)
(global-set-key (kbd "M-y") 'yank-pop)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq inhibit-splash-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(setq-default font-use-system-font t)
(setq-default font-lock-maximum-decoration t)

(setq whitespace-style '(face spaces tabs newline space-mark tab-mark))
(global-whitespace-mode t)

(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode t)

(setq sentence-end-double-space nil)

(setq-default word-wrap t)

(setq-default truncate-lines nil)

(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)
                            (setq indicate-empty-lines t)))

(setq-default tab-width 4)
(setq-default fill-column 72)
(set-fill-column 72)
(auto-fill-mode t)

(global-highlight-changes-mode -1)

(global-display-fill-column-indicator-mode -1)

(global-visual-line-mode t)
(global-hl-line-mode t)

(global-subword-mode t)

(show-paren-mode t)
(transient-mark-mode t)

(setq-default electric-indent-chars '(?\n ?\^?))
(electric-pair-mode t)
(electric-indent-mode t)

(delete-selection-mode t)

(abbrev-mode t)
(setq save-abbrevs 'silently)
(bind-key "M-/" 'hippie-expand)

(global-eldoc-mode t)

(auto-save-visited-mode t)

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook #'save-all)

(recentf-mode t)

(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode t)

(straight-use-package 'diminish)
(straight-use-package 'delight)
(straight-use-package '(bind-key :type built-in))
(straight-use-package '(use-package :type built-in))

(straight-use-package 'eldoc)

(use-package xref
  :straight t
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (setq xref-search-program 'ripgrep))

(use-package unicode-fonts
  :straight t
  :config
  (unicode-fonts-setup))

(use-package ligature
  :straight t
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (global-ligature-mode 't))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-nord t))

(use-package guru-mode
  :straight t
  :config
  (guru-global-mode t))

(use-package nyan-mode
  :config
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (nyan-mode t))

(use-package hide-mode-line
  :straight t
  :config
  (hide-mode-line-mode t))

(use-package focus
  :straight t
  :commands (focus-mode focus-read-only-mode))

(use-package writeroom-mode
  :straight t
  :commands writeroom-mode)

(use-package info+
  :straight t)

(use-package info-colors
  :straight t
  :hook (Info-selection  . info-colors-fontify-node))

  (use-package which-key
  :straight t
  :config
  (which-key-mode t))

(use-package which-key-posframe
  :straight t
  :init (which-key-posframe-mode t))

(use-package xclip
  :straight t
  :config
  (setq select-enable-primary t)
  (xclip-mode t))

(use-package mixed-pitch
  :straight t
  :hook
  ((text-mode . mixed-pitch-mode)
  (help-mode . mixed-pitch-mode)
  (org-mode . mixed-pitch-mode)
  (latex-mode . mixed-pitch-mode)
  (markdown-mode . mixed-pitch-mode)
  (gfm-mode . mixed-pitch-mode)
  (info-mode . mixed-pitch-mode)
  (nov-mode . mixed-pitch-mode))
  :config
  (variable-pitch-mode -1))

(use-package vterm
  :straight t)

(use-package rg
  :straight t)

(use-package fzf
  :straight t)

(use-package ag
  :straight t)

(use-package counsel
     :straight t
     :bind (("C-x C-f" . counsel-find-file)
            ("C-x b" . ivy-switch-buffer)
            ("C-x B" . counsel-switch-buffer-other-window)
            ("C-c C-r" . counsel-recentf)
            ("C-x d" . counsel-dired)
            ("M-s r" . counsel-rg)
            ("C-c r" . counsel-rg)
            ("C-c z" . counsel-fzf)
            ("M-s z" . counsel-fzf)
            ("C-c g" . counsel-git)
            ("C-c a" . counsel-ag)
            :map ivy-minibuffer-map ("C-r" . counsel-minibuffer-history))
     :config
     (global-set-key (kbd "M-x") 'counsel-M-x)
     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
     (global-set-key (kbd "C-x l") 'counsel-locate)
     (global-set-key (kbd "C-h f") 'counsel-describe-function)
     (global-set-key (kbd "C-h v") 'counsel-describe-variable)
     (global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
     (global-set-key (kbd "C-h u") 'counsel-unicode-char)
     (global-set-key (kbd "C-h l") 'counsel-find-library)
     (global-set-key (kbd "C-c j") 'counsel-git-grep)
     (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
     (add-to-list 'savehist-additional-variables 'counsel-compile-history)
     (counsel-mode t)
     (global-set-key (kbd "M-y") 'counsel-yank-pop))

(use-package helpful
  :straight t
  :after counsel
  :config
  (defalias 'describe-function 'helpful-callable)
  (defalias 'describe-variable 'helpful-variable)
  (defalias 'describe-key 'helpful-key)
  ;;(global-set-key (kbd "C-h f") #'helpful-callable)
  ;;(global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode t))

(use-package smartparens
  :straight t
  :hook (prog-mode . (smartparens-strict-mode show-smartparens-mode))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (push 'company-capf company-backends)
  (global-company-mode t))

(use-package company-posframe
  :straight t
  :after company
  :hook (company-mode . company-posframe-mode)
  :config
  (setq company-tooltip-minimum-width 40))

(use-package company-quickhelp
        :straight t
        :after company
        :custom
        (company-quickhelp-delay 3)
        :config
        (company-quickhelp-mode t))

(use-package company-math
  :straight t
  :after company
  :config
  (setq company-math-disallow-unicode-symbols-in-faces t)
  ;;(add-to-list 'company-backends 'company-math-symbols-latex)
  (append '((company-math-symbols-latex company-math-symbols-unicode))
          company-backends))

(use-package company-org-block
  :straight t
  :after company
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook (org-mode . (lambda ()
                      (add-to-list 'company-backends 'company-org-block)
                      (company-mode +1))))

(use-package company-statistics
  :straight t
  :after company
  :config
  (company-statistics-mode))

(use-package company-web
  :straight t
  :after company)


(use-package avy
  :straight t
  :defer t)

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode t))

(use-package flycheck-pos-tip
  :straight t
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package avy-flycheck
  :straight t
  :after flycheck
  :config
  (avy-flycheck-setup))

(use-package yasnippet
  :straight t
  :after company
  :config
  (push 'company-yasnippet company-backends)
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet
  :straight t)

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(use-package auto-yasnippet
  :straight t
  :adter yasnippet
  :config
  (global-set-key (kbd "C-c C-y w")   #'aya-create)
  (global-set-key (kbd "C-c C-y TAB") #'aya-expand)
  (global-set-key (kbd "C-c C-y SPC") #'aya-expand-from-history)
  (global-set-key (kbd "C-c C-y d")   #'aya-delete-from-history)
  (global-set-key (kbd "C-c C-y c")   #'aya-clear-history)
  (global-set-key (kbd "C-c C-y n")   #'aya-next-in-history)
  (global-set-key (kbd "C-c C-y p")   #'aya-previous-in-history)
  (global-set-key (kbd "C-c C-y s")   #'aya-persist-snippet)
  (global-set-key (kbd "C-c C-y o")   #'aya-open-line))

(use-package lsp-mode
  :straight t
  :hook (prog-mode . lsp)
  :init
  (setq lsp-auto-configure t))

(use-package company-lsp
  :straight '(company-lsp :type git :host github :repo "tigersoldier/company-lsp")
  :after company
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :straight t
  :hook (prog-mode . lsp-ui-mode)
  :init
  (setq lsp-auto-configure t)
  :config
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-ui-doc-show-with-cursor t))

(use-package rainbow-mode
  :straight t
  :config
  (rainbow-mode t))

(use-package electric-spacing
  :straight t
  :defer t
  :hook (prog-mode . electric-spacing-mode))

(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode t))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode t))

(use-package expand-region
  :straight t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package ggtags
  :straight t
  :hook (prog-mode . ggtags-mode))

(use-package counsel-gtags
  :straight t
  :init
  (setq counsel-gtags-ignore-case t
        counsel-gtags-auto-update t)
  :hook (ggtags-mode . counsel-gtags-mode))

(use-package emacs-lisp-mode
  :defer t
  :hook (emacs-lisp-mode . ggtags-mode)
  :hook (emacs-lisp-mode . semantic-mode)
  :hook (emacs-lisp-mode . auto-compile-mode)
  :config
  (with-eval-after-load 'semantic
    (semantic-default-elisp-setup)))

(use-package highlight-quoted
  :straight t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-numbers
  :straight t
  :hook (emacs-lisp-mode . highlight-numbers-mode))

(use-package ielm
  :defer t
  :hook (ielm-mode . smartparens-strict-mode)
  :hook (ielm-mode . rainbow-delimiters-mode)
  :hook (ielm-mode . eldoc-mode)
  :hook (ielm-mode . highlight-quoted-mode)
  :hook (ielm-mode . highlight-numbers-mode))


