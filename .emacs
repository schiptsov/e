;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8; -*-

;;; This file sets up a lot of outdated and obsolete packages
;;; so it is already bloated.
;;;
;;; The easiest thing to do is to comment out whole blocks of code
;;;
;;; In the modern world 'lsp-mode and 'lsp-ui are enough
;;; And they use
;;; - completions (built-in)
;;; - corfu, vertico, marginalia, orderless (enchancements)
;;; - ivy
;;; - eldoc
;;; - yasnippet
;;; - company
;;; - flycheck
;;; - flymake (optional)
;;;
;;; so these have to be set up properly (as /global modes/)
;;;
;;; As a quick hack, one just have to tell 'straight to
;;;
;;; (straight-use-package lsp-ui)
;;;
;;; and it will install all the minimal required dependencies
;;; so one has to customize them later.
;;;
;;; use hooks and implicit defers,
;;; not afters and configs
;;; some packages work only as modes in configs
;;;
;;; This file is already bloated and needs a cleanup
;;;

(setq-default load-prefer-newer t)

;; a temporary kludge
;; TODO: remove this line

(setq byte-compile-warnings t)
(setq native-comp-async-report-warnings-errors nil)

;; a simple GC hack
(add-function :after after-focus-change-function
              (defun garbage-collect-maybe ()
                (unless (frame-focus-state)
                  (garbage-collect))))

;; another nice hack
(defun save-all ()
  (interactive)
  (save-some-buffers t)
  (garbage-collect))
(add-hook 'focus-out-hook #'save-all)

;; be careful
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq enable-local-variables :all)

;; (setq-default socks-override-functions 1)
(setq-default url-gateway-method 'socks)
(setq-default socks-server '("Tor" "127.0.0.1" 9050 5))
(setq-default socks-noproxy '("127.0.0.1"))
(require 'socks)

;; (customize-set-variable 'url-proxy-services
;;                           '(("http"  . "127.0.0.1:8118")
;;                             ("https" . "127.0.0.1:9060")))

;; we use straight.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(setq frame-inhibit-implied-resize t)

;; this has to be set before 'use-package and 'straight.el
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_COLLATE" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

(set-charset-priority 'unicode)
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
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

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)

;; we will use this DSLs (a set of macros)
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose t)

;; use-package will use 'straight
(straight-use-package '(use-package :type built-in))
(straight-use-package '(bind-key :type built-in))
(straight-use-package 'diminish)
(straight-use-package 'delight)

(straight-use-package 'async)
(use-package ob-async
  :hook (org-load . (lambda () (require 'ob-async))))

;; less distractions
(use-package emacs
  :init
  (menu-bar-mode t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode -1)
  :config
  (setq use-file-dialog nil
        use-dialog-box nil)
  (setq inhibit-splash-screen t
        inhibit-startup-buffer-menu t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        inhibit-startup-screen t
        initial-scratch-message "")
  (setq x-underline-at-descent-line nil
        underline-minimum-offset 1)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0)
  (setq-default font-use-system-font t
                font-lock-maximum-decoration t)
  (global-font-lock-mode t))

;; selection
(use-package emacs
  :config
  (setq select-enable-clipboard t
        select-enable-primary t))

(straight-use-package 'spacemacs-theme)
(straight-use-package 'ef-themes)

;; default
(straight-use-package 'gruvbox-theme)

;; has to be early, because every face should inherit
(use-package doom-themes
  :demand
  :hook (after-init . (lambda ()
                        (load-theme 'doom-gruvbox t)))
  :hook (org-mode . (lambda ()
                      (require 'doom-themes-ext-org)
                      (doom-themes-org-config)))
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally
  :config
  (load-theme 'doom-gruvbox t))

;; the font section
(add-to-list 'default-frame-alist '(font . "SF Mono Light 16"))

(set-face-font 'default  (font-spec :family "SF Mono" :foundry "APPL" :weight 'light :size 16 :height 158))

(set-face-font 'fixed-pitch  (font-spec :family "SF Mono" :foundry "APPL" :weight 'light :size 16 :height 158))
(set-face-font 'fixed-pitch-serif (font-spec :family "SF Pro Display" :foundry "APPL" :weight 'light :size 16 :height 158))
(set-face-font 'variable-pitch (font-spec :family "SF Pro Text" :foundry "APPL" :weight 'light :size 16 :height 158))

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

(set-fontset-font t 'unicode (font-spec :family "Noto Emoji") nil 'prepend)
(set-fontset-font t 'devanagari (font-spec :family "Noto Sans Devanagari" :weight 'light))
(set-fontset-font t 'tibetan (font-spec :family "Noto Serif Tibetan" :weight 'light))

(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)

(use-package emacs
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default tab-always-indent 'complete)
  (setq-default completion-cycle-threshold 3)

  (add-hook 'sh-mode-hook (lambda () (setq indent-tabs-mode t)))

  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key (kbd "C-y") 'yank)
  (global-set-key (kbd "M-y") 'yank-pop))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package whitespace
  :diminish
  :straight '(:type built-in)
  :after doom-themes
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation
  empty space-after-tab space-mark tab-mark))))

(global-set-key (kbd "C-c w") #'whitespace-mode)

(defvar base-prettify-symbols-alist
  '(("<=" . ?≤)
    (">=" . ?≥)
    ("<-" . ?←)
    ("->" . ?→)
    ("<=" . ?⇐)
    ("=>" . ?⇒)
    ("lambda" . ?λ ))
  )

;; stolen from Doom
(defvar +ligatures-extra-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    :quote         "“"
    :quote_end     "”"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    :list          "𝕃"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :union         "⋃"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "<U+E135>" ;; FIXME: find a non-private char
    :dot           "•")
  "Maps identifiers to symbols, recognized by `set-ligatures'.")

(defvar extra-prettify-symbols-alist)

;; stolen from Alexandria
(cl-defun plist-alist (l &optional (acc '()))
  "stolen from Alexandria"
  (cond ((null l) (nreverse acc))
        (t (plist-alist (cddr l) (cons (cons (car l) (cadr l)) acc)))))

;; a systematic, principle-guided way
(setq extra-prettify-symbols-alist (append (plist-alist
                                            (mapcar (lambda (s)
                                                      (cond ((symbolp s) (substring  (symbol-name s) 1 nil))
                                                            (t s))) +ligatures-extra-symbols))
                                           base-prettify-symbols-alist))

(defun gas-lisp-prettify-symbols-hook ()
  "Set pretty symbols for lisp modes."
  (setq prettify-symbols-alist base-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode t))

(defun gas-python-prettify-symbols-hook ()
  "Set pretty symbols for Python."
  (setq prettify-symbols-alist
        (append '(("def" . ?ƒ) ("None" . "∅"))
                extra-prettify-symbols-alist))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode t))

(defun gas-js-prettify-symbols-hook ()
  "Set pretty symbols for JavaScript."
  (setq prettify-symbols-alist
        (append '(("function" . ?ƒ)) extra-prettify-symbols-alist)))

(defun gas-clj-prettify-symbols-hook ()
  "Set pretty symbols for Clojure(script)."
  (setq prettify-symbols-alist
        (append '(("fn" . λ)) extra-prettify-symbols-alist)))

(defun other-prettify-symbols-hook ()
  "Set pretty symbols for non-lisp programming modes."
  (setq prettify-symbols-alist
        (append '(("==" . ?≡)
                  ("!=" . ?≠)
                  (":=" . ?⇐)
                  ("::" . ?∷))
                extra-prettify-symbols-alist))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode t))

;; Hook 'em up.
(add-hook 'emacs-lisp-mode-hook #'gas-lisp-prettify-symbols-hook)
(add-hook 'python-mode-hook     #'gas-python-prettify-symbols-hook)
(add-hook 'web-mode-hook        #'other-prettify-symbols-hook)
(add-hook 'js-mode-hook         #'gas-js-prettify-symbols-hook)
(add-hook 'prog-mode-hook       #'other-prettify-symbols-hook)
(add-hook 'clojure-mode-hook    #'gas-clj-prettify-symbols-hook)

(use-package prettify-symbols
  :straight '(:type built-in)
  :init
  (setq-default prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (global-prettify-symbols-mode t))

(setq sentence-end-double-space nil)

(setq-default word-wrap t)

(setq-default truncate-lines nil)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))

;; cannot fit 2 pages on a 1080p screen
(use-package emacs
  :config
  (setq-default tab-width 4)
  (setq-default fill-column 72)
  (set-fill-column 72)
  (auto-fill-mode t))

;; yet another cool hack
(when (executable-find "fd")
  (setq find-program "fd"))

(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))

;; uses a face unsupported by themes
(global-highlight-changes-mode -1)

;; ugly
(global-display-fill-column-indicator-mode -1)

(global-visual-line-mode t)
(global-hl-line-mode t)

(global-subword-mode t)

;; also show-smartparens-mode
(show-paren-mode t)

;; C-SPC
(transient-mark-mode t)
(setq set-mark-command-repeat-pop t)

;; clashes with spartparens
(setq-default electric-indent-chars '(?\n ?\^?))
(setq electric-pair-preserve-balance t)
(electric-pair-mode -1)

;; but not in LISPs
(electric-indent-mode t)

(delete-selection-mode t)

(setq save-abbrevs 'silently)
(abbrev-mode t)
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)
;; (bind-key "M-/" 'hippie-expand)

;; this will be a corfu completion, and vertico
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(auto-save-visited-mode t)

(use-package recentf
  :demand
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                        ,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)))
  (recentf-mode t))

;; will be later rebound by consult
(defun find-recent-file ()
  "Find a file that was recently visted using `completing-read'."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))
(global-set-key (kbd "C-c r") #'find-recent-file)

(use-package savehist
  :config
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  (savehist-mode t))

(use-package saveplace
  :demand
  :config
  (save-place-mode))

(use-package midnight
  :demand
  :config
  (setq midnight-period 7200)
  (midnight-mode 1))

;; ugly
(use-package imenu-anywhere
  :demand
  :bind
  ("M-i" . ivy-imenu-anywhere))

;; just use consult-yank-pop
(use-package browse-kill-ring
  :commands browse-kill-ring
  :config
  (setq browse-kill-ring-quit-action 'save-and-restore))

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)  ; Overrides `mark-page'
(global-set-key (kbd "C-x C-n") 'next-buffer)      ; Overrides `set-goal-column'

(use-package gcmh
  :demand t
  :diminish 'gcmh-mode
  :config
  (gcmh-mode t))

(use-package pinentry
  :config (pinentry-start))

(use-package epg
  :straight '(:type built-in)
  :config
  (setq epg-pinentry-mode 'loopback)
  (setq epg-gpg-program "gpg"))

(use-package epa-file
  :straight '(:type built-in)
  :custom
  (epa-file-select-keys 'silent)
  :config
  (setq epa-pinentry-mode 'loopback)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq epa-file-select-keys nil)
  (epa-file-enable))

(use-package auth-source
  :straight (:type built-in)
  :config
  (setq auth-sources '("~/.authinfo.gpg")
        auth-source-cache-expiry nil))

(use-package pass
  :config
  (setf epa-pinentry-mode 'loopback)
  (auth-source-pass-enable))

(use-package whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode)
  :config
  (diminish 'whitespace-cleanup-mode))

(straight-use-package 'crypt++)

(use-package org-crypt
  :straight (:type built-in)
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom
  (org-crypt-key "lngnmn2@yahoo.com"))

;; a nice hack
(use-package grep
  :straight '(:type built-in)
  :init
  (setq-default grep-highlight-matches t
                grep-scroll-output t)
  :config
  (when (executable-find "rg")
    (setq grep-program "rg")
    (grep-apply-setting
     'grep-find-command
     '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))
    (add-to-list 'grep-find-ignored-directories "target")
    (add-to-list 'grep-find-ignored-directories "node_modules")
    (setq wgrep-enable-key "w")
    (global-set-key (kbd "C-x C-g") 'grep-find)))


;; (defun colorize-compilation-buffer ()
;;   "Enable colors in the *compilation* buffer."
;;   (require 'ansi-color)
;;   (let ((inhibit-read-only t))
;;     (ansi-color-apply-on-region (point-min) (point-max))))
;; (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

(setq-default compilation-scroll-output t)

(defun shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add 'shell-command-on-region :after 'shell-command-in-view-mode)

(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun colourise-compilation-buffer ()
    "Enable colors in the *compilation* buffer."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colourise-compilation-buffer))

(use-package auto-compile
  :demand
  :config (auto-compile-on-load-mode))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(defun my-find-tag ()
  "Use `completing-read' to navigate to a tag."
  (interactive)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

(global-set-key (kbd "C-c d") #'my-find-tag)

(global-set-key (kbd "C-c f") #'ffap)

;; disks are cheap
(use-package emacs
  :custom
  (auto-save-default t)
  (auto-save-visited-mode t)
  (make-backup-files t)
  (backup-by-copying t)
  (create-lockfiles t)
  (version-control t)
  (vc-make-backup-files t)
  (delete-old-versions t))

(use-package super-save
  :diminish
  :config
  (super-save-mode +1))

(use-package undo-tree
  :demand
  :diminish
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode t))

(straight-use-package 'fontaine)

(use-package unicode-fonts
  :demand
  :config
  (unicode-fonts-setup))

(use-package ligature
  :demand
  :diminish
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (global-ligature-mode 't))

(use-package all-the-icons
  :demand)

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode)
;;   :config
;;   (setq emojify-emoji-styles 'unicode)
;;   (emojify-set-emoji-styles emojify-emoji-styles))

(use-package mixed-pitch
  :demand
  :hook
  ((text-mode . mixed-pitch-mode)
   (help-mode . mixed-pitch-mode)
   (Man-mode . mixed-pitch-mode)
   (Info-mode . mixed-pitch-mode)
   (org-mode . mixed-pitch-mode)
   (LaTeX-mode . mixed-pitch-mode)
   (latex-mode . mixed-pitch-mode)
   (tex-mode . mixed-pitch-mode)
   (markdown-mode . mixed-pitch-mode)
   (gfm-mode . mixed-pitch-mode)
   (nov-mode . mixed-pitch-mode))
  :hook (mixed-pitch-mode . (lambda ()
                              (setq mixed-pitch-face 'variable-pitch)
                              (solaire-mode t)
                              (variable-pitch-mode t))))

(use-package visual-fill-column
  :demand
  :hook (visual-line-mode . visual-fill-column-mode))

(defun display-ansi-colors ()
  "Render colors in a buffer that contains ASCII color escape codes."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package ispell
  :straight (:type built-in)
  :demand
  :config
  (add-to-list 'ispell-extra-args "-C")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (setq ispell-encoding-command "utf-8")
  (setq ispell-skip-html t))

(use-package flyspell
  :straight (:type built-in)
  :demand
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-large-region t)
  (setq flyspell-consider-dash-as-word-delimiter-flag t)
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
  (flyspell-mode t))

(use-package flyspell-correct-ivy
  :demand
  :after ivy)

(use-package spell-fu
  :config (global-spell-fu-mode t))

(use-package flycheck-languagetool
  :hook (text-mode . flycheck-languagetool-setup)
  :init
  (setq flycheck-languagetool-server-jar "/opt/LanguageTool/languagetool-server.jar"))

(use-package sgml-mode
  :hook
  ((html-mode . sgml-electric-tag-pair-mode)
   (html-mode . sgml-name-8bit-mode))
  :custom
  (sgml-basic-offset 2)
  :config
  (setq sgml-xml-mode t)
  (setq sgml-transformation-function 'upcase)
  (setq sgml-set-face t)
  (setq sgml-auto-activate-dtd t)
  (setq sgml-indent-data t)
  (defadvice sgml-delete-tag (after reindent activate)
    (indent-region (point-min) (point-max))))

(setq sgml-markup-faces '(
    (start-tag . font-lock-keyword-face)
    (end-tag . font-lock-keyword-face)
    (comment . font-lock-comment-face)
    (pi . font-lock-constant-face) ;; <?xml?>
    (sgml . font-lock-type-face)
    (doctype . bold)
    (entity . italic)
    (shortref . font-lock-reference-face)))

(use-package tidy
  :config
  (setq sgml-validate-command "tidy"))

(use-package tagedit
  :hook (sgml-mode . tagedit-mode )
  :config
  (with-eval-after-load 'sgml-mode
    (tagedit-add-paredit-like-keybindings)
    (define-key tagedit-mode-map (kbd "M-?") nil)
    (define-key tagedit-mode-map (kbd "M-s") nil)))

(use-package nxml-mode
  :straight (:type built-in)
  :init
  (fset 'xml-mode 'nxml-mode)
  (fset 'html-mode 'nxml-mode)
  :config
  (setq nxml-child-indent 2)
  (setq nxml-attribute-indent 2)
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t))

(defun tidy-html ()
  "Tidies the HTML content in the buffer using `tidy'"
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "tidy -i -w 120 -q"
   ;; output buffer
   (current-buffer)
     ;; replace?
   t
   ;; name of the error buffer
   "*Tidy Error Buffer*"
   ;; show error buffer?
   t))

;; an outdated but cool
(straight-use-package 'nxhtml)

(straight-use-package 'htmlize)
(straight-use-package 'engrave-faces)
;; (straight-use-package 'engrave-faces-latex)

(use-package latex-mode
  :straight '(:type built-in))

;; we absolutely want to edit latex within org-mode
(use-package latex
  :demand
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :commands (latex-mode LaTeX-mode)
  :hook (LaTeX-mode . LaTeX-math-mode)
  :hook ((tex-mode-local-vars-hook
          latex-mode-local-vars-hook)
         . #'lsp-deferred)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Don't start the Emacs server when correlating sources.
        TeX-source-correlate-start-server nil
        ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Just save, don't ask before each compilation.
        TeX-save-query nil)
  (setq TeX-PDF-mode t)
  (setq-default TeX-master nil))

(use-package auctex-latexmk
  :demand
  :after latex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package tex-fold
  :demand
  :straight '(:type built-in)
  :after latex
  :hook (TeX-mode . TeX-fold-mode))

(use-package texfrag
  :demand
  :after latex
  :hook (after-init . texfrag-global-mode))

(use-package preview
  :demand
  :after latex
  :straight '(:type built-in)
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4))

(use-package latex-preview-pane
  :demand
  :after latex
  :commands org-latex-pane-mode)

(use-package cdlatex
  :demand
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; Use \( ... \) instead of $ ... $.
  (setq cdlatex-use-dollar-to-ensure-math t))

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(use-package company-auctex
  :demand
  :after company
  :config
  (add-to-list 'company-backends 'company-auctex-macros t))

(use-package adaptive-wrap
  :demand
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 1))

(use-package xenops
  :after latex
  :hook (LaTeX-mode . xenops-mode))

;; Use the latest version
(straight-use-package 'org-contrib)

(straight-use-package 'async)
(use-package ob-async
  :hook (org-load . (lambda () (require 'ob-async))))

;; load this early
(straight-use-package 'ob-rust)
(straight-use-package 'ob-sml)
(use-package ob-erlang
  :straight '(ob-erlang :type git :host github :repo "xfwduke/ob-erlang"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (org . t)
   (shell . t)
   (awk . t)
   (C . t)
   (scheme . t)
   (ocaml . t)
   (python . t)
   (gnuplot . t)
   (octave . t)
   (rust . t)
   (haskell . t)
   (sml . t)
   (erlang . t)
   ))

(use-package org
  :custom
  (org-src-tab-acts-natively t)
  :hook (org-mode . (lambda ()
                      (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                      (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)))
  :hook (org-mode . (lambda ()
                      (set-face-background 'org-block 'unspecified) ;; fix
                      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-link nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-special-keyword nil
                                          :inherit 'fixed-pitch)
                      (mixed-pitch-mode t)
                      (variable-pitch-mode t)))
  :bind (:map org-mode-map
              ("C-c a" . org-agenda)
              ("C-c o b" . org-back-to-heading)
              ("C-c o p" . org-display-outline-path))
  :config
  (require 'xdg)
  (setq org-export-coding-system 'utf-8-unix)
  (setq org-html-coding-system 'utf-8-unix)
  (setq org-ascii-charset 'utf-8)

  (setq org-use-property-inheritance t)

  (setq org-export-with-sub-superscripts '{})

  (setq org-inline-src-prettify-results '("⟨" . "⟩"))

  (setq org-directory (expand-file-name "org" (xdg-data-home)))
  (setq org-agenda-files (list org-directory))

  (setq org-default-notes-file (expand-file-name "~/NOTES.org"))

  (setq org-export-headline-levels 5) ; I like nesting

  (setq org-refile-use-outline-path 'file)

  (setq org-reverse-note-order t)

  (setq org-catch-invisible-edits 'show-and-error
        org-completion-use-ido t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-insert-heading-respect-content t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…")

  (setq org-startup-indented t
        org-startup-folded t)

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 2
        org-use-property-inheritance t
        org-list-allow-alphabetical t
        org-export-in-background t)

  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-done-headline t)

  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t)

  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)

)

;; will stumble on emacs-lisp blocks
(use-package org-src-context
  :straight '(:host github :repo "karthink/org-src-context")
  :hook (org-mode . org-src-context-mode))

(use-package solaire-mode
  :hook (mixed-pitch-mode .  solaire-mode-reset)
  :hook (prog-mode . solaire-mode-reset)
  :hook (after-init . (lambda ()
                        (solaire-global-mode +1))))

(use-package org-pretty-tags
  :diminish
  :hook (org-mode . org-pretty-tags-mode))

(use-package org-pretty-table
  :diminish
  :straight '(:host github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

(use-package org-modern
  :demand
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table t)
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")))

(use-package org-reverse-datetree
  :after org
  :demand)

(use-package org-appear
  :demand
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package ox-clip
  :after org
  :config
  (setq org-hugo-front-matter-format "yaml"))

(use-package ox-html
  :straight (:type built-in)
  :after ox
  :config
  (setq org-html-coding-system 'utf-8-unix))

(use-package ox-gfm
  :commands (org-gfm-export-as-markdown org-gfm-export-to-markdown)
  :after ox)

(use-package ox-hugo
  :after ox)

(straight-use-package 'transient)

(use-package org-pandoc-import
  :straight '(org-pandoc-import :type git :host github :repo "tecosaur/org-pandoc-import"))

(use-package guru-mode
  :demand
  :delight
  :config (guru-global-mode t))

(use-package nyan-mode
  :demand
  :config
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (nyan-mode t))

(use-package hide-mode-line
  :config (hide-mode-line-mode t))

(use-package focus
  :commands (focus-mode focus-read-only-mode)
    :config
        (add-to-list 'focus-mode-to-thing '(org-mode . paragraph)))

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . (lambda ()
                      (visual-line-mode t)
                      (visual-fill-column-mode t)
                      (whitespace-mode -1) ;; fucks up nov-mode
                      (mixed-pitch-mode t)
                      (variable-pitch-mode t)
                      (focus-read-only-mode t)
                      (hide-mode-line-mode t)))
  :config
  (setq nov-text-with 72))

(use-package org-indent
  :straight '(:type built-in)
  :diminish
  :after org
  :hook (org-mode . org-indent-mode))

(use-package org-rich-yank
  :after org
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package org-cliplink
  :after org)

(use-package org-download
  :after org)

(use-package org-web-tools
  :after org)

;; (use-package predictive
;;   :config
;;   (set-default 'predictive-auto-add-to-dict t)
;;   (setq predictive-main-dict 'rpg-dictionary
;;         predictive-auto-learn t
;;         predictive-add-to-dict-ask nil
;;         predictive-use-auto-learn-cache nil
;;         predictive-which-dict t))


(use-package org-latex-preview
  :straight '(:type built-in)
  :after org
  :hook (org-mode . org-latex-preview)
  :commands org-latex-preview)

(use-package org-auctex
  :after org
  :straight '(:host github :repo "karthink/org-auctex")
  :hook (org-mode . org-auctex-mode))

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(with-eval-after-load 'org
  (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PLAN(c)" "WAITING" "SOMEDAY(s)" "READ(r)" "WRITE(w)" "PROGRAM(p)" "WATCH(v)" "LISTEN(l)" "ROUTINE" "DAILY" "DISCIPLINE" "|" "DONE(d)" "DELEGATED" "CANCELED")))

  (setq org-tag-alist
        '(("reading" . ?r) ("writing" . ?w) ("math" . ?m) ("programming" . ?p) ("assistant" . ?a) ("trading" . ?t) ("emacs" . ?e) ("neovim" . ?v) ("chore" . ?c) ("daily" . ?d) ("routine" . ?r)))

  (setq org-capture-templates
        '(("i" "INBOX" entry (file+headline "~/INBOX.org" "Inbox")
           "* %?\n  %i\n  %a" :prepend t :kill-buffer t)
          ("t" "TODO" entry (file+headline "~/TODO.org" "Tasks")
           "* TODO %?\n SCHEDULED: %t\n %i\n  %a" :prepend t :kill-buffer t)
          ("p" "PLAN" entry (file+headline "~/PLAN.org" "Plans")
           "* PLAN %?\n  %i\n  %a" :prepend t :kill-buffer t)
          ("n" "NOTE" entry (file+datetree "~/NOTES.org" "Notes")
           "* %?\nEntered on %U\n  %i\n  %a" :prepend t :kill-buffer t)
					("K" "Cliplink capture task" entry (file "")
					 "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)))
  )

;; "unstructured" note taking
;; we use libraries from a bloatware, maybe just a single function
;; this is also delegation and use of stable interfaces
(use-package org-roam
  :after org
  :diminish 'Org-roam
  ;; :hook (after-init . org-roam-mode)
  :custom
  (org-roam-directory (expand-file-name "org-roam" (xdg-data-home)))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n")
        :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
           :map org-mode-map
           ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

;; this has the same universal notions of sets and relations as SQL
;; https://github.com/alphapapa/org-ql/blob/master/examples.org
(use-package org-ql
  :after org
  :commands (org-ql-search org-ql-find))

(use-package org-super-agenda
  :after org-agenda
  :hook (org-agenda . org-super-agenda-mode))

(with-eval-after-load 'org-agenda
  (let ((inhibit-message t))
    (org-super-agenda-mode t)))

(with-eval-after-load 'org-agenda
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t)

;; https://orgmode.org/worg/org-tutorials/tracking-habits.html
  (setq org-agenda-custom-commands
        '(("h" "Habits"
           ((agenda ""))
           ((org-agenda-show-log t)
            (org-agenda-ndays 7)
            (org-agenda-log-mode-items '(state))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
          ("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :todo "TODAY"
                            :scheduled today
                            :order 1)
                           (:habit t)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                             :todo "NEXT"
                             :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
  )

(use-package idle-org-agenda
  :after org-agenda
  :demand
  :config (idle-org-agenda-mode))

(straight-use-package 'info+)

(use-package info-colors
  :hook (Info-selection  . info-colors-fontify-node))

(add-hook 'after-init-hook 'help-quick)

(use-package which-key
  :diminish
  :config(which-key-mode t))

(use-package which-key-posframe
  :init (which-key-posframe-mode t))

(use-package xclip
  :demand
  :config
  (setq select-enable-primary t)
  (xclip-mode t))

(use-package djvu)

(use-package djvu3
  :after djvu
  :straight '(:host github
                      :repo "dalanicolai/djvu3")
  :mode ("\\.djvu\\'" . djvu-read-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode))

(use-package saveplace-pdf-view
  :after pdf-view-mode)

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-gfm-additional-languages '("python" "sh")
        markdown-make-gfm-checkboxes-buttons t
        markdown-fontify-whole-heading-line t)
  :hook (markdown-mode . (lambda ()
                           (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
                           (set-face-attribute
                            'markdown-inline-code-face nil :inherit 'fixed-pitch)
                           (mixrd-pitch-mode t)
                           (variable-pitch-mode t)))
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package poly-markdown
  :after markdown-mode
  :hook (markdown-mode . poly-markdown-mode))

(use-package grip-mode
  :hook (markdown-mode . grip-mode))

(use-package vterm
  :hook (vterm-mode . yas-minor-mode)
  :commands (vterm vterm-other-window)
  :config
  (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
  (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
  (define-key vterm-mode-map (kbd "M-y") 'vterm-yank-pop)
  (define-key vterm-mode-map (kbd "M-/") 'vterm-send-tab))

(setq
 browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 shr-indentation 2                           ; Left-side margin
 shr-width 72                                ; Fold text to 70 columns
 eww-search-prefix "https://google.com/?q=")

;; (setq eww-retrieve-command
;;       '("chromium" "--headless" "--dump-dom"))

(use-package w3m
  :commands (w3m w3m-browse-url)
  :config
  (setq w3m-quick-start nil)
  (setq w3m-display-mode 'plain)
  (setq w3m-use-cookies t)
  (setq w3m-use-toolbar nil)
  (setq w3m-use-tab-line nil)
  (setq w3m-use-tab-menubar nil))

(use-package xwwp
  :straight (xwwp :type git :host github :repo "canatella/xwwp")
  :commands (xwwp)
  :config
  (setq xwwp-follow-link-completion-system 'ivy))

(defun google-suggest ()
  "Search `w3m-search-default-engine' with google completion canditates."
  (interactive)
  (w3m-search w3m-search-default-engine
              (completing-read  "Google search: "
                                (dynamic-completion-table
                                 'google-suggest-aux))))

(defun google-suggest-aux (input)
  (with-temp-buffer
    (insert
     (shell-command-to-string
      (format "w3m -dump_source %s"
              (shell-quote-argument
               (format
                "http://www.google.com/complete/search?hl=en&js=true&qu=%s"
                input)))))
    (read
     (replace-regexp-in-string "," ""
                               (progn
                                 (goto-char (point-min))
                                 (re-search-forward "\(" (point-max) t 2)
                                 (backward-char 1)
                                 (forward-sexp)
                                 (buffer-substring-no-properties
                                  (1- (match-end 0)) (point)))))))

(use-package google-this
  :diminish 'google-this-mode
  :config (google-this-mode 1))

(global-set-key (kbd "C-c ;") #'comment-line)

(use-package xref
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (setq xref-search-program 'ripgrep))

(use-package dumb-jump
  :after xref
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; this has pre-defined actions and the way to define your own
(use-package avy
  :bind ("C-'" . #'avy-goto-char-timer)
        ("C-:" . #'avy-goto-char-2)
        ("M-g l" . #'avy-goto-line)
        ("M-g w" . #'avy-goto-word-1)
  :config
  (setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
                   ?a ?s ?d ?f ?g ?h ?j
                   ?k ?l ?' ?x ?c ?v ?b
                   ?n ?, ?/))
)

(with-eval-after-load 'avy
(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

(setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
      (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank-pop))
  t)

(setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
      (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
)

(use-package avy-menu
  :after avy)

(straight-use-package 'rg)
(straight-use-package 'fzf)
(straight-use-package 'ag)

;; frontend
(use-package ripgrep
  :commands ripgrep-regexp)

;; some fancy shit
(use-package deadgrep
  :commands deadgrep)

;; edit the buffer (can be toggled from *grep*
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (define-key grep-mode-map (kbd "w") 'wgrep-change-to-wgrep-mode)
  (setq wgrep-auto-save-buffer t))

;; rgrep replacement
(use-package urgrep
  :commands urgrep)

(setq completions-detailed t)
(setq completions-format 'one-column) ;; like ido
(setq completion-styles '(basic partial-completion emacs22 flex))

(unless (version< emacs-version "29.0")
  (setq completion-auto-help 'visible
        completion-auto-select 'second-tab
        completion-show-help t
        completions-sort nil ;; tricky crap
        completions-header-format nil))

;; It is OK, we can use both toolchains together
(use-package orderless
  :demand
  :config
  (add-to-list 'completion-styles 'orderless t) ;; this
  (setq orderless-component-separator "[ &]")
  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; extends built-in completions, a-la vertico
(use-package corfu
  :demand
  :init
  (setq-default tab-always-indent 'complete)
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-separator ?\s
        corfu-preview-current t
        corfu-quit-no-match t))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
    (comint-send-input))))

(advice-add #'corfu-insert :after #'corfu-send-shell)

(use-package projectile
  :demand
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-require-project-root t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (setq-default projectile-generic-command "rg --files --hidden -0")
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; a rip-off of Doom
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defvar +vertico-company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.")

(defvar +vertico/find-file-in--history nil)
;;;###autoload
(defun +vertico/find-file-in (&optional dir initial)
  "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (interactive)
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (prompt-dir (consult--directory-prompt "Find" default-directory))
         (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
    (find-file
     (consult--read
      (split-string (cdr (apply #'doom-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :initial (if initial (shell-quote-argument initial))
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input +vertico/find-file-in--history)))))

;; this extends built-in default completion system
(use-package vertico
  :demand
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                ;; vertico-mouse
                                ;; vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :hook (after-init . vertico-mode)
  :hook
  ((minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved for `vertico-repeat'
   (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
   )
  :init
  (defadvice! +vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
   (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args))))

;; this extends default completion lists
(use-package marginalia
  :demand
  :after vertico
  :hook (after-init . marginalia-mode)
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (advice-add #'marginalia--project-root :override #'projectile-project-root)
    (pushnew! marginalia-command-categories
            '(flycheck-error-list-set-filter . builtin)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))

(use-package consult
  :bind (
    ([remap bookmark-jump] .               #'consult-bookmark)
    ([remap goto-line] .                   #'consult-goto-line)
    ([remap imenu] .                       #'consult-imenu)
    ([remap Info-search] .                 #'consult-info)
    ([remap locate] .                      #'consult-locate)
    ([remap load-theme] .                  #'consult-theme)
    ([remap man] .                         #'consult-man)
    ([remap recentf-open-files] .          #'consult-recent-file)
    ([remap switch-to-buffer] .            #'consult-buffer)
    ([remap switch-to-buffer-other-window] . #'consult-buffer-other-window)
    ([remap switch-to-buffer-other-frame] . #'consult-buffer-other-frame)
    ([remap dired]                      .  #'consult-dir)
    ([remap yank-pop] .                    #'consult-yank-pop))
  :config
  (setq consult-project-root-function #'projectile-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize
     consult-ripgrep consult-git-grep consult-grep))

;; C-x C-f
(use-package consult-dir
  :demand
  :after consult
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package consult-flycheck
  :demand
  :after (consult flycheck))

(use-package consult-lsp
  :demand
  :after (consult lsp)
    :init
  (map! :map lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; (use-package consult-todo :after consult)

(use-package all-the-icons-completion
  :demand
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package ibuffer-projectile
  :after projectile)

;; will be loaded by ivy
(straight-use-package 'flx)

(setq isearch-lazy-count t)
(setq isearch-yank-on-move t)

;; loat on-demand
(use-package ivy
  :diminish
  ;; :hook (after-init . ivy-mode) ;; another kludge
  :bind
   (:map ivy-minibuffer-map
         ([remap doom/delete-backward-word] . #'ivy-backward-kill-word)
         ("C-o" . #'ivy-dispatching-done))
  :init
  (let ((standard-search-fn
         ;; #'+ivy-prescient-non-fuzzy)
         #'ivy--regex-plus)
        (alt-search-fn
         #'ivy--regex-fuzzy))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn)))
    (setq ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
  :config
  (require 'counsel nil t) ;; a kludge
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t)
  (setq ivy-display-style 'fancy)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (ivy-set-occur 'counsel-fzf 'counsel-fzf-occur)
  (ivy-set-occur 'counsel-rg 'counsel-ag-occur)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  (ivy-set-occur 'swiper-multi 'counsel-ag-occur))
;;  (ivy-mode t))

;; this is such a cool embedded DLS
(use-package counsel
  :diminish
;;  :after ivy
  :bind
  (([remap apropos]                .  #'counsel-apropos)
   ([remap bookmark-jump]          .  #'counsel-bookmark)
   ([remap compile]                .  #'+ivy/compile)
   ([remap describe-bindings]      .  #'counsel-descbinds)
   ([remap describe-face]          .  #'counsel-faces)
   ([remap describe-function]      .  #'counsel-describe-function)
   ([remap describe-variable]      .  #'counsel-describe-variable)
   ([remap describe-symbol]        .  #'counsel-describe-symbol)
;;   ([remap execute-extended-command] .  #'counsel-M-x)
;;   ([remap find-file]              .  #'counsel-find-file)
   ([remap find-library]           .  #'counsel-find-library)
   ([remap imenu]                  .  #'counsel-imenu)
   ([remap info-lookup-symbol]     .  #'counsel-info-lookup-symbol)
   ([remap load-theme]             .  #'counsel-load-theme)
   ([remap locate]                 .  #'counsel-locate)
   ([remap org-goto]               .  #'counsel-org-goto)
   ([remap org-set-tags-command]   .  #'counsel-org-tag)
   ([remap projectile-compile-project] . #'+ivy/project-compile)
   ;; ([remap recentf-open-files]     .  #'counsel-recentf)
   ([remap set-variable]           .  #'counsel-set-variable)
   ([remap swiper]                 .  #'counsel-grep-or-swiper)
   ([remap insert-char]            .  #'counsel-unicode-char)
   ;; ([remap yank-pop]               .  #'counsel-yank-pop)
   ;; ([remap dired]                  .  #'counsel-dired)
   ("C-x B" . counsel-switch-buffer-other-window)
   ("M-s r" . counsel-rg)
   ("C-c r" . counsel-rg)
   ("C-c z" . counsel-fzf)
   ("M-s z" . counsel-fzf)
   ("C-c g" . counsel-git)
   ("C-c a" . counsel-ag)
   ("C-c j" . #'counsel-git-grep)
   :map ivy-minibuffer-map ("C-r" . counsel-minibuffer-history))
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        counsel-descbinds-function #'helpful-callable)
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (add-hook 'counsel-grep-post-action-hook #'recenter)
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)
  (add-to-list 'savehist-additional-variables 'counsel-compile-history)
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file)))
;;  (counsel-mode t))

(use-package ivy-rich
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-project-root-cache-mode +1)
  (ivy-rich-mode t))

(use-package all-the-icons-ivy
  :demand
  :after ivy
  :config
  ;; (setq all-the-icons-ivy-buffer-commands nil)
  (all-the-icons-ivy-setup)
	(all-the-icons-ivy-rich-mode))

(use-package all-the-icons-ivy-rich
  :demand
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package prescient
  :config (prescient-persist-mode +1))

;; lots of Doom Emacs hacks. a nasty bug with :init instead of :config
;; TODO: try to remove prescient and use flx only
(use-package ivy-prescient
  :commands +ivy-prescient-non-fuzzy
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :config
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))
  (setq prescient-filter-method
        '(literal regexp initialism fuzzy))
  (add-to-list 'ivy-sort-functions-alist '(ivy-resume))
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer lsp-ivy-workspace-symbol
               ivy-resume ivy--restore-session counsel-grep counsel-git-grep
               counsel-rg counsel-ag counsel-ack counsel-fzf counsel-pt counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf
               counsel-outline counsel-org-goto counsel-jq)
        ivy-prescient-retain-classic-highlighting t))

;; (use-package doom-todo-ivy
;;   :straight (:host github :repo "jsmestad/doom-todo-ivy")
;;   :hook (after-init . doom-todo-ivy))

(use-package counsel-projectile
  :bind
  (([remap projectile-find-file] . #'+ivy/projectile-find-file)
   ([remap projectile-find-dir]  . #'counsel-projectile-find-dir)
   ([remap projectile-switch-to-buffer] . #'counsel-projectile-switch-to-buffer)
   ([remap projectile-grep] .  #'counsel-projectile-grep)
   ([remap projectile-ag] . #'counsel-projectile-ag)
   ([remap projectile-switch-project] . #'counsel-projectile-switch-project))
  :config
  (setf (alist-get 'projectile-find-file counsel-projectile-key-bindings)
        #'+ivy/projectile-find-file)
  (setq counsel-projectile-sort-files t))

(use-package counsel-web
  :after counsel
  :config
  (setq counsel-web-search-action #'eww-browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-alternate-action #'w3m)
  (defvar counsel-web-map
    (let ((map (make-sparse-keymap "counsel-web")))
      (define-key map (kbd "w") #'counsel-web-suggest)
      (define-key map (kbd "s") #'counsel-web-search)
      (define-key map (kbd ".") #'counsel-web-thing-at-point)
      map))
  (global-set-key (kbd "C-c w") counsel-web-map))

(use-package counsel-gtags
  :demand
  :after counsel)

(use-package counsel-etags
  :demand
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(use-package counsel-test
  :after counsel)

(use-package counsel-tramp
  :config
  (setq tramp-default-method "ssh")
  (define-key global-map (kbd "C-c s") 'counsel-tramp)
  (add-hook 'counsel-tramp-pre-command-hook
            '(lambda () (global-aggressive-indent-mode -1)
               (projectile-mode -1)
               (editorconfig-mode -1)))
  (add-hook 'counsel-tramp-quit-hook
            '(lambda () (global-aggressive-indent-mode 1)
               (projectile-mode 1)
               (editorconfig-mode 1))))

;; (use-package ivy-posframe
;;   :straight t
;;   :hook (ivy-mode . ivy-posframe-mode)
;;   :config
;;   (setq ivy-fixed-height-minibuffer nil
;;         ivy-posframe-border-width 10
;;         ivy-posframe-parameters
;;         `((min-width . 90)
;;           (min-height . ,ivy-height))))

(use-package ivy-pass
  :demand
  :after ivy)

(use-package ivy-avy
  :demand
  :after ivy)

(use-package ivy-xref
  :demand
  :after ivy
  :init
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  ;; Use ivy-xref to display `xref.el' results.
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; less bloated isearch
(use-package pcre2el)

(use-package visual-regexp)
(use-package visual-regexp-steroids
  :custom
  (vr/engine 'pcre2el "Use PCRE regular expressions")
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  ("C-M-r" . vr/isearch-backward)
  ("C-M-s" . vr/isearch-forward))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("M-s s" . swiper)
         ("M-s m" . swiper-multi)
         ("M-s w" . swiper-thing-at-point))
  :config
  (setq swiper-action-recenter t))

(use-package helpful
  :after counsel
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  :config
  (defalias 'describe-function 'helpful-callable)
  (defalias 'describe-command 'helpful-command)
  (defalias 'describe-variable 'helpful-variable)
  (defalias 'describe-key 'helpful-key)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (setq counsel-descbinds-function #'helpful-callable)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package ace-link
  :config (ace-link-setup-default))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (rainbow-delimiters-mode t))

(use-package smartparens
  :demand
  :diminish
  :hook (after-init . smartparens-global-strict-mode)
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :hook (prog-mode . turn-on-show-smartparens-mode)
  :config
  (require 'smartparens-config))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(use-package eldoc
  :diminish
  :after company
  :hook (after-init . global-eldoc-mode)
  :hook (prog-mode . eldoc-mode)
  :config
  (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort)
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package company
  :demand
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (after-init . global-company-mode)
  :hook (prog-mode . company-mode)
  :bind ("M-/" . #'company-complete-common-or-cycle)
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  (setq company-minimum-prefix-length 2
        company-show-numbers nil
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never)
  (setq company-global-modes
        '(not message-mode
              Man-mode
              help-mode
              Info-mode
              gud-mode
              vterm-mode))
  (setq company-frontends
        '(company-pseudo-tooltip-frontend  ; always show        candidates in overlay tooltip
          company-echo-metadata-frontend))  ; show selected )
  (setq company-backends
        '((company-keywords
           company-capf
           company-gtags
           company-etags
           company-semantic
           company-files)
          (company-abbrev company-dabbrev company-dabbrev-code)
          )))

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

;; prescient will be loaded on demand
(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :hook (company-prescient-mode . prescient-persist-mode))

;; good for ~company~ and ~which-key~. shitty for ~ivy~
(use-package company-posframe
  :diminish
  :hook (company-mode . company-posframe-mode)
  :config
  (setq company-tooltip-minimum-width 40))

(use-package company-quickhelp
  :demand
  :after company
  :custom
  (company-quickhelp-delay 3)
  :hook (company-mode . company-quickhelp-mode))

(use-package company-math
  :demand
  :after company
  :config
  (setq company-math-disallow-unicode-symbols-in-faces t)
  (add-to-list 'company-backends 'company-math-symbols-latex t)
  (add-to-list 'company-backends 'company-math-symbols-unicode t))

(use-package company-org-block
  :after company
  :demand
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook (org-mode . (lambda ()
                      (add-to-list 'company-backends 'company-org-block)
                      (company-mode +1))))

(use-package company-statistics
  :after company
  :config (company-statistics-mode))

(use-package company-web
  :after company
  :config
  :hook (nxml-mode . (lambda ()
                       (add-to-list 'company-backends 'company-web-html t))))

(use-package flycheck
  :demand
  :diminish
  :hook (after-init . global-flycheck-mode)
  :config (global-flycheck-mode t))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :init
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  (setq flycheck-check-syntax-automatically '(save
                                            idle-change
                                            mode-enabled)))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package avy-flycheck
  :after flycheck
  :config (avy-flycheck-setup))

(use-package yasnippet
  :demand
  :diminish
  :after company
  :hook (adfter-init . (lambda ()
                         (interactive)
                         (yas-global-mode t)
                         (yas-reload-all)))
  :config
  (cl-pushnew 'company-yasnippet company-backends)
  (yas-global-mode t))

(use-package yasnippet-snippets
  :demand
  :after yasnippet)

(use-package doom-snippets
  :demand
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(use-package :aas
  :straight '(:host github :repo "ymarco/auto-activating-snippets")
  :commands aas-mode)

(use-package ivy-yasnippet
  :demand
  :after (ivy yasnippet))

;; TODO: convert to :bind
(use-package auto-yasnippet
  :after yasnippet
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
  :diminish
  :hook (prog-mode . lsp)
  :hook (lsp-completion-mode . (lambda ()
                                 (remq 'company-capf company-backends)))
  :init
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-auto-configure t))

(use-package lsp-org
  :after org
  :straight '(:type built-in))

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))
))
    `(progn
       (defun ,intern-pre (info)
         (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                    "org-src-babel.tmp"))
         (setq-local lsp-headerline-breadcrumb-enable nil)
         (lsp-deferred))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      'lsp-mode (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

(defconst org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "c" "rust" "ocaml" "haskell" "cpp" "c++"))
(add-to-list 'org-babel-lang-list "shell")
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(use-package company-lsp
  :demand
  :straight '(company-lsp :type git :host github :repo "tigersoldier/company-lsp")
  :after company
  :config
  (cl-pushnew 'company-lsp company-backends))

(use-package lsp-ui
  :hook (prog-mode . lsp-ui-mode)
  :init
  (setq lsp-auto-configure t)
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-lens-enable t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-fontify 'always)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  (setq lsp-ui-doc-show-with-cursor t))

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)

(use-package devdocs
  :after lsp
  :config
  :hook (devdocs-mode . (lambda ()
    (face-remap-add-relative 'variable-pitch '(:family "SF Pro Text")))))

(use-package outline-mode
 :straight '(:type built-in))

(use-package outshine
 :hook (outline-minor-mode . outshine-mode))

;; text-mode-hook
(add-hook 'text-mode-hook
          (lambda ()
            (interactive)
            ;; (setq-local show-trailing-whitespace -1)
            ;; (setq-local indicate-empty-lines -1)
            (word-wrap-whitespace-mode t)
            (set-fill-column 72)
            (auto-fill-mode t)
            (visual-line-mode t)
            ))

;; will be overwritten by org-modern
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                                 ("#+END_SRC" . "«")
                                                 ("#+begin_src" . "»")
                                                 ("#+end_src" . "«")
                                                 ("lambda"  . "λ")
                                                 ("->" . "→")
                                                 ("->>" . "↠")))
            (setq-local prettify-symbols-unprettify-at-point 'right-edge)
            (prettify-symbols-mode t)))

;; comint-mode-hook
(add-hook 'comint-mode-hook
          (lambda ()
            (interactive)
            (setq-local show-trailing-whitespace nil)
            (setq-local indicate-empty-lines nil)
            ;; (whitespace-mode t)
            (setq-local indent-tabs-mode nil)
            (setq-local corfu-auto nil)
            (setq-local comint-prompt-read-only t)
            (add-hook 'completion-at-point-functions
                      'pcomplete-completions-at-point)
            (cl-pushnew 'company-capf company-backends)))

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; prog-mode-hook
(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (setq-local show-trailing-whitespace t)
            (setq-local indicate-empty-lines t)
            (whitespace-mode t)
            (set-fill-column 72)
            (auto-fill-mode t)
            (electric-pair-mode -1) ;; clashes with smartparens
            (electric-indent-mode t)
            (abbrev-mode t)
            (outline-minor-mode t)))

;; maintains a tag database
;; parses and completes
;; powerful but outdate, prefer modern /clangd/ and LSP based solutions
(add-hook 'semantic-mode-hook
          '(lambda ()
             (interactive)
             (require 'semantic/ia)
             (require 'semantic/bovine/c)
             (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
             (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                          "/usr/lib/gcc/x86_64-linux-gnu/13/include/stddef.h")
             (semanticdb-enable-gnu-global-databases 'c-mode)
             (semanticdb-enable-gnu-global-databases 'c++-mode)
             (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
             (local-set-key "\C-c\C-s" 'semantic-ia-show-summary)
             ;(push 'company-semantic company-backends)
             ))

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style)
  :hook (c-mode-common . google-make-newline-indent))

(use-package clang-format
  :commands clang-format)

(use-package disaster
  :commands (disaster))

(use-package rmsbolt
  :commands (rmsbolt))

(use-package rainbow-mode
  :config (rainbow-mode t))

(use-package electric-spacing
  :hook (prog-mode . electric-spacing-mode))

(use-package aggressive-indent
  :diminish
  :config (global-aggressive-indent-mode t))

(use-package hl-todo
  :hook ((org-mode prog-mode) . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package diff-hl
  :after magit
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-flydiff-delay 0.5)  ; default: 0.3
  (setq diff-hl-show-staged-changes nil)
  (global-diff-hl-mode t))


(use-package selected
  :ensure t
  :commands selected-minor-mode
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)
              :map selected-org-mode-map
              ("t" . org-table-convert-region)))

(use-package expand-region
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(use-package ggtags
  :diminish 'ggtags-mode
  :hook (prog-mode . ggtags-mode)
  :config (cl-pushnew 'company-gtags company-backends))

(use-package counsel-gtags
  :init
  (setq counsel-gtags-ignore-case t
        counsel-gtags-auto-update t)
  :hook (ggtags-mode . counsel-gtags-mode))

(use-package volatile-highlights
  :diminish 'volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode))

(use-package highlight-indent-guides
  :diminish 'highlight-indent-guides-mode
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :hook (after-init . highlight-indent-guides-auto-set-faces)
  :init (setq highlight-indent-guides-method 'character))

(use-package auto-highlight-symbol
  :diminish 'auto-highlight-symbol-mode
  :commands (ahs-highlight-p)
  :hook (prog-mode . auto-highlight-symbol-mode)
  :config
  (diminish auto-highlight-symbol-mode)
  (setq ahs-case-fold-search nil
        ahs-default-range 'ahs-range-whole-buffer
        ahs-idle-interval 3.75))

(use-package symbols-outline
  :commands symbols-outline-follow-mode
  :config
  (setq symbols-outline-window-position 'left)
  (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; (add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))
(use-package f90-mode
  :straight '(:type built-in)
  :hook (f90-mode . lsp-deferred)
  :mode "\\.f\\'")

(use-package f90-interface-browser
  :after f90-mode)

(use-package fortran-tags
 :straight '(:type built-in)
   :after f90-mode)

;; (autoload 'octave-mode "octave-mod" nil t)
(use-package octave-mode
  :straight '(:type built-in)
  :mode ("\\.m\\'" . octave-mode)
  :hook (octave-mode . (lambda ()
                         (setq-local indent-tabs-mode nil)
                         ;; (setq-local corfu-auto nil)
                         (setq-local comint-prompt-read-only t)
                         (setq-local comint-prompt-regexp (rx bol ">>" space))))
  :commands run-octave)

;; better and proper MATLAB
(use-package julia-mode
  :interpreter "julia"
  :mode "\\.jl\\'")

(use-package lsp-julia
  :hook (julia-mode . lsp-deferred)
  :init
  (setq lsp-julia-default-environment "~/.julia/environments/v1.9"))

(use-package eglot-jl
  :after eglot
  :init
  (setq eglot-jl-language-server-project "~/.julia/environments/v1.9")
  :config (eglot-jl-init))

(use-package julia-vterm)
(use-package ob-julia-vterm)

(use-package julia-repl
  :hook (julia-mode . julia-repl-mode)
  :hook (julia-repl-mode . julia-repl-use-emacsclient)
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode)
  :config
  (setq julia-snail-popup-display-eval-results :command)
  (setq julia-snail-multimedia-enable t))

(use-package emacs-lisp-mode
  :straight '(:type built-in)
  :hook (emacs-lisp-mode . (lambda ()
                             (interactive)
                             (electric-pair-mode -1)
                             (electric-spacing-mode -1)
                             (auto-compile-mode t)
                             (semantic-mode t)))
  :config
  (with-eval-after-load 'semantic
    (semantic-default-emacs-lisp-setup)))

(use-package xscheme
  :hook (scheme-mode . (lambda ()
                             (interactive)
                             (electric-pair-mode -1)
                             (electric-spacing-mode -1)))
  :init
  (setq scheme-program-name "mit-scheme")
  (setq inferior-scheme-program "mit-scheme"))

(straight-use-package 'hippie-expand-slime)

;; comint, etc.
(use-package slime
  ;; :hook (lisp-mode . slime-mode) ;; slows down emacs-lisp
  :hook (lisp-mode-local-vars . slime-editing-mode)
  :hook (lisp-mode . (lambda ()
                             (interactive)
                             (electric-pair-mode -1)
                             (electric-spacing-mode -1)))
  :hook (inferior-lisp-mode . inferior-slime-mode)
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-lisp-implementations
          '((sbcl ("/usr/bin/sbcl"))))
  (setq slime-auto-start 'always)
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (slime-setup '(
                 slime-asdf
                 slime-autodoc
                 slime-editing-commands
                 slime-fancy
                 slime-fancy-inspector
                 slime-fontifying-fu
                 slime-fuzzy
                 slime-company
                 slime-indentation
                 slime-mdot-fu
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-sbcl-exts
                 slime-scratch
                 slime-xref-browser
                   ))
    (slime-autodoc-mode)
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function
          'slime-fuzzy-complete-symbol)
    (slime-auto-start))

(use-package slime-repl-ansi-color
  :init
  (add-to-list 'slime-contribs 'slime-repl-ansi-color))

(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

;; (use-package sly
;;   :hook (lisp-mode-local-vars . sly-editing-mode)
;; )
;; (straight-use-package 'sly-repl-ansi-color)
;; (straight-use-package 'sly-macrostep)

(use-package sml-mode
          :mode "\\.s\\(?:ml\\|ig\\)\\'")

(use-package company-mlton
      :straight '(company-mlton :type git :host github :repo "MatthewFluet/company-mlton")
      :after company
      :hook (sml-mode . company-mlton-init)
      :config
      (add-to-list company-backends 'company-mlton-grouped-backend))

(use-package lua-mode
  :mode "\\.lua?\\'"
  :hook (lua-mode . lsp-deferred)
  :hook (lua-mode . (lambda ()
                      (setq-local tab-width 2
                                  indent-tabs-mode t)))
  :init
  (setq lsp-clients-lua-language-server-install-dir "/opt/lua-language-server")
  (setq lua-default-application "luajit"))

(defmacro file! ()
  "Return the file of the file this macro was called."
  (or
   ;; REVIEW: Use `macroexp-file-name' once 27 support is dropped.
   (let ((file (car (last current-load-list))))
     (if (stringp file) file))
   (bound-and-true-p byte-compile-current-file)
   load-file-name
   buffer-file-name   ; for `eval'
   (error "file!: cannot deduce the current file path")))

(defmacro dir! ()
  "Return the directory of the file this macro was called."
   (let (file-name-handler-alist)
     (file-name-directory (macroexpand '(file!)))))

(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is used."
  `(let ((default-directory (dir!))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'string=))))

(add-load-path! (car (file-expand-wildcards "/usr/lib64/erlang/lib/tools-*/emacs")))

;; just right mode -- comint, etags, electric modes, flymake
(use-package erlang
  :straight '(:type built-in) ;; DO NOT clone whole otp
  :load-path (lambda () (car (file-expand-wildcards "/usr/lib64/erlang/lib/tools-*/emacs")))
  :hook (erlang-mode . flymake-mode)
  :hook (erlang-mode . flycheck-mode)
  :hook (erlang-mode . lsp)
  :config
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; default node name to emacs@localhost
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (setq erlang-root-dir "/usr/lib64/erlang")
  (setq lsp-ui-doc-enable t)
  (require 'erlang-start))

;; outdated solution, use the 'lsp-mode
(use-package distel
  :after erlang
  :straight '(:host github :repo "/massemanet/distel")
  :load-path (lambda () (car (file-expand-wildcards "/usr/lib64/erlang/lib/tools-*/emacs")))
  :init
  (require 'erlang-start)
  :config
  (add-load-path! (car (file-expand-wildcards "/usr/lib64/erlang/lib/tools-*/emacs")))
  (distel-setup)
  (require 'company-distel)
  (require 'company-distel-frontend))

(use-package company-distel
 :straight '(:host github :repo "sebastiw/distel-completion" :files ("company-distel.el"))
 :hook (erlang-mode .
                    (lambda ()
                      (push 'company-distel company-backends))))

(use-package haskell-mode
    :custom
    (haskell-stylish-on-save t)
    :hook (haskell-mode . (lambda ()
                            (interactive)
                            (turn-on-haskell-doc-mode)
                            (turn-on-haskell-indentation))))

  (use-package flycheck-haskell
    :after flycheck)

  (use-package company-ghci
    :after company
    :config
    (add-to-list 'company-backends 'company-ghci))

  (use-package lsp-haskell
    :after lsp
    :hook ((haskell-mode . lsp)
           (literate-haskell-mode . lsp))
    :init
    (setq lsp-haskell-server-path "haskell-language-server-wrapper")
    (setq lsp-haskell-server-args ()))

(use-package merlin
  :after company
  :config
  (add-to-list 'company-backends 'merlin-company-backend)
  (setq merlin-completion-with-doc t))

(use-package tuareg
  :hook (tuareg-mode-local-vars . merlin-mode)
  :hook (tuareg-mode-local-vars . lsp)
  :hook (tuareg-mode-local-vars . tree-sitter-mode)
  :config
  (setq tuareg-prettify-symbols-full t)
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package flycheck-ocaml
  :after merlin
  :hook (tuareg-mode . flycheck-ocaml-setup))

(use-package merlin-eldoc
  :after merlin
  :hook (merlin-mode . merlin-eldoc-setup))

(use-package merlin-imenu
  :straight (:type built-in)
  :after merlin
  :hook (merlin-mode . merlin-use-merlin-imenu))

(use-package ocamlformat
  :commands ocamlformat)

(use-package ocp-indent
  :hook (tuareg-mode . ocp-setup-indent))

(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

(use-package merlin-iedit
  :after iedit)

(use-package utop
    :hook (tuareg-mode . utop-minor-mode))

(use-package lsp-metals
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :hook (scala-mode . lsp))

(use-package scala-mode
  :custom
    (flycheck-scala-executable "scalac --color never")
  :interpreter
    ("scala3 --color never" . scala-mode)
  :config
    (setq prettify-symbols-alist scala-prettify-symbols-alist))

(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/v1/"))

(use-package function-args
  :hook (c-common . function-args-mode)
  :config
  (set-default 'semantic-case-fold -1)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (fa-config-default))

;; yet another cool tags solution
(use-package rtags
  :defer t
  :config
  (setq rtags-autostart-diagnostics t
        rtags-use-bookmarks nil
        rtags-completions-enabled t
        rtags-display-result-backend 'ivy
        rtags-results-buffer-other-window t
        rtags-jump-to-first-match nil)
  (progn
    (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
    (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

    (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
    (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
    (rtags-enable-standard-keybindings)

    ;; Shutdown rdm when leaving emacs.
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
    ))

(use-package ivy-rtags
  :after rtagd)

(use-package flycheck-rtags
  :after rtags
  :defer t
  :config
  (progn
    ;; ensure that we use only rtags checking
    ;; https://github.com/Andersbakken/rtags#optional-1
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
      )
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
    ))

;; just use LSP and clang-format tools
(use-package cc-mode
  :hook (c-mode-common . (lambda ()
                           (interactive)
                           (setq-local c-basic-offset 4
                                       tab-width 4
                                       indent-tabs-mode t)
                           (setq c-syntactic-indentation t)
                           (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
                           (push 'company-clang comnany-backends)
                           (semantic-mode t)
                           ))
  :hook (c-mode-common . tree-sitter-mode)
  :hook (c++-mode . (lambda ()
                      (interactive)
                      (setq flycheck-clang-language-standard "c++20")))
  :config
  (with-eval-after-load 'semantic
    (semantic-default-c-setup)))

(use-package cmake-mode
   :config (push 'company-cmake company-backends))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package demangle-mode
  :hook (c++mode . demandgle-mode))

(use-package gdb-mi
  :init
  (setq gdb-many-windows t
        gdb-show-main t))

(use-package realgud
  :commands realgud:gdb)

(use-package ccls
  :hook (lsp-configure . (lambda ()
    (setq ccls-sem-highlight-method                      ccls-sem-highlight-method)))
  :init
  (defvar ccls-sem-highlight-method 'font-lock)
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories "^.ccls-cache$")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
  )

;; cool but outdated
;; relies on rtags
(use-package cmake-ide
  :config
  (setq cmake-ide-flags-c++ (append '("-std=c++20")))
  (cmake-ide-setup))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package elisp-def
  :diminish
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package macrostep
  :mode (("\\*.el\\'" . emacs-lisp-mode)
         ("Cask\\'" . emacs-lisp-mode)))

(use-package elisp-slime-nav
  :diminish
  :hook (emacs-lisp-mode  . elisp-slime-nav-mode))

(use-package eval-sexp-fu
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

(use-package ielm
  :straight '(:type built-in)
  :hook (ielm-mode . smartparens-strict-mode)
  :hook (ielm-mode . rainbow-delimiters-mode)
  :hook (ielm-mode . eldoc-mode)
  :hook (ielm-mode . highlight-quoted-mode)
  :hook (ielm-mode . highlight-numbers-mode)
  :init
  (setq-local font-lock-keywords `(,@lisp-el-font-lock-keywords-2
                                 ,@lisp-cl-font-lock-keywords-2)))

(use-package tramp
  :config
  (setq vc-handled-backends '(Git)
        file-name-inhibit-locks t
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package dired-async
  :straight '(:type built-in)
  :config
  :hook (dired-mode-load . dired-async-mode))

;; https://github.com/Fuco1/dired-hacks
(use-package dired
  :straight '(:type built-in)
  :hook (dired-mode . dired-hide-details-mode)
  :bind ("C-x C-k" . 'dired-do-delete)
  :config
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask))

(use-package dired-aux
  :straight '(:type built-in)
  :config
  (require 'dired-async)
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

(use-package dired-x
  :straight '(:type built-in)
  :hook (dired-mode . dired-omit-mode)
  :commands (dired-jump
             dired-jump-other-window
             dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"
                "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__"))
  )

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-gitignore
  :straight '(:type git :host github :repo "johannes-mueller/dired-gitignore.el")
  :hook (dired-mode . dired-gitignore-mode))

(use-package fd-dired
  :init
  (global-set-key [remap find-dired] #'fd-dired))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-git-info
  :hook (dired-mode . (lambda ()
                        (dired-git-info-mode t))))

(use-package all-the-icons-dired
  :demand
  :diminish t
  :if (display-graphic-p)
  :hook (dired-mode . (lambda () (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode))))
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package magit
  :demand
  :hook (magit-post-refresh  . diff-hl-magit-post-refresh)
  :init
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq git-commit-fill-column 72)
  (setq magit-section-initial-visibility-alist
      '((untracked . show)
        (unstaged . show)
        (unpushed . show)
        (unpulled . show)
        (stashes . show)))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq transient-default-level 5)
  (setq magit-save-repository-buffers nil)
  (setq magit-revision-insert-related-refs nil)
  (setq magit-diff-refine-hunk t)
  (setq magit-push-always-verify nil)
  (setq magit-revert-buffers 'silent)
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes)))

(use-package git-modes
  :after magit)

(use-package ghub
  :after magit)

(use-package forge
  :after magit)

(use-package orgit
  :after org)

(use-package orgit-forge
  :after forge)

;; the future

(defun auto-configure-treesitter ()
  "Find and configure installed grammars, remap to matching -ts-modes if present.
Return a list of languages seen along the way."
  (let ((grammar-name-to-emacs-lang '(("emacs-lisp". "elisp")
                                      ("common-lisp" . "commonlisp")
                                      ("c-sharp" . "csharp")
                                      ("cpp" . "c++")
                                      ("gomod" . "go-mod")
                                      ("javascript" . "js")))
        seen-grammars)
    (dolist (dir (cons (expand-file-name "tree-sitter" user-emacs-directory)
                       treesit-extra-load-path))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir))
          (let ((fname (file-name-sans-extension (file-name-nondirectory file))))
            (when (string-match "libtree-sitter-\\(.*\\)" fname)
              (let* ((file-lang (match-string 1 fname))
                     (emacs-lang (or (cdr (assoc-string file-lang grammar-name-to-emacs-lang)) file-lang)))
                ;; Override library if its filename doesn't match the Emacs name
                (unless (or (memq (intern emacs-lang) seen-grammars)
                            (string-equal file-lang emacs-lang))
                  (let ((libname (concat "tree_sitter_" (replace-regexp-in-string "-" "_" file-lang))))
                    (add-to-list 'treesit-load-name-override-list
                                 (list (intern emacs-lang) fname libname))))
                ;; If there's a corresponding -ts mode, remap the standard mode to it
                (let ((ts-mode-name (intern (concat emacs-lang "-ts-mode")))
                      (regular-mode-name (intern (concat emacs-lang "-mode"))))
                  (when (fboundp ts-mode-name)
                    (add-to-list 'major-mode-remap-alist
                                 (cons regular-mode-name ts-mode-name))))
                ;; Remember we saw this language so we don't squash its config when we
                ;; find another lib later in the treesit load path
                (push (intern emacs-lang) seen-grammars)))))))
    seen-grammars))

(auto-configure-treesitter)

(use-package treesit-auto
  :demand
  :config
  (global-treesit-auto-mode))

(use-package treesit
  :straight '(:type built-in)
  :init
  (setq-default treesit-font-lock-level 4)
  (when (boundp 'treesit-extra-load-path)
    (add-to-list 'treesit-extra-load-path "/usr/lib64/")
    (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
    (add-to-list 'treesit-extra-load-path "~/.local/lib/"))
  :config
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (json-mode . json-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(use-package combobulate
  :straight '(:host github :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c o")

  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;; the obsolete way
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'tree-sitter-indent)

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :hook (tree-sitter-after-on . tree-sitter-indent-mode)
  :config
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t)
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(lisp-interaction-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(python-mode . python))
  (global-tree-sitter-mode t))

;;; When /not in a rush/, this is a /principle-guided/ way.

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

;;;###autoload
(defun +eshell/quit-or-delete-char (arg)
  "Delete a character (ahead of the cursor) or quit eshell if there's nothing to
delete."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (eshell-life-is-too-much)
    (delete-char arg)))

;;;###autoload
(defun +eshell/search-history ()
  "Search the eshell command history with helm, ivy or `eshell-list-history'."
  (interactive)
  (require 'em-hist)
  (let* ((ivy-completion-beg (eshell-bol))
         (ivy-completion-end (point-at-eol))
         (input (buffer-substring-no-properties
                 ivy-completion-beg
                 ivy-completion-end)))
    ;; Better than `counsel-esh-history' because that doesn't
    ;; pre-populate the initial input or selection.
    (ivy-read "Command: "
              (delete-dups
               (when (> (ring-size eshell-history-ring) 0)
                 (ring-elements eshell-history-ring)))
              :initial-input input
              :action #'ivy-completion-in-region-action)))

(defun +eshell/clear-prompt ()
  (interactive)
  (eshell/clear-scrollback)
  (eshell-emit-prompt))

(use-package esh-mode
  :straight '(:type built-in)
  :bind (:map eshell-mode-map
              ("C-a" .  #'eshell-bol)
              ("C-e" .  #'end-of-line)
              ("C-l" .  #'+eshell/clear-prompt)
              ;; ("C-l" .  (cmd! (eshell/clear-scrollback) (eshell-emit-prompt)))
              ("C-r"  . #'counsel-esh-history)
              ("M-s"  . #'+eshell/search-history)
              ("C-d"  . #'+eshell/quit-or-delete-char))
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (add-to-list 'eshell-modules-list 'eshell-smart))

(use-package eshell
  :commands eshell
  :straight '(:type built-in)
  :hook (eshell-mode . (lambda ()
                         (interactive)
                         (semantic-mode -1)
                         (hide-mode-line-mode t)
                         (rainbow-delimiters-mode t)
                         (highlight-numbers-mode t)
                         (highlight-quoted-mode t)
                         (smartparens-strict-mode t)
                         (visual-line-mode +1)
                         (setq hscroll-margin 0)
                         (set-display-table-slot standard-display-table
                                                 0 ?\ )))
  :config
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-destroy-buffer-when-process-dies t)
  (setq pcomplete-cycle-completions nil)
  (with-eval-after-load 'em-cmpl
    (setq eshell-cmpl-cycle-completions nil))
  (with-eval-after-load 'em-hist
    (setq eshell-hist-ignoredups t)
    (setq eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input))))
    )
  (with-eval-after-load 'em-term
    (append '("htop" "vim" "nvim" "ncmpcpp") eshell-visual-commands))
  (with-eval-after-load 'em-alias
    (setq eshell-command-aliases-list '(("q"  "exit")
                                        ("f"  "find-file $1")
                                        ("ff" "find-file-other-window $1")
                                        ("d"  "dired $1")
                                        ("bd" "eshell-up $1")
                                        ("rg" "rg --color=always $*")
                                        ("l"  "ls -lh $*")
                                        ("ll" "ls -lah $*")
                                        ("git" "git --no-pager $*")
                                        ("gg" "magit-status")
                                        ("cdp" "cd-to-project")
                                        ("clear" "clear-scrollback"))))
  (with-eval-after-load 'em-glob
    (setq  eshell-glob-case-insensitive t
           eshell-error-if-no-glob t))
  (with-eval-after-load 'em-smart
    (setq eshell-where-to-jump 'begin
          eshell-review-quick-commands nil
          eshell-smart-space-goes-to-end t)
    (add-hook 'eshell-mode-hook #'eshell-smart-initialize))
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply))

(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'vterm)

(use-package eshell-up
  :commands eshell-up eshell-up-peek)

(use-package esh-help
  :after eshell
  :config (setup-esh-help-eldoc))

(straight-use-package 'shrink-path)

(use-package eshell-did-you-mean
  :after esh-mode
  :config (eshell-did-you-mean-setup)
  (setq eshell-last-command-name "catt")
  (eshell-did-you-mean-output-filter "catt: command not found"))

(use-package eshell-syntax-highlighting
  :hook (esh-mode . eshell-syntax-highlighting-mode))

(straight-use-package 'shell-pop)

;;; Some hacks to make ~company-mode~ work.

(defun toggle-shell-auto-completion-based-on-path ()
  "Deactivates automatic completion on remote paths.
  Retrieving completions for Eshell blocks Emacs. Over remote
  connections the delay is often annoying, so it's better to let
  the user activate the completion manually."
  (if (file-remote-p default-directory)
      (setq-local company-idle-delay nil)
    (setq-local company-idle-delay 0.6)))

(defun eshell-switch-company-frontend ()
  "Sets the company frontend to `company-preview-frontend' in e-shell mode."
  (require 'company)
  (setq-local company-backends '(company-capf))
  (setq-local company-frontends '(company-preview-frontend)))

(add-hook 'eshell-directory-change-hook
          #'toggle-shell-auto-completion-based-on-path)
;; The default frontend screws everything up in short windows like
;; terminal often are
(add-hook 'eshell-mode-hook
          #'eshell-switch-company-frontend)

(use-package shell
  :straight '(:type built-in))

(use-package shell-command
  :after shell
  :config
  (shell-command-completion-mode))

(use-package bash-completion
  :after shell
  :config
  (bash-completion-setup))

(use-package lsp-pyright
  :config
  (setq lsp-pyright-disable-language-service nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t))

;;; a major mode
(use-package python-mode
  :demand
  :mode "\\.py\\'"
  :interpreter "ipython"
  :hook (python-mode . tree-sitter-mode)
  :hook (python-mode . lsp-deferred)
  :hook (python-mode . elpy-enable)
  :config
  (setq tab-width     4
        python-indent 4)
  (setq indent-tabs-mode nil)
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"
        python-shell-prompt-detect-failure-warning nil))

(add-hook
 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("def" .      #x2131)
           ("not" .      #x2757)
           ("in" .       #x2208)
           ("not in" .   #x2209)
           ("return" .   #x27fc)
           ("yield" .    #x27fb)
           ("for" .      #x2200)
           ;; Base Types
           ("int" .      #x2124)
           ("float" .    #x211d)
           ("str" .      #x1d54a)
           ("True" .     #x1d54b)
           ("False" .    #x1d53d)
           ;; Mypy
           ("Dict" .     #x1d507)
           ("List" .     #x2112)
           ("Tuple" .    #x2a02)
           ("Set" .      #x2126)
           ("Iterable" . #x1d50a)
           ("Any" .      #x2754)
           ("Union" .    #x22c3)))))

;;; an actual mode which uses it all
(use-package elpy
  :demand
  :mode ("\\.py\\'" . elpy-mode)
  :bind
  (:map elpy-mode-map
        ("C-M-n" . elpy-nav-forward-block)
        ("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode)
         (elpy-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             '((elpy-company-backend :with
                                                     company-yasnippet company-lsp))))))
  :config
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '72))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode))

(use-package ein
  :load-path "lisp"
  :config
  (require 'ob-ein)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ein . t))))

;;; defer
(use-package rust-mode)

;;; the actual fancy mode
(use-package rustic
  :mode "\\.rs\\'")

(add-hook 'lisp-mode-hook (lambda ()
                              (require 'slime)))

(with-eval-after-load 'slime
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations
                 '(sbcl ("sbcl") :coding-system utf-8-unix)))
  (when (executable-find "ccl")
    (add-to-list 'slime-lisp-implementations
                 '(ccl ("ccl") :coding-system utf-8-unix))))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "https://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

(use-package showkey
  :commands showkey-mode)

(use-package smartscan
  :config (global-smartscan-mode t))

(use-package spray
  :commands spray-mode)

(use-package elfeed
  :commands elfeed
  :init
  (setq elfeed-use-curl t)
  :config
  (setq elfeed-search-filter "@2-week-ago "
        elfeed-show-entry-switch #'pop-to-buffer
        shr-max-image-proportion 0.8))

(use-package elfeed-org
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (expand-file-name "elfeed.org" user-emacs-directory))
  :config
  (elfeed-org))

(use-package elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package elfeed-webkit
  :after elfeed
  :demand ;; !
  :init
  (setq elfeed-webkit-auto-enable-tags '(webkit comics))
  :config
  (elfeed-webkit-auto-toggle-by-tag)
  :bind (:map elfeed-show-mode-map
              ("%" . elfeed-webkit-toggle)))

;;; init.el ends here
