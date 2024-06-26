;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8; -*-
;;;
;;; Emacs Configuration with my little biralo friend, now dead.
;;;
;;; Every buffer possesses a major mode, which determines the editing
;;; behavior of Emacs while that buffer is current
;;;
;;; This file sets up a lot of outdated and obsolete packages
;;; so it is already bloated.
;;;
;;; The easiest thing to do is to comment out whole blocks of code
;;;
;;; In the modern world 'lsp-mode and 'lsp-ui are enough
;;; And they use
;;; - comint
;;; - completions (built-in)
;;; - corfu, consult, vertico, marginalia, orderless (enchancements)
;;; - ivy, counsel
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
;;; ~C-c @ C-t~ and then ~TAB~ finally!
;;; ~M-x consult-flycheck~
;;;
;;; [[https://groups.csail.mit.edu/mac/users/gjs/6.945/dont-panic/]]
;;; [[https://stuff.mit.edu/iap/2009/emacs/part1.html]]
;;;
;;; Code:

;;; a temporary kludge
;; TODO: remove this line
(setq-default user-emacs-directory "/home/lngnmn2/.emacs1.d/")
(setq-default load-prefer-newer t)

;;; custom.el has to come first -- global variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

(custom-set-variables '(load-prefer-newer t))

(setq load-prefer-newer t)

;;; warnings
(setq warning-minimum-level :debug)
(setq byte-compile-warnings t)
(setq native-comp-async-report-warnings-errors nil)

;;; a simple GC hack
(add-function :after after-focus-change-function
              (defun garbage-collect-maybe ()
                (unless (frame-focus-state)
                  (garbage-collect))))

;;; another nice hack
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook #'save-all)

;;; (Socks)
;; TODO: convert to custom
;; (setq-default socks-override-functions 1)
(setq-default url-gateway-method 'socks)
(setq-default socks-server '("Tor" "127.0.0.1" 9050 5))
(setq-default socks-noproxy '("127.0.0.1"))
(require 'socks)

;;; privoxy (with a tor forward)
(defun set-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services
                          '(("http"  . "127.0.0.1:8118")
                            ("https" . "127.0.0.1:8118"))))

(defun unset-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services nil))

;;; (Straight)
;; we use straight.el
(custom-set-variables '(package-enable-at-startup nil))
(custom-set-variables '(package-quickstart nil))

(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(setq frame-inhibit-implied-resize t)

;;; (locales)
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
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; We use the 'straight bloatware
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



(custom-set-variables '(straight-use-package-by-default t)
                      '(straight-vc-git-default-clone-depth 1))

(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1)

;; we will use this DSLs (a set of macros)
(custom-set-variables '(use-package-always-defer t)
                      '(use-package-always-ensure t)
                      '(use-package-expand-minimally t)  ; for debugging
                      '(use-package-compute-statistics t)
                      '(use-package-verbose t))

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally nil
      use-package-compute-statistics t
      use-package-verbose t)

;; use-package will use 'straight
(straight-use-package '(use-package :type built-in))
(straight-use-package '(bind-key :type built-in))
(straight-use-package 'diminish)
(straight-use-package 'delight)

;;; straight has a built-in compat package
;; (use-package compat
;;   :straight (:host github
;;                    :repo "emacs-compat/compat"))

;;; Doom macros
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

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

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and diff-hl have loaded)
    (after! (magit diff-hl) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

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


(straight-use-package 'async)
(straight-use-package 'org)
(straight-use-package 'ob-async)
(straight-use-package 'org-contrib)

(use-package ob-async
  :hook (org-load . (lambda () (require 'ob-async))))

;;; XDG
(use-package xdg
  :demand
  :straight '(:type built-in)
  )

;;; (Custom)
(use-package cus-edit
  :straight '(:type built-in)
  :custom (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (setq enable-local-variables :all)
  (when (file-exists-p custom-file)
    (load custom-file t)))

(straight-use-package 'ansi-color)

(defun shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add 'shell-command-on-region :after 'shell-command-in-view-mode)

(use-package scf-mode
  :hook ((compilation-mode grep-mode) . (lambda () (scf-mode 1))))

;;; ~M-x compile~, ~M-x consult-compile-error-
(use-package compile
  :straight (:type built-in)
  :init (require 'ansi-color)
  :hook (shell-mode . compilation-shell-minor-mode)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (defadvice compile (before ad-compile-starart activate)
    (ad-set-arg 1 t)))

(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun colourise-compilation-buffer ()
    "Enable colors in the *compilation* buffer."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colourise-compilation-buffer))

(use-package auto-compile
  :demand
  :config (auto-compile-on-load-mode t))

;; M-x consult-flymake
(use-package flymake
  :demand
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;;; Personification
(setq user-full-name "Ln Gnmn"
      user-mail-address "lngnmn2@yahoo.com")

(setq epa-file-encrypt-to "B5BCA34F13278C5B")

;;; less distractions
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
  (setq x-underline-at-descent-line t
        underline-minimum-offset 1)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0)
  (setq-default font-use-system-font t
                font-lock-maximum-decoration t)
  (global-font-lock-mode t))

(use-package font-lock+
  :demand
  :after font-lock)

;;; selection
;; xclip?
(use-package emacs
  :config
  (setq-default select-enable-clipboard t
                select-enable-primary t))

(use-package xclip
  :straight '(:type built-in)
  :config (xclip-mode t))

(custom-set-variables  '(backup-directory-alist    `(("." . ,(concat user-emacs-directory "backups")))))

;;; Never lose your work again
(use-package emacs
  :custom
  (auto-save-default t)
  (make-backup-files t)
  (backup-by-copying t)
  (version-control t)
  (vc-make-backup-files t)
  (delete-old-versions t)
  (create-lockfiles t)
  (auto-save-visited-mode t)
  :config
  (defun save-all ()
    (interactive)
    (save-some-buffers t))

  (add-hook 'focus-out-hook #'save-all))

(use-package super-save
  :demand
  :diminish
  :config
  (super-save-mode +1))

(use-package autorevert
  :straight '(:type built-in)
  :delight auto-revert-mode
  :bind ("C-x R" . revert-buffer)
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode))

;;; dired
;; nice and tidy section, alnost no over-configuration here
(use-package dired
  :straight '(:type built-in)
  :commands (dired dired-jump)
  :delight "Dired"
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

(use-package dired-async
  :straight '(:type built-in)
  :after dired
  :hook (dired . dired-async-mode))

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

(use-package dired-single
  :after dired
  :bind (:map dired-mode-map
              ([remap dired-find-file] . dired-single-buffer)
              ([remap dired-up-directory] . dired-single-up-directory)
              ("M-DEL" . dired-prev-subdir)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package fd-dired
  :init
  (global-set-key [remap find-dired] #'fd-dired))

(use-package dired-gitignore
  :straight '(:host github :repo "johannes-mueller/dired-gitignore.el")
  :hook (dired-mode . dired-gitignore-mode))

(use-package dired-git-info
  :hook (dired-mode . (lambda ()
                        (dired-git-info-mode t))))

(use-package dired-open
  :after (dired dired-jump)
  :custom (dired-open-extensions '(("mp4" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package dired-narrow
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-n" . dired-narrow)
              ("C-c C-f" . dired-narrow-fuzzy)))

;;; (Outline) built-in, ~org-mode~ uses it
(use-package outline
  :straight (:type built-in)
  :diminish Outl
  :custom
  (outline-minor-mode-cycle t)
  :hook (prog-mode . outline-minor-mode))

;;; unicode-fonts
(use-package unicode-fonts
  :demand t
  :config
  (unicode-fonts-setup))

(use-package ligature
  :demand t
  :diminish
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (global-ligature-mode 't))

;;; (fonts) the font section
(add-to-list 'default-frame-alist '(font . "SF Mono Light 16"))

(set-face-font 'default  (font-spec :family "SF Mono" :foundry "APPL" :weight 'light :size 22 :height 158))

(set-face-font 'fixed-pitch  (font-spec :family "SF Mono" :foundry "APPL" :weight 'light :size 22 :height 158))
(set-face-font 'fixed-pitch-serif (font-spec :family "SF Pro Display" :foundry "APPL" :weight 'light :size 22 :height 158))
(set-face-font 'variable-pitch (font-spec :family "SF Pro Text" :foundry "APPL" :weight 'light :size 22 :height 158))

;;; Faces
(set-face-attribute 'font-lock-constant-face nil :weight 'normal)
(set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil :italic t)
(set-face-attribute 'font-lock-doc-face nil :italic t)

;;; variable-pitch face
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

;;; fuck emoji
(set-fontset-font t 'unicode (font-spec :family "Noto Emoji") nil
                  'prepend)

;;; local fonts
(set-fontset-font t 'devanagari (font-spec :family "Noto Sans Devanagari" :weight 'light))
(set-fontset-font t 'tibetan (font-spec :family "Noto Serif Tibetan" :weight 'light))

;;; (misc)
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)

;;; Indentation
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package whitespace
  :straight '(:type built-in)
  :bind ("C-c w" . whitespace-mode)
  :config
  (global-whitespace-mode -1))

(use-package prettify-symbols
  :straight '(:type built-in)
  :hook (after-init . global-prettify-symbols-mode)
  :init
  (defvar base-prettify-symbols-alist
    '(("<=" . ?≤)
      (">=" . ?≥)
      ("<-" . ?←)
      ("->" . ?→)
      ("<=" . ?⇐)
      ("=>" . ?⇒)
      ("lambda" . ?λ )))
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;;; TODO: wrap in ~use-package~
;;; word wrapping
(setq-default sentence-end-double-space nil)
(setq-default word-wrap t)
(setq-default truncate-lines nil)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))

;; globals, better than text-mode-hook
;;; auto-fill
(setq-default tab-width 4)
(setq-default fill-column 72)
(setq-default next-line-add-newlines t) ; only useful with auto-fill
(set-fill-column 72)
(auto-fill-mode t)

;;; (Hacks) yet another cool hack
;; defined in ~grep.el~
(with-eval-after-load 'grep
  (when (executable-find "rg")
    (setq grep-program "rg")))

;; defined in ~files.el~
(with-eval-after-load 'files
  (when (executable-find "fd")
    (setq find-program "fd")))

(with-eval-after-load 'ispell
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")))

;;; Global modes
(global-highlight-changes-mode -1) ; crap

(global-display-fill-column-indicator-mode -1)

(global-visual-line-mode t)

(use-package hl-line
  :straight '(:type built-in)
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  :config
  (global-hl-line-mode t))

(global-subword-mode t)

(show-paren-mode t)
(transient-mark-mode t)

(use-package so-long
  :straight nil
  :hook
  (after-init-hook . global-so-long-mode))

;;; Server
(use-package server
  :demand
  :straight '(:type built-in)
  :hook (server-done . (lambda () (delete-frame)))
  :custom (server-kill-new-buffers t)
  :config (unless (server-running-p) (server-start)))

;;; Guru and biralo
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

;;; mixed-pitch
(use-package mixed-pitch
  :demand
  :hook
  ((text-mode . mixed-pitch-mode)
   (help-mode . mixed-pitch-mode)
   (Man-mode . mixed-pitch-mode)
   (Info-mode . mixed-pitch-mode)
   (org-mode . mixed-pitch-mode)
   (latex-mode . mixed-pitch-mode)
   (markdown-mode . mixed-pitch-mode)
   (gfm-mode . mixed-pitch-mode)
   (nov-mode . mixed-pitch-mode))
  :hook (mixed-pitch-mode . (lambda ()
                              (solaire-mode t)
                              (variable-pitch-mode t))))

(use-package savehist
  :straight '(:type built-in)
  :demand
  :init
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  :config
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (savehist-mode t))

(if (fboundp #'save-place-mode)
    (save-place-mode +1)
  (setq-default save-place t))

;;; recentf, uue ~consult-recent-file~
(use-package recentf
  :straight '(:type built-in)
  :demand
  :init
  (setq recentf-exclude
        `(,(expand-file-name "straight/build/" user-emacs-directory)
          ,(expand-file-name "eln-cache/" user-emacs-directory)
          ,(expand-file-name "etc/" user-emacs-directory)
          ,(expand-file-name "var/" user-emacs-directory)))
  (defun find-recent-file ()
    "Find a file that was recently visted using `completing-read'."
    (interactive)
    (find-file (completing-read "Find recent file: " recentf-list nil t)))
  (global-set-key (kbd "C-c r") #'find-recent-file)
  :config
  (recentf-mode t))

;; EPG
(use-package epa-file
  :straight '(:type built-in)
  :custom
  (epa-file-select-keys 'silent)
  :config
  (setq epa-pinentry-mode 'loopback)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq epa-file-select-keys nil)
  (epa-file-enable))

(use-package epg
  :straight '(:type built-in)
  :config
  (setq epg-pinentry-mode 'loopback)
  (setq epg-gpg-program "gpg"))

(use-package auth-source
  :straight (:type built-in)
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  :config
  (setq auth-source-cache-expiry nil))

(use-package pass
  :config
  (auth-source-pass-enable))

(use-package pinentry
  :demand
  :config (pinentry-start))

(straight-use-package 'crypt++)

(use-package org-crypt
  :straight (:type built-in)
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom
  (org-crypt-key "lngnmn2@yahoo.com"))

(use-package whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode)
  :config
  (diminish 'whitespace-cleanup-mode))

(use-package switch-buffer-functions
  :after recentf
  :preface
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (add-hook 'switch-buffer-functions #'my-recentf-track-visited-file))

;;; pcre
(straight-use-package 'pcre2el)
(use-package visual-regexp-steroids
  :init
  (require 'pcre2el)
  :config
  (setq vr/engine 'pcre2el)
  (map! "C-c s r" #'vr/replace)
  (map! "C-c s q" #'vr/query-replace))

(use-package abbrev
  :straight '(:type built-in)
  :delight
  :hook (text-mode . abbrev-mode)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;; Spelling
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
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_GB")
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-large-region t)
  (setq flyspell-consider-dash-as-word-delimiter-flag t)
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
  (flyspell-mode t))

(use-package flyspell-lazy
  :after fluspell)

(use-package flyspell-correct-ivy
  :demand
  :after ivy)

;;; Fucking projectile
(use-package projectile
  :demand
  :diminish
  :custom
  (projectile-project-search-path (list (expand-file-name "~/")))
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (emacs-startup . projectile-global-mode)
  :config
  (setq projectile-project-search-path (list (expand-file-name "~/")))
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-sort-order 'recentf))

(use-package ibuffer-projectile
  :demand
  :after projectile
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(use-package counsel-projectile
  :demand
  :after (projectile ivy-rich counsel)
  :commands (consult-projectile)
  :bind (([remap projectile-find-dir]         . #'counsel-projectile-find-dir)
         ([remap projectile-switch-to-buffer] . #'counsel-projectile-switch-to-buffer)
         ([remap projectile-grep]             . #'counsel-projectile-grep)
         ([remap projectile-ag]               . #'counsel-projectile-ag)
         ([remap projectile-switch-project]   . #'counsel-projectile-switch-project)))

(use-package consult-projectile
  :demand
  :after (projectile consult)
  :commands (consult-projectile))

(use-package projectile-variable
  :after projectile
  :commands (projectile-variable-put
             projectile-variable-get
             projectile-variable-alist
             projectile-variable-plist))

;;; flycheck
(use-package flycheck
  :demand
  :diminish
  :hook (after-init . global-flycheck-mode)
  :config (global-flycheck-mode t))

(use-package flycheck-pos-tip
  :demand
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

;;; (eglot)
(use-package eglot
  :demand
  :straight (:type built-in))

(use-package flycheck-eglot
  :after eglot
  :hook (after-init . global-flycheck-eglot-mode))

(use-package flycheck-inline
  :after flyckeck
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package avy-flycheck
  :after flycheck
  :config
  (avy-flycheck-setup))

;;; completion (built-in)
(use-package emacs
  :config
  (setq completions-format 'one-column) ;; like ido
  (setq completion-styles '(flex basic partial-completion emacs22))

  (unless (version< emacs-version "29.0")
    (setq completion-auto-help 'visible
          completion-auto-select 'second-tab
          completion-show-help t
          completions-sort nil ;; tricky crap
          completions-header-format nil))
  )

;;; using space key in completions
(use-package orderless
  :demand
  :custom
  (completion-category-overrides '((file (styles basic
                                                 partial-completion))))
  :config
  (add-to-list 'completion-styles 'orderless) ;; this
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;; the ~comint-mode~ enhancement
(use-package corfu
  :demand
  :hook ((after-init . global-corfu-mode)
         (eshell-mode . (lambda ()
                          (setq-local corfu-auto nil)
                          (corfu-mode t))))
  :config
  (setq corfu-auto t
        corfu-preview-current t
        corfu-quit-no-match t))

;; (use-package corfu-terminal
;;   :demand)

;;; Cape provides Completion At Point Extensions which can be used in combination with Corfu, Company
(use-package cape
  :demand
  :hook ((org-mode markdown-mode) . (lambda ()
                                      (add-hook 'completion-at-point-functions #'cape-tex)
                                      (add-hook
                                       'completion-at-point-functions
                                       #'cape-elisp-block 0 t)))
  :bind (("C-c a p" . completion-at-point) ;; capf
         ("C-c a t" . complete-tag)        ;; etags
         ("C-c a d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c a h" . cape-history)
         ("C-c a f" . cape-file)
         ("C-c a k" . cape-keyword)
         ("C-c a s" . cape-elisp-symbol)
         ("C-c a e" . cape-elisp-block)
         ("C-c a a" . cape-abbrev)
         ("C-c a l" . cape-line)
         ("C-c a w" . cape-dict)
         ("C-c a :" . cape-emoji)
         ("C-c a \\" . cape-tex)
         ("C-c a _" . cape-tex)
         ("C-c a ^" . cape-tex)
         ("C-c a &" . cape-sgml)
         ("C-c a r" . cape-rfc1345))
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  ;; Make these capfs composable.
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  )

(use-package yasnippet-capf
  :demand
  :after yasnippet
  :hook (yas-minor-mode . (lambda ()
                            (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))))

;; better completions
;; (use-package mct
;;   :config (mct-mode t))

;; (use-package prescient
;;   :demand
;;   :config (prescient-persist-mode +1))

;; (use-package ivy-prescient
;;   :hook (ivy-mode . ivy-prescient-mode)
;;   :hook (ivy-prescient-mode . prescient-persist-mode))

;;; undo-tree
(use-package undo-tree
  :demand t
  :diminish
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-timestamps nil)
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode t))

;;; command-line tools
(use-package fzf
  :commands (fzf fzf-directory)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "rg --no-heading -nH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 17))

(use-package ag
  :commands ag ag-regexp ag-dired)

;; sets up default key bindings
(use-package rg
  :demand
  :commands rg)

;; frontend
(use-package ripgrep
  :commands ripgrep-regexp)

;; some fancy shit
(use-package deadgrep
  :commands deadgrep)

;;; (wgrep)
(use-package wgrep
  :demand
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package wgrep-deadgrep
  :after (wgrep deadgrep)
  :commands wgrep-deadgrep)

;;; grep, wgrep, counsel-grep, etc.
;; knows ~eshell~ current directory!
(use-package grep
  :demand
  :straight '(:type built-in)
  :bind (:map grep-mode-map ("w" . wgrep-change-to-wgrep-mode))
  :init
  (setq-default grep-highlight-matches t
                grep-scroll-output t)
  (setq eshell-plain-grep-behavior nil)
  :config
  (setq wgrep-enable-key "w")
  (when (executable-find "rg")
    (setq-default grep-program "rg")
    (grep-apply-setting
     'grep-find-command
     '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))
    (add-to-list 'grep-find-ignored-directories "node_modules")
    (add-to-list 'grep-find-ignored-directories ".vscode")
    (add-to-list 'grep-find-ignored-directories "target")
    (global-set-key (kbd "C-x g") 'grep-find)))  ; it works

(use-package better-jumper)

(use-package dumb-jump
  :demand
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-default-project "~/")
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-quiet nil))

;;; trainsient first
(straight-use-package 'transient)

;;; magit
(use-package magit
  :demand
  :after transient
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

;;; ediff
(use-package ediff)

;;; diff-hl
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

(use-package expand-region
  :config (global-set-key (kbd "C-=") 'er/expand-region))

;;; vdiff
(use-package vdiff
  :commands (vdiff-buffers vdiff-files))

;;; ivy
(use-package ivy
  :init
  (let ((standard-search-fn #'ivy--regex-plus)
        (alt-search-fn #'ivy--regex-ignore-order))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            ;; (swiper         . ,standard-search-fn)
            ;; (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
  :config
  (require 'counsel nil t)
  (add-to-list 'counsel-compile-root-functions
               #'projectile-project-root)
  (setq ivy-fixed-height-minibuffer nil)
  (setq ivy-use-virtual-buffers t))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode)
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-project-root-cache-mode +1)
  (ivy-rich-mode t))

;; (use-package ivy-posframe
;;   :hook (ivy-mode . ivy-posframe-mode)
;;   :config
;;   (setq ivy-fixed-height-minibuffer nil
;;         ivy-posframe-border-width 10
;;         ivy-posframe-parameters
;;         `((min-width . 90)
;;           (min-height . ,ivy-height))))

;;; lsp-ivy
(use-package lsp-ivy
  :after (ivy lsp)
  :commands lsp-ivy-workspace-symbol)

;;; avy, ~M-x consult-goto-line~
(use-package avy
  :demand
  :bind (("M-g a" . #'avy-goto-line)))

(use-package ivy-avy
  :after ivy)

;;; counsel
;; this is such a cool embedded DLS
(use-package counsel
  :after projectile
  :diminish
  :bind
  (([remap apropos]                .  #'counsel-apropos)
   ([remap bookmark-jump]          .  #'counsel-bookmark)
   ([remap list-buffers]           .  #'counsel-ibuffer)
   ;;([remap switch-to-buffer]       .  #'counsel-buffer-or-recentf)
   ([remap describe-bindings]      .  #'counsel-descbinds)
   ([remap describe-face]          .  #'counsel-faces)
   ([remap describe-function]      .  #'counsel-describe-function)
   ([remap describe-variable]      .  #'counsel-describe-variable)
   ([remap describe-symbol]        .  #'counsel-describe-symbol)
   ;; ([remap execute-extended-command] .  #'counsel-M-x)
   ;; ([remap find-file]              .  #'counsel-find-file)
   ([remap find-library]           .  #'counsel-find-library)
   ([remap imenu]                  .  #'counsel-imenu)
   ([remap info-lookup-symbol]     .  #'counsel-info-lookup-symbol)
   ([remap load-theme]             .  #'counsel-load-theme)
   ([remap locate]                 .  #'counsel-locate)
   ([remap org-goto]               .  #'counsel-org-goto)
   ([remap org-set-tags-command]   .  #'counsel-org-tag)
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

;;; counsel and swiper first
(use-package swiper
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-r" . counsel-grep-or-swiper-backward)
         ("M-s i" . swiper-isearch)
         ("M-s r" . swiper-isearch-backward)
         ("M-s s" . swiper)
         ("M-s m" . swiper-multi)
         ("M-s w" . swiper-thing-at-point))
  :config
  (setq swiper-action-recenter t))

(use-package info+
  :demand)

;;; helpful
(use-package helpful
  :demand
  :diminish
  ;; :bind
  ;; ([remap describe-function] . helpful-callable)
  ;; ([remap describe-command] . helpful-command)
  ;; ([remap describe-variable] . helpful-variable)
  ;; ([remap describe-key] . helpful-key)
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

;;; the future, they said
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
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t))

(use-package vertico-multiform
  :hook (vertico-mode . vertico-multiform-mode))

(use-package marginalia
  :demand
  :after vertico
  :hook (after-init . marginalia-mode)
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

;;; consult, maybe here is ok
;; remaps are wonderful! ~consult-goto-line~, etc.
(use-package consult
  :demand
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-delay 0)
  (register-preview-function #'consult-register-preview)
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
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

;; M-x consult-flycheck
(use-package consult-flycheck
  :demand
  :after (consult flycheck))

(use-package consult-yasnippet
  :demand
  :after (consult yasnippet))

(use-package consult-eglot
  :demand
  :after (consult eglot))

(use-package consult-lsp
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-symbols)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] .  #'consult-lsp-symbols)))

;;; which-key
(use-package which-key
  :demand
  :diminish
  :config (which-key-mode t))

;; (use-package which-key-posframe
;;   :demand
;;   :after which-key
;;   :config (which-key-posframe-mode t))

;;; company
(use-package company
  :diminish
  :demand
  :custom
  (company-begin-commands '(self-insert-command))
  (company-show-quick-access t)
  (company-tooltip-align-annotations 't)
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
        company-tooltip-limit 17)
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

;;; TODO: convert to :bind
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

;; (use-package company-posframe
;;   :demand
;;   :after company
;;   :diminish
;;   :hook (company-mode . company-posframe-mode)
;;   :config
;;   (setq company-tooltip-minimum-width 40))

(use-package company-quickhelp
  :demand
  :after company
  :custom
  (company-quickhelp-delay 3)
  :hook (company-mode . company-quickhelp-mode))

;; prescient will be loaded on demand
;; (use-package company-prescient
;;   :demand
;;   :hook (company-mode . company-prescient-mode)
;;   :hook (company-prescient-mode . prescient-persist-mode))

;;; treesit (29+)

(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
  (add-to-list 'treesit-extra-load-path "~/.local/lib/"))

(setq-default treesit-font-lock-level 3)

;; old shit to load the old binaries
(straight-use-package 'tree-sitter-langs)

(use-package treesit-auto
  :demand
  ;; :after tree-sitter-langs ;; a hack
  :config (global-treesit-auto-mode t))

(use-package treesit
  :straight '(:type built-in)
  :diminish
  :hook (after-init . global-tree-sitter-mode))

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo
                     "emacs-tree-sitter/ts-fold")
  :diminish
  :hook (prog . ts-fold-mode)
  :hook (org-mode . ts-fold-mode)
  ;; :hook (python-mode . ts-fold-mode)
  ;; :hook (rust-mode . ts-fold-mode)
  :config (global-ts-fold-mode t))

;;; crap
(use-package combobulate
  :hook (
         (bash-ts-mode . combobulate-mode)
         (c-ts-mode . combobulate-mode)
         (python-ts-mode . combobulate-mode)
         (ocaml-ts-mode . combobulate-mode)
         (haskell-ts-mode . combobulate-mode)
         (rust-ts-mode . combobulate-mode)
         (go-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;;; xref
(use-package xref
  :after consult
  :custom
  (consult-line-start-from-top t)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  )

(use-package ivy-xref
  :after ivy
  :config
  ;; (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  )

;;; eldoc
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

;;; prog-mode-hook
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (setq indicate-empty-lines t)
            (set-fill-column 72)
            (auto-fill-mode t)
            (electric-pair-mode t)
            (electric-indent-mode t)
            (abbrev-mode t)))

;;; copilot
;; (use-package copilot
;;   ;; :straight '(:host github :repo "copilot-emacs/copilot.el")
;;   :diminish
;;   :after company
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; (use-package eglot-copilot
;;   :straight '(:host github :repo "tyler-dodge/eglot-copilot")
;;   :after (company eglot)
;;   :config
;;   (eglot-copilot-setup))

;;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (rainbow-delimiters-mode t))

;; TODO: convert to ~custom:~
;; [[https://emacs-lsp.github.io/lsp-mode/]]
;;; lsp-mode
(use-package lsp-mode
  :diminish
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :hook (lsp-completion-mode . (lambda ()
                                 (remq 'company-capf company-backends)))
  :custom
  (lsp-enable-semantic-highlighting t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-keep-workspace-alive nil)
  (lsp-auto-execute-action nil)
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting t)
  (lsp-auto-configure t))

(use-package company-lsp
  :demand
  :straight '(company-lsp :type git :host github :repo "tigersoldier/company-lsp")
  :after (company lsp)
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-treemacs
  :after lsp)

;;; dap-mode
;; just delegate
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;;; smartparens
(use-package smartparens
  :demand
  :diminish
  :hook (after-init . smartparens-global-strict-mode)
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

;;; programming
(use-package electric-spacing
  :demand
  :hook (prog-mode . electric-spacing-mode))

(use-package volatile-highlights
  :demand
  :diminish
  :hook (after-init . volatile-highlights-mode))

(use-package aggressive-indent
  :demand
  :diminish
  :hook (after-init . global-aggressive-indent-mode))

(use-package lisp-mode
  :straight '(:type built-in)
  :hook (lisp-mode . (lambda ()
                       (electric-pair-mode -1)
                       (electric-spacing-mode -1)
                       (eldoc-mode t)
                       (smartparens-strict-mode t)
                       (rainbow-delimiters-mode t)
                       (highlight-quoted-mode t)
                       (highlight-numbers-mode t)
                       )))

;;; emacs-lisp
(use-package emacs-lisp-mode
  :straight '(:type built-in)
  :hook (emacs-lisp-mode . (lambda ()
                             (electric-pair-mode -1)
                             (electric-spacing-mode -1)
                             (auto-compile-mode t)
                             (semantic-mode t)))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (with-eval-after-load 'semantic
    (semantic-default-emacs-lisp-setup)))

;; (use-package gtags-mode
;;   :hook ((emacs-startup . gtags-mode))
;;   :config
;;   (setq gtags-mode-update-args "--gtagsconf=/usr/share/gtags/gtags.conf --gtagslabel=universal-ctags"))

;; maybe this shit does not work
(use-package counsel-etags
  :hook
  (prog-mode . (lambda ()
                 (add-hook 'after-save-hook
                           'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

;;; ggtags
(use-package ggtags
  :diminish
  :hook (emacs-startup . ggtags-mode)
  :hook (prog-mode . ggtags-mode)
  :hook (LaTeX-mode . ggtags-mode))

(use-package counsel-gtags
  :after counsel
  :hook (ggtags-mode . counsel-gtags-mode))

(use-package flycheck-package
  :hook (emacs-lisp-mode . flycheck-package-setup))

  (use-package flycheck-elsa
    :hook (emacs-lisp-mode . flycheck-elsa-setup))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package elisp-def
  :diminish
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package auto-highlight-symbol
  :diminish 'auto-highlight-symbol-mode
  :commands (ahs-highlight-p)
  :hook (prog-mode . auto-highlight-symbol-mode)
  :config
  (diminish auto-highlight-symbol-mode)
  (setq ahs-case-fold-search nil
        ahs-default-range 'ahs-range-whole-buffer
        ahs-idle-interval 3.75))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; use ~helpful-at-point~
;; ~elisp-slime-nav-find-elisp-thing-at-point~
(use-package elisp-slime-nav
  :diminish
  :hook (emacs-lisp-mode  . elisp-slime-nav-mode))

(use-package eval-sexp-fu
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

;; breaks the mode
(use-package macrostep
  :diminish
  :commands (macrostep-expand)
  ;; :hook (emacs-lisp-mode . macrostep-mode)
  )

(use-package srefactor
  :commands (srefactor-lisp-format-buffer
             srefactor-lisp-format-defun
             srefactor-lisp-format-sexp
             srefactor-lisp-one-line))

(use-package emr
  :after emacs-lisp-mode)

;; unit tests
(use-package overseer
  :defer t)

(use-package ielm
  :demand
  :straight '(:type built-in)
  :hook (ielm-mode . smartparens-strict-mode)
  :hook (ielm-mode . rainbow-delimiters-mode)
  :hook (ielm-mode . eldoc-mode)
  :hook (ielm-mode . highlight-quoted-mode)
  :hook (ielm-mode . highlight-numbers-mode)
  :config
  (defun ielm-indent-line ()
    (interactive)
    (let ((current-point (point)))
      (save-restriction
        (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
        (lisp-indent-line))))
  (cl-pushnew 'company-files company-backends)
  (cl-pushnew 'company-capf company-backends))

;; Webshit to use ~prettier~
(use-package format-all
  :diminish
  :hook (emacs-lisp-mode . format-all-mode))

(use-package which-func
  :commands which-function)

;; ro be used with eglot
(use-package breadcrumb
  :defer t
  :hook (prog-mode . breadcrumb-local-mode)
  :hook (text-mode . breadcrumb-local-mode)
  )


(use-package xscheme
  :demand
  :hook (lisp-mode . (lambda ()
                       (electric-pair-mode -1)
                       (electric-spacing-mode -1)
                       ))
  :config
  (setq scheme-program-name "mit-scheme")
  (setq inferior-scheme-program "mit-scheme"))

(use-package yasnippet
  :demand
  :diminish
  :after company
  :hook (after-init . (lambda ()
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

(use-package ivy-yasnippet
  :demand
  :after (ivy yasnippet))

(use-package vterm
  :hook (vterm-mode . yas-minor-mode)
  :hook (vterm-mode . (lambda ()
                        (hl-line-mode -1)
                        (auto-fill-mode -1)))
  :commands (vterm vterm-other-window)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-tramp-shells '(("ssh" "/bin/bash")))
  :config
  (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
  (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
  (define-key vterm-mode-map (kbd "M-y") 'vterm-yank-pop)
  (define-key vterm-mode-map (kbd "M-/") 'vterm-send-tab)
  (add-to-list 'vterm-eval-cmds
               '("find-file-other-window" find-file-other-window)))

(add-to-list 'term-file-aliases
             '("fbterm" . "xterm-256color")
             '("rxvt-unicode-256color" . "xterm-256color"))

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

(use-package vterm-toggle
  :after vterm
  :bind (("C-`" . vterm-toggle-cd)
         :map vterm-mode-map
         (("<C-return>" . vterm-toggle-insert-cd)
          ("C-M-n" . vterm-toggle-forward)
          ("C-M-p" . vterm-toggle-backward)))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  (vterm-toggle-fullscreen-p nil))

(use-package multi-vterm
  :after vterm)

;;; comint [[https://www.emacswiki.org/emacs/ComintMode]]
(use-package pcmpl-args
  :demand
  :after pcomplet)

(use-package comint-intercept
  :hook (shell-mode . comint-intercept-mode))

;; maybe crap
(use-package comint-fold
  :straight '(:host github :repo "jdtsmith/comint-fold")
  :config
  (comint-fold-mode 1)
  ;; configure some modes specially; try first without this, many "just work"
  (add-hook 'gdb-mode-hook
            (comint-fold-configure-hook 0 (rx bol "(gdb)" (+ space))))
  (add-hook 'ipy-mode-hook
            (comint-fold-configure-hook 1 'ipy-prompt-regexp)))

;; over-configuration
;; (defun shell-comint-input-sender-hook ()
;;   "Check certain shell commands.
;;    Executes the appropriate behavior for certain commands."
;;   (setq comint-input-sender
;;         (lambda (proc command)
;;           (cond
;;            ;; Check for clear command and execute it.
;;            ((string-match "^[ \t]*clear[ \t]*$" command)
;;             (comint-send-string proc "\n")
;;             (let ((inhibit-read-only  t))
;;               (erase-buffer)))
;;            ;; Check for man command and execute it.
;;            ((string-match "^[ \t]*man[ \t]*" command)
;;             (comint-send-string proc "\n")
;;             (setq command (replace-regexp-in-string
;;                            "^[ \t]*man[ \t]*" "" command))
;;             (setq command (replace-regexp-in-string
;;                            "[ \t]+$" "" command))
;;             (funcall 'man command))
;;            ;; Send other commands to the default handler.
;;            (t (comint-simple-send proc command))))))
;; (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)

;;; xterm-color for comint
;; ~M-x xterm-color-text~ wow!
(use-package xterm-color
  :config
  ;; Comint and Shell
  (setenv "TERM" "xterm-256color")
  (setq xterm-color-use-bold-for-bright t
        term-color-preserve-properties t)
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output
                comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions
                        'xterm-color-filter nil t)))
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t))))

;; (with-eval-after-load 'eshell
;; (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;; (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(with-eval-after-load 'eshell
  ;; Work around bug in eshell's preoutput-filter code.
  ;; Eshell doesn't call preoutput-filter functions in the context of the eshell
  ;; buffer. This breaks the xterm color filtering when the eshell buffer is updated
  ;; when it's not currently focused.
  ;; To remove if/when fixed upstream.
  (defun eshell-output-filter@spacemacs-with-buffer (fn process string)
    (let ((proc-buf (if process (process-buffer process)
                      (current-buffer))))
      (when proc-buf
        (with-current-buffer proc-buf
          (funcall fn process string)))))
  (advice-add
   #'eshell-output-filter
   :around
   #'eshell-output-filter@spacemacs-with-buffer))

;;; ( eshell )
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

(use-package exec-path-from-shell
  :demand
  :config   (exec-path-from-shell-initialize))

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
  ;; add xterm-color-filter later
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (add-to-list 'eshell-modules-list 'eshell-smart))

(use-package esh-help
  :after eshell
  :hook (eshell-first-time-mode . setup-esh-help-eldoc)
  (setup-esh-help-eldoc))

;;; hide-mode-line
(use-package hide-mode-line
  :config (hide-mode-line-mode t))

;;; eshell
(use-package eshell
  :commands eshell
  :straight '(:type built-in)
  :hook (eshell-mode . (lambda ()
                         (hide-mode-line-mode t)
                         (semantic-mode -1)
                         (rainbow-delimiters-mode t)
                         (highlight-numbers-mode t)
                         (highlight-quoted-mode t)
                         (smartparens-strict-mode t)
                         (visual-line-mode +1)
                         (setq hscroll-margin 0)
                         (set-display-table-slot standard-display-table
                                                 0 ?\ )))
  :config
  (cl-pushnew 'company-capf company-backends)
  (setq comint-prompt-read-only t)
  (setq pcomplete-cycle-completions nil)

  ;; [[https://www.emacswiki.org/emacs/EshellAlias]]
  (defalias 'eshell/e 'find-file-other-window)
  ;; (defalias 'eshell/d 'dired)
  (defalias 'eshell/r 'find-file-read-only)
  (defalias 'eshell/rg "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)")

  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-destroy-buffer-when-process-dies t)
  (with-eval-after-load 'em-cmpl
    (setq eshell-cmpl-cycle-completions nil))
  (with-eval-after-load 'em-hist
    (setq eshell-hist-ignoredups t)
    (setq eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input))))
    )
  (with-eval-after-load 'em-term
    (append '("less" "ssh" "top" "htop" "vim" "nvim" "ncmpcpp") eshell-visual-commands))
  (with-eval-after-load 'em-alias
    (setq eshell-command-aliases-list '(("q"  "exit")
                                        ("f"  "find-file $1")
                                        ("ff" "find-file-other-window $1")
                                        ("rg" "rg -n -H --no-heading -e $1 $(git rev-parse --show-toplevel || pwd)")
                                        ("d"  "dired $1")
                                        ("bd" "eshell-up $1")
                                        ("rg" "rg --color=always $*")
                                        ("l"  "ls -lh $*")
                                        ("ll" "ls -lah $*")
                                        ("git" "git --no-pager $*")
                                        ("gg" "magit-status")
                                        ("cdp" "cd-to-project")
                                        ("clear" "clear-scrollback"))))
  ;; (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  ;; (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
  (with-eval-after-load 'em-glob
    (setq  eshell-glob-case-insensitive t
           eshell-error-if-no-glob t))
  (with-eval-after-load 'em-smart
    (setq eshell-where-to-jump 'begin
          eshell-review-quick-commands nil
          eshell-smart-space-goes-to-end t)
    (add-hook 'eshell-mode-hook #'eshell-smart-initialize))
  ;; (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)
  (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter))

(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'vterm)

(use-package eshell-up
  :commands eshell-up eshell-up-peek)

(straight-use-package 'shrink-path)

(use-package eshell-prompt-extras
  :after eshell
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-z
  :after eshell
  :config
  (setq eshell-z-freq-dir-hash-table-file-name
        (concat spacemacs-cache-directory "eshell/.z")))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 1.0))

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

(use-package eshell-toggle
  :custom
  (eshell-toggle-use-git-root t)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("C-c s t" . eshell-toggle))

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode +1))

;;; maybe crap
(use-package eat
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-shell-prompt-annotation t)
  :hook (eshell-first-time-mode . eat-eshell-mode)
  ;; :hook (eshell-first-time-mode . eat-eshell-visual-line-mode)
  :config
  (setq eshell-visual-commands '()))

;;; (ficus) and "productivy"
(use-package focus
  :commands (focus-mode focus-read-only-mode))

(use-package writeroom-mode
  :commands writeroom-mode)

;;; (nov)
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 72)
  :hook (nov-mode . (lambda ()
                      (visual-line-mode t)
                      (visual-fill-column-mode t)
                      (mixed-pitch-mode t)
                      (variable-pitch-mode t)
                      (focus-read-only-mode t)
                      (hide-mode-line-mode t))))

(use-package nov-xwidget
  :straight '(:host github :repo
                    "chenyanming/nov-xwidget")
  :commands nov-xwidget-view
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

(use-package shr-tag-pre-highlight
  :init
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       (propertize "#+END_SRC" 'face 'org-block-end-line ))
      (shr-ensure-newline)
      (setq end (point))
      (add-face-text-property start end '(:background "#1f2329" :extend t))
      (shr-ensure-newline)
      (insert "\n")))
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shrface-shr-tag-pre-highlight)))

(use-package shrface
  :hook (eww-after-render . shrface-mode)
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings)
  (setq shrface-href-versatile t))

;;; HTML viewer (not a web-browser)
(use-package eww
  :straight (:type built-in)
  :commands (eww eww-browse-url)
  :config
  (setq
   browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
   shr-use-fonts  nil                          ; No special fonts
   shr-use-colors nil
   shr-inhibit-images t
   shr-indentation 2                           ; Left-side margin
   shr-width 72                                ; Fold text to 70 columns
   eww-search-prefix "https://google.com/?q=")

  (setq eww-retrieve-command
        '("chromium" "--headless" "--dump-dom")))

(use-package counsel-web
  :after counsel
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-alternate-action #'w3m)
  (defvar counsel-web-map
    (let ((map (make-sparse-keymap "counsel-web")))
      (define-key map (kbd "w") #'counsel-web-suggest)
      (define-key map (kbd "s") #'counsel-web-search)
      (define-key map (kbd ".") #'counsel-web-thing-at-point)
      map))
  (global-set-key (kbd "C-c w") counsel-web-map))

(use-package w3m
  :commands (w3m w3m-browse-url)
  :config
  (setq w3m-quick-start nil)
  (setq w3m-display-mode 'plain)
  (setq w3m-use-cookies t)
  (setq w3m-use-toolbar nil)
  (setq w3m-use-tab-line nil)
  (setq w3m-use-tab-menubar nil))

(use-package google-this
  :diminish
  :custom
  (google-this-base-url "https://m.google")
  :config
  (google-this-mode 1))

;;; (SGML)
(use-package sgml-mode
  :straight (:type built-in)
  :hook
  (html-mode . sgml-electric-tag-pair-mode)
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 4)
  :config
  (setq sgml-xml-mode t)
  (setq sgml-transformation-function 'upcase))

(use-package nxml-mode
  :straight (:type built-in)
  :config
  (setq nxml-child-indent 2)
  (setq nxml-attribute-indent 2)
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t))

;; (straight-use-package 'nxhtml)
(use-package htmlize
  :demand)

(use-package tidy
  :demand
  :custom
  (sgml-validate-command "tidy"))

(defun tidy-buffer ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
                           "tidy -f /tmp/tidy-errs -q -i -wrap 72 -c" t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed")
  )
(global-set-key (kbd "C-x t") 'tidy-buffer)

(use-package engrave-faces
  :demand)

(use-package gnuplot
  :mode "\\.\\(gp\\|gpi\\|plt\\)'"
  :bind (:map gnuplot-mode-map
              ("C-c C-c".  gnuplot-send-buffer-to-gnuplot)))
;;; TeX
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(use-package tex
  :straight '(:type built-in)
  :hook (TeX-mode . (lambda ()
                      (setq
                       ;; Tell Emacs how to parse TeX files.
                       ispell-parser 'tex
                       ;; Don't auto-fill in math blocks.
                       fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))))
  :hook ((tex-mode-local-vars-hook
          latex-mode-local-vars-hook)
         . lsp)
  :config
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        ;; Use hidden directories for AUCTeX files.
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Don't start the Emacs server when correlating sources.
        TeX-source-correlate-start-server nil
        ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Just save, don't ask before each compilation.
        TeX-save-query nil)
  ;; Do not prompt for a master file.
  (setq-default TeX-master t)
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  ;; Enable `rainbow-mode' after applying styles to the buffer.
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  )

(straight-use-package 'auctex)

;;;###autoload
(defun +latex-fold-last-macro-a (&rest _)
  "Advice to auto-fold LaTeX macros after functions that
typically insert macros."
  ;; A simpler approach would be to just fold the whole line, but if point was
  ;; inside a macro that would kick it out. So instead we fold the last macro
  ;; before point, hoping its the one newly inserted.
  (TeX-fold-region (save-excursion
                     (search-backward "\\" (line-beginning-position) t)
                     (point))
                   (1+ (point))))

(use-package tex-fold
  :straight '(:type built-in)
  :after auctex
  :hook (TeX-mode . +latex-TeX-fold-buffer-h)
  :hook (TeX-mode . TeX-fold-mode)
  :config
  (defun +latex-TeX-fold-buffer-h ()
    (run-with-idle-timer 0 nil 'TeX-fold-buffer))
  ;; Fold after all AUCTeX macro insertions.
  (advice-add #'TeX-insert-macro :after #'+latex-fold-last-macro-a)
  ;; Fold after CDLaTeX macro insertions.
  (advice-add #'cdlatex-math-symbol :after #'+latex-fold-last-macro-a)
  (advice-add #'cdlatex-math-modify :after #'+latex-fold-last-macro-a)
  )

(use-package latex
  :straight '(:type built-in)
  :init
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

  )

(use-package lsp-latex
  :after tex
  :if (executable-find "texlab")
  ;; To properly load `lsp-latex', the `require' instruction is important.
  :hook (LaTeX-mode . (lambda ()
                        (require 'lsp-latex)
                        (lsp-deferred)))
  :custom (lsp-latex-build-on-save t))

(use-package xenops
  :after latex
  :hook (LaTeX-mode . xenops-mode))

(use-package magic-latex-buffer
  :defer t
  :init
  (add-hook 'TeX-update-style-hook 'magic-latex-buffer)
  (setq magic-latex-enable-block-highlight t
        magic-latex-enable-suscript t
        magic-latex-enable-pretty-symbols t
        magic-latex-enable-block-align nil
        magic-latex-enable-inline-image nil))

(use-package auctex-latexmk
  :after latex
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default.
  (setq-hook! LaTeX-mode TeX-command-default "LatexMk")
  :config
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package cdlatex
  :diminish
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode)
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil)
  )

(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))

(use-package company-auctex
  :after company)

(straight-use-package 'math-symbol-lists)

(use-package company-math
  :after company
  :config
  (setq company-math-disallow-unicode-symbols-in-faces t)
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode))

;; (use-package company-statistics
;;  :after company
;;  :config (company-statistics-mode t))

(use-package company-web
  :after company
  :config
  :hook (nxml-mode . (lambda ()
                       (add-to-list 'company-backends 'company-web-html))))


;; load this early
(straight-use-package 'ob-rust)
(straight-use-package 'ob-sml)
(use-package ob-erlang
  :straight '(ob-erlang :type git :host github :repo "xfwduke/ob-erlang")
  :defer t)

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

;;; (org-mode) org-mode!
;; need to load it so org-capture will work form anywhere
;; ~(directory-files org-directory t "\\.org$")~
(use-package org
  :demand
  :bind ("C-c c" .  (lambda () (interactive) (org-capture nil "i")))
  :bind ("C-c t" .  (lambda () (interactive) (org-capture nil "t")))
  :hook (org-mode . (lambda ()
                      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-link nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-special-keyword nil
                                          :inherit 'fixed-pitch)
                      (set-face-background 'org-code 'unspecified) ;; fix
                      (set-face-background 'org-block 'unspecified) ;; fix
                      (require 'whitespace)
                      (set-face-background 'whitespace-empty 'unspecified) ;; fix
                      (set-face-background 'whitespace-hspace 'unspecified) ;; fix
                      (mixed-pitch-mode t)
                      (variable-pitch-mode t)))
  :bind (:map org-mode-map
              ("C-c a" . org-agenda)
              ("C-c o b" . org-back-to-heading)
              ("C-c o p" . org-display-outline-path))
  :init
  (require 'xdg) ;; a hack
  ;; (setq org-directory (expand-file-name "org" (xdg-data-home)))
  ;; (setq org-agenda-files (list org-directory))
  :custom
  (org-directory (expand-file-name "~/org"))
  (org-agenda-files (directory-files org-directory t "\\.org$"))
  (org-default-notes-file (expand-file-name "~/NOTES.org"))
  (org-startup-indented t)
  :config
  (setq org-export-coding-system 'utf-8-unix)
  (setq org-html-coding-system 'utf-8-unix)
  (setq org-ascii-charset 'utf-8)
  (cl-pushnew 'company-capf company-backends)
  (setq org-capture-templates
        '(("i" "INBOX" entry (file+headline "~/org/INBOX.org" "Inbox")
           "* %?\n  %i\n  %a\n")
          ("t" "Todo" entry (file+headline "~/org/TODO.org" "Tasks")
           "* TODO %?\n  %i\n  %a\n")
          ("n" "Note" entry (file+headline "~/org/NOTES.org" "Notes")
           "* %?\n  %i\n  %a\n")
          ("c" "Cheatsheet" entry (file+headline "~/org/Cheatsheet.org"
                                                 "Cheat Sheets")
           "* %?\n  %i\n  %a\n")))
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…" )
  (setq org-auto-align-tags nil
        org-startup-with-inline-images nil
        org-latex-prefer-user-labels t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t))

(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PLAN(p)" "|" "DONE(d)")
          (sequence "ROUTINE" "DAILY" "DISCIPLINE" "|" "CANCELED(c)")
          (sequence "READ(r)" "|" "WRITE(w)" "PROG")
          (sequence "WAITING" "SOMEDAY(s)"))))

(with-eval-after-load 'org
  (setq org-tag-alist
        '(("read" . ?r) ("write" . ?w) ("program" . ?p) ("assistant" . ?a) ("trading" . ?t) ("chore" . ?c) ("daily" . ?d) ("routine" . ?r))))

(use-package valign
  :after org
  :diminish
  :hook ((markdown-mode org-mode) . valign-mode)
  :hook (valign-mode . (lambda () (unless valign-mode
                               (valign-remove-advice)))))

(use-package org-indent
  :diminish
  :straight '(:type built-in)
  :hook (org-mode . org-indent-mode)
  :custom (org-indent-indentation-per-level 4))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autosubmarkers t)
  )

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :init
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(use-package org-pretty-tags
  :hook (org-mode . org-pretty-tags-mode))

(use-package org-pretty-table
  :straight '(:host github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

(use-package org-super-agenda
  :after org
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  )

(use-package idle-org-agenda
  :after org-agenda
  :demand t
  :config (idle-org-agenda-mode))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(use-package org-rich-yank
  :after org
  :demand t)

(use-package org-sticky-header
  :after org
  :init
  (add-hook 'org-mode-hook 'org-sticky-header-mode))

(use-package company-org-block
  :after (company org)
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook (org-mode . (lambda ()
                      (cl-pushnew 'company-org-block company-backends
                                  )))
  )

(use-package toc-org
  ;; Automatically update toc when saving an Org file.
  :hook (org-mode . toc-org-mode))


;;; org-roam
(use-package org-roam
  :demand
  :after org
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind ("C-c i" .  (lambda () (interactive) (org-roam-capture nil "d")))
  :bind ((:map org-mode-map ("C-M-i" . completion-at-point))
         ("C-c n c" . org-roam-capture)
         ("C-c n r" . org-roam-refile)
         ("C-c n i" . org-roam-insert)
         ("C-c n f" . org-roam-find))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n\n %i")
           :unnarrowed t)))
  (org-roam-setup))

(use-package consult-org-roam
  :after org-roam
  :hook (org-roam . consult-org-roam-mode)
  :custom
  (consult-org-roam-grep-function #'consult-ripgrep)
)

;;; org-publish
(use-package org-publish
  :straight (:type built-in)
  :after org
  :config
  (setq org-publish-project-alist
        '(("orgfiles"
           :base-directory "~/Pages/"
           :base-extension "org"
           :publishing-directory "~/schiptsov.github.io/"
           :publishing-function org-html-publish-to-html
           :recursive t
           :html-doctype "html5"
           :html-html5-fancy t
           :html-head-include-default-style t
           :html-head-include-scripts t
           :headline-levels 5
           :section-numbers nil
           :auto-preamble t          ; Enable auto preamble
           :auto-postamble t         ; Enable auto postamble
           :table-of-contents nil    ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
           :with-author t
           :with-creator t
           :with-fixed-width t
           :with-latex t
           :with-date nil
           :with-toc nil
           :toc-levels 1             ; Just the default for this project.
           :auto-sitemap t           ; Generate sitemap.org
           :sitemap-sort-files anti-chronologically ; https://orgmode.org/manual/Site-map.html
           :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
           :sitemap-title "Org Notes"         ; ... with title 'Sitemap'.
           :html-link-home "index.html"
           :author "<schiptsov@gmail.com>"
           :html-head "
<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
<link href=\"https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@300&display=swap\" rel=\"stylesheet\">
<link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans:wght@300&display=swap\" rel=\"stylesheet\">
<link href=\"https://fonts.googleapis.com/css2?family=Noto+Serif:wght@300&display=swap\" rel=\"stylesheet\">
<style>
font-family: 'Noto Serif', serif;
font-family: 'Noto Sans', sans-serif;
font-family: 'Source Code Pro', monospace;
</style>
<script src=\"https://polyfill.io/v3/polyfill.min.js?features=es6\"></script>
<script type=\"text/javascript\" id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>
<link rel='stylesheet' type='text/css' href='/css/main.css'/>"
           :html-preable t
           )
          ("images"
           :base-directory "~/Pages/images/"
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory "~/schiptsov.github.io/images/"
           :publishing-function org-publish-attachment
           )
          ("website" :components ("orgfiles" "images")))))

;;; org-pandoc-import
(use-package org-pandoc-import
  :straight (:host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors"))
  :after org)

;;; (ox) exports
(use-package ox
  :straight (:type built-in)
  :after org
  :config
  (setq org-html-head-include-scripts t
        org-export-with-toc nil
        org-export-with-author t
        org-export-headline-levels 5
        org-export-with-drawers nil
        org-export-with-email t
        org-export-with-footnotes t
        org-export-with-sub-superscript t
        org-export-with-latex t
        org-export-with-section-numbers nil
        org-export-with-properties nil
        org-export-with-smart-quotes t
        org-export-backends '(pdf ascii html latex gfm odt md pandoc)))

(use-package ox-html
  :straight (:type built-in)
  :config
  (setq org-html-coding-system 'utf-8-unix))

(use-package ox-latex
  :straight (:type built-in)
  :after ox
  :init
  (setenv "LANG" "en_US.UTF-8"))

(use-package ox-gfm
  :after ox)

(use-package ox-hugo
  :after ox)

(use-package ox-pandoc
  :after ox)

;;; rethro packages
;; broken - invalid utf-8 char or something
;; [[https://code.launchpad.net/~nxhtml/nxhtml/main]]
;; (use-package nxhtml
;;   :config
;;   (require 'autostart))

;;(use-package icicles)

;;(use-package grep+
;;  :after grep)

;;(use-package dired+
;;  :after dired)

;;; CSV
(use-package csv-mode :mode ("\\.\\(csv\\|tsv\\)\\'"))

;;; Java
;; [[https://github.com/emacs-lsp/lsp-java]]
(after! 'projectile
  (pushnew! projectile-project-root-files "gradlew" "build.gradle"))

(use-package java-mode
  :straight '(:type built-in))

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package android-mode
  :after java
  :commands android-mode
  :hook (yas-minor-mode . android-mode))

(use-package dap-java
  :straight '(:type built-in))

;;; Clojure
(after! projectile
  (pushnew! projectile-project-root-files "project.clj" "build.boot"
            "deps.edn"))

(use-package clojure-mode
  :hook (clojure-mode . lsp-deferred))

(use-package cider)
(use-package clj-refactor)
(use-package flycheck-clj-kondo)

(use-package neil
  :straight (:host github :repo "babashka/neil" :files ("*.el")))

;;; Scala
(after! projectile
  (add-to-list 'projectile-project-root-files "build.sbt"))

(use-package sbt-mode)

(use-package scala-mode
  :custom
  (flycheck-scala-executable "scalac --color never")
  :interpreter
  ("scala3 --color never" . scala-mode)
  :config
  (setq prettify-symbols-alist scala-prettify-symbols-alist))

(use-package lsp-metals
  :demand
  :hook (scala-mode . lsp-deferred))

;;; Python
;; use the built-in ones
(after! projectile
  (pushnew! projectile-project-root-files "pyproject.toml"
            "requirements.txt" "setup.py"))

(use-package lsp-pyright
  :demand
  :if (executable-find "pyright")
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  )

;;; [[https://robbmann.io/posts/006_emacs_2_python/]]
(use-package python
  :straight '(:type built-in)
  :custom
  (flycheck-pylintrc "~/.pylintrc")
  (flycheck-python-pylint-executable "/usr/bin/pylint"))

(use-package blacken
  :delight
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 72))

(use-package pyvenv
  :after python
  ;; :bind
  ;; (("C-c p w" . pyvenv-workon)
  ;;  ("C-c p d" . pyvenv-deactivate)
  ;;  ("C-c p a" . pyvenv-activate))
  :config
  (pyvenv-mode))

(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  :config
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1))

(use-package tramp-sh
  :straight '(:type built-in)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package tramp-venv
  :straight '(:type built-in)
  :commands (tramp-venv-activate tramp-venv-deactivate))

(use-package pytest
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-last-failed
             pytest-pdb-last-failed
             pytest-module
             pytest-pdb-module))

;;; Rust
(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(straight-use-package 'rust-mode)

(use-package rustic
  :demand
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-deferred)
  :config
  (cl-pushnew 'rustic-clippy flycheck-checkers)
  (sp-local-pair 'rustic-mode "'" nil :actions nil)
  )

;;; C/C++
(use-package cc-mode
  :straight '(:type built-in)
  :hook ((c-mode-local-vars-hook
          c++-mode-local-vars-hook) . tree-sitter-mode)
  :hook (c-mode-common . rainbow-delimiters-mode)
  :hook (c-mode-common . (lambda ()
                           (setq-local c-basic-offset 4
                                       tab-width 4
                                       indent-tabs-mode t)
                           (setq c-syntactic-indentation t)
                           (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
                           (cl-pushnew 'company-clang comnany-backends)
                           ))
  :hook (c++-mode . (lambda ()
                      (setq flycheck-clang-language-standard "c++20")))
  :config
  (setq c-basic-offset tab-width
        c-backspace-function #'delete-backward-char)
  )

(use-package google-c-style
  :hook (((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package ccls
  :after lsp
  :hook ((c-mode c++-mode) . lsp-deferred)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  )

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "^.ccls-cache$")
  (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
  (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))

(use-package cmake-mode
  :hook (cmake-mode . lsp-deferred)
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package company-cmake  ; for `cmake-mode'
  :straight '(:type built-in)
  :after cmake-mode
  :config (cl-pushnew 'company-cmake company-backends))

(use-package cmake-ide
  :config
  (setq cmake-ide-flags-c++ (append '("-std=c++20")))
  (cmake-ide-setup))

(use-package company-c-headers
  :after cc-mode
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/v1/")
  (cl-pushnew 'company-c-headers company-backends))

(use-package function-args
  :hook (c-common . function-args-mode)
  :config
  (set-default 'semantic-case-fold -1)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (fa-config-default))

(use-package demangle-mode
  :hook (c++mode . demandgle-mode))

(use-package gdb-mi
  :init
  (setq gdb-many-windows t
        gdb-show-main t))

(use-package realgud
  :commands realgud:gdb)

(use-package rmsbolt
  :commands (rmsbolt))

;;; pdf
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq mailcap-user-mime-data
        '((type . "application/pdf")
          (viewer . pdf-view-mode))))

(use-package pdf-view-restore
  :after pdf-tools
  :init
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;;; djvu
(straight-use-package 'djvu)

(use-package djvu3
  :straight '(:host github :repo "dalanicolai/djvu3")
  :magic ("%DJVU" . djvu-read-mode)
  ;; :mode ("\\.djvu\\'" . djvu-read-mode)
  )

;;; mu4e
(use-package mu4e
  :straight '(:type built-in)
  :hook (mu4e-compose-mode . (lambda ()
                               (require 'htmlize)
                               (require 'org-msg)
                               (org-msg-mode t)
                               (org-msg-edit-mode)))
  :commands mu4e mu4e-compose-new
  :init
  (provide 'html2text) ; disable obsolete package
  :config
  (setq mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask)

  (setq user-mail-address "lngnmn2@yahoo.com"
        user-full-name  "Ln Gnmn"
        mu4e-compose-signature
        (concat
         "Ln Gnmn\n"
         "https://lngnmn2.github.io/\n"))

  (setq mu4e-compose-format-flowed t)
  (setq org-mu4e-convert-to-html t)

  (setq mu4e-attachment-dir  (expand-file-name "~/Downloads/"))

  (setq mu4e-maildir (expand-file-name "~/.Maildir"))
  (setq mu4e-refile-folder "/Archive"
        mu4e-trash-folder  "/Trash"
        mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Draft")

  (setq mu4e-maildir-shortcuts
        '(("/INBOX"   . ?i)
          ("/Sent"    . ?s)
          ("/Archive" . ?a)
          ("/Trash"   . ?t)))

  (setq mu4e-get-mail-command "mbsync -a -q"
        mu4e-change-filenames-when-moving t)

  (setq mu4e-compose-reply-to-address user-mail-address)

  (setq mail-user-agent 'mu4e-user-agent
        message-mail-user-agent 'mu4e-user-agent)

  (setq mu4e-update-interval nil
        mu4e-sent-messages-behavior 'sent
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t) ; close after sending
  )

;;; smtpmail-send-it
(use-package smtpmail
  :defer t
  :init
  (setq smtpmail-default-smtp-server "smtp.mail.yahoo.com")
  :config
  (setq gnutls-algorithm-priority "PFS")
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-smtp-user "lngnmn2"
        smtpmail-local-domain "yahoo.com"
                ;;; smtpmail-starttls-credentials '(("smtp.mail.yahoo.com" 587 nil nil))
                ;;; smtpmail-auth-credentials '(("lngnmn2@smtp.mail.yahoo.com" 587 "lngnmn2@yahoo.com" nil))
        smtpmail-smtp-server "smtp.mail.yahoo.com"
        starttls-use-gnutls t
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
  )

(use-package org-mime
  :after org
  :init
  (defun htmlize-and-send ()
    "When in an org-mu4e-compose-org-mode message, htmlize and send it."
    (interactive)
    (when
        (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
      (org-mime-htmlize)
      (org-mu4e-compose-org-mode)
      (mu4e-compose-mode)
      (message-send-and-exit)))
  :config
  ;; This overloads the amazing C-c C-c commands in org-mode with one more function
  ;; namely the htmlize-and-send, above.
  (add-hook 'org-ctrl-c-ctrl-c-hook #'htmlize-and-send t)
  )
;; org-mime:1 ends here

(use-package org-msg
  :after org
  :config
  (setq org-msg-default-alternatives '((new     	    . (text))
                                       (reply-to-html	. (text))
                                       (reply-to-text	. (text))))
  (setq mail-user-agent 'mu4e-user-agent)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi%s,\n\n"
        org-msg-recipient-names '(("lngnmn2@yahoo.com" . "Ln Gnmn"))
        org-msg-greeting-name-limit 3
        org-msg-convert-citation t
        org-msg-signature "
  #+begin_signature
  --
  Ln Gnmn,
  [[https://lngnmn2.github.io]]
  #+end_signature"))

;; (use-package yeetube)

(use-package octave-mode
  :straight '(:type built-in)
  :mode "\\.m\\'")

;; haskell
(after! projectile
  (add-to-list 'projectile-project-root-files "stack.yaml"))

(use-package haskell-mode
  :custom
  (haskell-stylish-on-save t)
  :hook (haskell-mode . (lambda ()
                          (interactive)
                          (turn-on-haskell-doc-mode)
                          (turn-on-haskell-indentation))))

(use-package flycheck-haskell
  :after (flycheck haskell-mode))

(use-package company-ghci
  :after (company haskell-mode)
  :config
  (cl-pushnew 'company-ghci company-backends))

(use-package lsp-haskell
  :after lsp
  :hook ((haskell-mode . lsp-deferred)
         (literate-haskell-mode . lsp-deffered))
  :init
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  (setq lsp-haskell-server-args nil))

;;; Common Lisp
(use-package sly
  :hook (lisp-mode-local-vars . sly-editing-mode)
  ;; :hook (inferior-lisp-mode . inferior-slye-mode)
  :custom
  (inferior-lisp-program "sbcl")
  :init
  (setq inferior-lisp-program "sbcl")
  (setq sly-lisp-implementations
        '((sbcl ("/usr/bin/sbcl"))))
  (setq sly-auto-start 'always)
  (setq sly-contribs '(sly-fancy
                       sly-autodoc
                       sly-indentation
                       sly-fuzzy
                       sly-company
                       sly-references
                       sly-repl
                       sly-sbcl-exts
                       sly-scratch))
  :config
  (setq sly-net-coding-system 'utf-8-unix)
  (require 'sly-quicklisp)
  (sly-setup)
  ;; (sly-autodoc-mode)
  )

(use-package sly-company
  :after (sly company)
  :config (setq sly-company-completion 'fuzzy
                sly-company-after-completion 'sly-company-just-one-space))

(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-asdf
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sly-quicklisp
  :init
  (add-to-list 'sly-contribs 'sly-quicklisp))

(use-package sly-stepper
  :straight '(:type built-in)
  :init
  (add-to-list 'sly-contribs 'sly-stepper))

;;; ocaml
(after! projectile
  (add-to-list 'projectile-project-root-files "dune-project"))

(use-package merlin
  :after company
  :config
  (cl-pushnew 'merlin-company-backend company-backends)
  (setq merlin-completion-with-doc t))

(use-package tuareg
  :hook (tuareg-mode-local-vars . merlin-mode)
  :hook (tuareg-mode-local-vars . lsp-deferred)
  :hook (tuareg-mode-local-vars . tree-sitter-mode)
  :config
  (setq tuareg-prettify-symbols-full t)
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package flycheck-ocaml
  :after (flycheck merlin)
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

(use-package merlin-iedit
  :after iedit)

(use-package utop
  :commands (utop)
  :hook (tuareg-mode . utop-minor-mode))

(use-package sml-mode
  :mode "\\.s\\(?:ml\\|ig\\)\\'")

(use-package company-mlton
  :straight '(company-mlton :type git :host github :repo "MatthewFluet/company-mlton")
  :after company
  :hook (sml-mode . company-mlton-init)
  :config
  (add-to-list 'company-backends 'company-mlton-grouped-backend))

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

;;; erlang
;; just right mode -- comint, etags, electric modes, flymake
(use-package erlang
  :straight '(:type built-in) ;; DO NOT clone whole otp
  :load-path (lambda () (car (file-expand-wildcards "/usr/lib64/erlang/lib/tools-*/emacs")))
  :hook (erlang-mode . flymake-mode)
  :hook (erlang-mode . flycheck-mode)
  :hook (erlang-mode . lsp-deferred)
  :config
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; default node name to emacs@localhost
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (setq erlang-root-dir "/usr/lib64/erlang")
  (setq lsp-ui-doc-enable t)
  (require 'erlang-start))

(use-package company-distel
  :straight '(:host github :repo "sebastiw/distel-completion" :files ("company-distel.el"))
  :hook (erlang-mode .
                     (lambda ()
                       (cl-pushnew 'company-distel company-backends))))

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

;;; crap
(use-package chatgpt-shell
  ;; :straight (:host github
  ;;                  :repo "xenodium/chatgpt-shell"
  ;;                  :files ("shell-maker.el" "chatgpt-shell.el" "ob-chatgpt-shell.el"))
  :config
  (require 'shell-maker)
  (require 'ob-chatgpt-shell)
  :custom
  (chatgpt-shell-openai-key (auth-source-pick-first-password :host
                                                             "api.openai.com")))
;;; crap
;;; ~machine api.openai.com login apikey password TOKEN~
(use-package gptel
  :config
  (setq gptel-api-key "your key"))

;;; (themes)
(use-package doom-themes
  :demand t
  :hook (after-init . (lambda ()
                        (load-theme 'doom-tokyo-night t)))
  :hook (org-mode . (lambda ()
                      (require 'doom-themes-ext-org)
                      (doom-themes-org-config)))
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tokyo-night t))

(use-package solaire-mode
  :demand t
  :custom (solaire-mode-remap-fringe t)
  :hook (mixed-pitch-mode .  solaire-mode-reset)
  :hook (prog-mode . solaire-mode-reset)
  :hook (after-init . (lambda ()
                        (solaire-global-mode +1))))

;;; init.el ends here
