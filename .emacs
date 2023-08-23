;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; use hooks and implicit defers,
;; not afters and configs
;; some packages work only as modes in configs

(setq-default load-prefer-newer t)

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
  (save-some-buffers t))
(add-hook 'focus-out-hook #'save-all)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq enable-local-variables :all)

;; (setq-default socks-override-functions 1)
(setq-default url-gateway-method 'socks)
(setq-default socks-server '("Tor" "127.0.0.1" 9050 5))
(setq-default socks-noproxy '("127.0.0.1"))
(require 'socks)

;; we use straight.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(setq frame-inhibit-implied-resize t)

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

;; use-package will use 'straight
(straight-use-package '(bind-key :type built-in))
(straight-use-package '(use-package :type built-in))
(straight-use-package 'diminish)
(straight-use-package 'delight)

;; we will use this DSLs (a set of macros)
(setq
 use-package-always-defer nil   ;; should be nil for :defer to work
 use-package-always-ensure t    ;; should be t for straight
 use-package-compute-statistics t
 use-package-verbose t)

(menu-bar-mode t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

(setq x-underline-at-descent-line t)
(setq underline-minimum-offset 1)

(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-splash-screen t)

(setq select-enable-clipboard t
      select-enable-primary t)

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

(set-fontset-font t 'unicode (font-spec :family "Noto Emoji") nil 'prepend)
(set-fontset-font t 'devanagari (font-spec :family "Noto Sans Devanagari"))

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 35.0)

(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq-default indent-tabs-mode nil)
(add-hook 'sh-mode-hook (lambda () (setq indent-tabs-mode t)))

(setq-default tab-always-indent 'complete)

(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq whitespace-style '(face spaces tabs newline space-mark tab-mark))
(global-whitespace-mode t)
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

(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode t)

(setq sentence-end-double-space nil)

(setq-default word-wrap t)

(setq-default truncate-lines nil)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))

(setq-default tab-width 4)
(setq-default fill-column 72)
(set-fill-column 72)
(auto-fill-mode t)

;; yet another cool hack
(when (executable-find "rg")
  (setq grep-program "rg")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(when (executable-find "fd")
  (setq find-program "fd"))

(global-set-key (kbd "C-x C-g") 'grep-find)

(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))

(global-highlight-changes-mode -1)

(global-display-fill-column-indicator-mode -1)

(global-visual-line-mode t)
(global-hl-line-mode t)

(global-subword-mode t)

(show-paren-mode t)
(transient-mark-mode t)

;; clashes with spartparens
(setq-default electric-indent-chars '(?\n ?\^?))
(setq electric-pair-preserve-balance t)
(electric-pair-mode -1)

(electric-indent-mode t)

(delete-selection-mode t)

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(abbrev-mode t)
(setq save-abbrevs 'silently)
;; (bind-key "M-/" 'hippie-expand)

(auto-save-visited-mode t)

(recentf-mode t)
(setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory)
                          ,(expand-file-name "etc/" user-emacs-directory)
                          ,(expand-file-name "var/" user-emacs-directory)))

(defun find-recent-file ()
  "Find a file that was recently visted using `completing-read'."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))
(global-set-key (kbd "C-c r") #'find-recent-file)

(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode t)

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)  ; Overrides `mark-page'
(global-set-key (kbd "C-x C-n") 'next-buffer)      ; Overrides `set-goal-column'

(setq compilation-scroll-output t)

(defun colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

(use-package gcmh
  :straight t
  :demand t
  :diminish t
  :config
  (gcmh-mode t))

(use-package auto-compile
  :straight t
  :demand t
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

(use-package eldoc
  :straight t
  :diminish
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode t))

(use-package emacs
  :custom
  (auto-save-default t)
  (make-backup-files t)
  (backup-by-copying t)
  (version-control t)
  (vc-make-backup-files t)
  (delete-old-versions t)
  (create-lockfiles t)
  (auto-save-visited-mode t))

(use-package super-save
  :straight t
  :diminish
  :config
  (super-save-mode +1))

(use-package undo-tree
  :straight t
  :diminish
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode t))

(use-package fontaine
  :straight t
  :defer t)

(use-package unicode-fonts
  :straight t
  :config
  (unicode-fonts-setup))

(use-package ligature
  :straight t
  :diminish
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (global-ligature-mode 't))

(use-package all-the-icons
  :straight t
  :demand t)

(use-package all-the-icons-completion
  :after all-the-icons
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-ivy
  :straight t
  :after ivy
  :config
  (setq all-the-icons-ivy-buffer-commands nil)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode)
  :config
   (setq emojify-styles)
   (emojify-set-emoji-styles emojify-styles))

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
  :hook (mixed-pitch-mode . (lambda ()
                              (solaire-mode t)
                              (variable-pitch-mode t))))

(use-package visual-fill-column
  :straight t
  :hook (visual-line-mode . visual-fill-column-mode))

(defun display-ansi-colors ()
  "Render colors in a buffer that contains ASCII color escape codes."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Use the latest version
(straight-use-package 'org)
(straight-use-package 'org-contrib)

(straight-use-package 'async)
(use-package ob-async
  :straight t
  :hook (org-load . (lambda () (require 'ob-async))))

(use-package org
  :straight t
  :custom
  (org-src-tab-acts-natively t)
  :hook (org-mode . (lambda ()
                      (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                      (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)))
  :hook (org-mode . (lambda ()
                      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-link nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-special-keyword nil
                                          :inherit 'fixed-pitch)
                      (solaire-mode t)
                      (mixed-pitch-mode t)
                      (variable-pitch-mode t)))
  :bind (:map org-mode-map
              ("C-c a" . org-agenda)
              ("C-c b" . org-back-to-heading)
              ("C-c p" . org-display-outline-path))
  :config
  (require 'xdg)
  (setq org-export-coding-system 'utf-8-unix)
  (setq org-html-coding-system 'utf-8-unix)
  (setq org-ascii-charset 'utf-8)

  (setq org-use-property-inheritance t)

  (setq org-export-with-sub-superscripts '{})

  (setq org-inline-src-prettify-results '("⟨" . "⟩"))

  (setq org-directory (expand-file-name "org" (xdg-data-home)))
  (setq  org-agenda-files (list org-directory))

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

  (setq org-fontify-done-headline t)

  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t)

  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)

  (setq-local prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                         ("#+END_SRC" . "«")
                                         ("#+begin_src" . "»")
                                         ("#+end_src" . "«")
                                         ("lambda"  . "λ")
                                         ("->" . "→")
                                         ("->>" . "↠")))
  (setq-local prettify-symbols-unprettify-at-point 'right-edge))

;;; load this early
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))

(use-package ef-themes
  :straight t
  :defer t)

(use-package doom-themes
  :straight t
  :hook (after-init . (lambda ()
                        (load-theme 'doom-nord t)))
  :hook (org-mode . (lambda ()
                      (require 'doom-themes-ext-org)
                      (doom-themes-org-config)))
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally
  :config
  (load-theme 'doom-nord t))

(use-package solaire-mode
  :straight t
  :hook (mixed-pitch-mode .  solaire-mode-reset)
  :hook (prog-mode . solaire-mode-reset)
  :hook (after-init . (lambda ()
                        (solaire-global-mode +1))))

(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table t)
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")))

(use-package org-reverse-datetree
  :straight t
  :after org
  :demand)

(use-package ox-clip
  :straight t
  :after org
  :config
  (setq org-hugo-front-matter-format "yaml"))

(use-package ox-hugo
  :straight t
  :after org)

(use-package guru-mode
  :straight t
  :delight
  :config
  (guru-global-mode t))

(use-package nyan-mode
  :straight t
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

(use-package org-indent
  :straight '(:type built-in)
  :after org
  :hook (org-mode . org-indent-mode))

(use-package org-rich-yank
  :straight t
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package org-cliplink
  :straight t
  :after org)

(use-package org-download
  :straight t
  :after org)

(use-package org-web-tools
  :straight t
  :after org)

(use-package idle-org-agenda
  :after org-agenda
  :straight t
  :config (idle-org-agenda-mode))

(use-package info+
  :straight t)

(use-package info-colors
  :straight t
  :hook (Info-selection  . info-colors-fontify-node))

(use-package which-key
  :straight t
  :diminish
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

(use-package pdf-tools
  :straight t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode))

(use-package saveplace-pdf-view
  :straight t
  :defer t
  :after pdf-view)

(use-package markdown-mode
  :straight t
  :defer t
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
  :straight t
  :after markdown-mode
  :hook (markdown-mode . poly-markdown-mode))

(use-package grip-mode
  :straight t
  :hook (markdown-mode . grip-mode))

(use-package ox-gfm
  :straight t
  :commands (org-gfm-export-as-markdown org-gfm-export-to-markdown)
  :after org)

(use-package vterm
  :straight t
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

(setq eww-retrieve-command
      '("chromium" "--headless" "--dump-dom"))

(use-package w3m
  :straight t
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
  :custom
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
  :straight t
  :diminish t
  :config
  (google-this-mode 1))

(global-set-key (kbd "C-c ;") #'comment-line)

(use-package xref
  :straight t
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (setq xref-search-program 'ripgrep))

(use-package dumb-jump
  :straight t
  :after xref
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package avy
  :straight t
  :defer t)

(use-package rg
  :straight t)

(use-package fzf
  :straight t)

(use-package ag
  :straight t)

(use-package ripgrep
  :straight t
  :defer t)

(use-package deadgrep
  :straight t
  :defer t)

(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(setq completions-format 'one-column) ;; like ido
(setq completion-styles '(flex basic partial-completion emacs22))

(use-package orderless
  :straight t
  :config
  (add-to-list 'completion-styles 'orderless)
  (setq orderless-component-separator "[ &]")

  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))

(unless (version< emacs-version "29.0")
  (setq completion-auto-help 'visible
        completion-auto-select 'second-tab
        completion-show-help t
        completions-sort t
        completions-header-format nil))

(use-package flx
  :straight t
  :defer t)

(use-package prescient
  :straight t
  :defer t
  :config (prescient-persist-mode +1))

(use-package ivy
  :straight t
  :demand t
  :diminish
  :hook (after-init . ivy-mode) ;; another kludge
  :bind ("C-x b" . ivy-switch-buffer)
  :init
  (let ((standard-search-fn
         #'+ivy-prescient-non-fuzzy)
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
  (ivy-set-occur 'swiper-multi 'counsel-ag-occur)
  (ivy-mode t))

;; lots of Doom Emacs hacks
(use-package ivy-prescient
  :straight t
  :commands +ivy-prescient-non-fuzzy
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :init
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient--old-re-builder str)))
  (setq prescient-filter-method
        '(literal regexp initialism fuzzy))
  :config
  (add-to-list 'ivy-sort-functions-alist '(ivy-resume))
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer lsp-ivy-workspace-symbol
               ivy-resume ivy--restore-session counsel-grep counsel-git-grep
               counsel-rg counsel-ag counsel-ack counsel-fzf counsel-pt counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf
               counsel-outline counsel-org-goto counsel-jq)
        ivy-prescient-retain-classic-highlighting t))

;; has to be after ivy in a file
(use-package counsel
  :straight t
  :defer t
  :diminish
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
  (setq ivy-initial-inputs-alist nil)
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
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))
  (counsel-mode t)
  (global-set-key (kbd "M-y") 'counsel-yank-pop))

(use-package counsel-web
  :straight t
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
  :straight t
  :defer t)

(use-package counsel-etags
  :straight t
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
  :straight t
  :defer t)

(use-package counsel-tramp
  :straight t
  :defer t
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

(use-package ivy-avy
  :straight t
  :after ivy)

(use-package ivy-rich
  :straight t
  :after ivy
  ;; :hook (ivy-mode . ivy-rich-mode)
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-project-root-cache-mode +1)
  (ivy-rich-mode t))

(use-package ivy-xref
  :straight t
  :init
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  ;; Use ivy-xref to display `xref.el' results.
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package swiper
  :straight t
  :defer t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("M-s s" . swiper)
         ("M-s m" . swiper-multi)
         ("M-s w" . swiper-thing-at-point))
  :config
  (setq swiper-action-recenter t))

(use-package helpful
  :straight t
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
    ;; (global-set-key (kbd "C-h f") #'helpful-callable)
    ;; (global-set-key (kbd "C-h v") #'helpful-variable)
    ;; (global-set-key (kbd "C-h k") #'helpful-key)
    ;; (global-set-key (kbd "C-h x") #'helpful-command)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (setq counsel-descbinds-function #'helpful-callable)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package ace-link
  :straight t
  :config
  (ace-link-setup-default))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode t))

(use-package smartparens
  :straight t
  :diminish
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :init
  (smartparens-global-strict-mode t)
  :config
  (require 'smartparens-config))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(use-package company
  :straight t
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (prog-mode . company-mode)
  :init
  (global-company-mode)
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  (setq company-minimum-prefix-length 2
        company-show-numbers nil
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never)
  (setq company-backends
        '((company-keywords
           company-capf
           company-gtags
           company-etags
           company-semantic
           company-files)
          (company-abbrev company-dabbrev company-dabbrev-code)
          )))

(use-package company-prescient
  :straight t
  :hook (company-mode . company-prescient-mode)
  :hook (company-prescient-mode . prescient-persist-mode))

(use-package company-posframe
  :straight t
  :diminish
  :hook (company-mode . company-posframe-mode)
  :config
  (setq company-tooltip-minimum-width 40))

(use-package company-quickhelp
  :straight t
  :custom
  (company-quickhelp-delay 3)
  :hook (company-mode . company-quickhelp-mode))

(use-package company-math
  :straight t
  :config
  (setq company-math-disallow-unicode-symbols-in-faces t)
  ;;(add-to-list 'company-backends 'company-math-symbols-latex)
  (append '(company-math-symbols-latex company-math-symbols-unicode)
          company-backends))

(use-package company-org-block
  :straight t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook (org-mode . (lambda ()
                      (add-to-list 'company-backends 'company-org-block)
                      (company-mode +1))))

(use-package company-statistics
  :straight t
  :config
  (company-statistics-mode))

(use-package company-web
  :straight t
  :config
  :hook (nxml-mode . (lambda ()
                       (add-to-list 'company-backends 'company-web-html))))

(use-package flycheck
  :straight t
  :diminish
  :init
  (global-flycheck-mode t))

(use-package flycheck-pos-tip
  :straight t
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(setq flycheck-check-syntax-automatically '(save
                                            idle-change
                                            mode-enabled))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package avy-flycheck
  :straight t
  :after flycheck
  :config
  (avy-flycheck-setup))

(use-package yasnippet
  :straight t
  :diminish
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

(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)

(use-package lsp-mode
  :straight t
  :diminish
  :hook (prog-mode . lsp)
  :hook (lsp-completion-mode . (lambda ()
                                 (remq 'company-capf company-backends)))
  :init
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-headerline-breadcrumb-enable t)
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
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-fontify 'always)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-ui-doc-show-with-cursor t))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)

(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)
                            (setq indicate-empty-lines t)
                            (set-fill-column 72)
                            (auto-fill-mode t)
                            (electric-pair-mode t)
                            (electric-indent-mode t)
                            (abbrev-mode t)
                            (rainbow-delimiters-mode t)))

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

(use-package hl-todo
  :straight t
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
  :straight t
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-flydiff-delay 0.5)  ; default: 0.3
  (setq diff-hl-show-staged-changes nil)
  (global-diff-hl-mode t))

  (use-package expand-region
    :straight t
    :config
    (global-set-key (kbd "C-=") 'er/expand-region))

(use-package ggtags
  :straight t
  :diminish
  :hook (prog-mode . ggtags-mode)
  :config
  (push 'company-gtags company-backends))

(use-package counsel-gtags
  :straight t
  :init
  (setq counsel-gtags-ignore-case t
        counsel-gtags-auto-update t)
  :hook (ggtags-mode . counsel-gtags-mode))

(use-package  highlight-indent-guides
  :straight t
  :diminish
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :hook (after-init . highlight-indent-guides-auto-set-faces)
  :init (setq highlight-indent-guides-method 'character))

(use-package emacs-lisp-mode
  :straight '(:type built-in)
  :defer t
  :hook (emacs-lisp-mode . ggtags-mode)
  :hook (emacs-lisp-mode . semantic-mode)
  :hook (emacs-lisp-mode . auto-compile-mode)
  :config
  (with-eval-after-load 'semantic
    (semantic-default-emacs-lisp-setup)))

(use-package highlight-quoted
  :straight t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-numbers
  :straight t
  :hook (emacs-lisp-mode . highlight-numbers-mode))

(use-package elisp-def
  :straight t
  :diminish
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package macrostep
  :straight t
  :mode (("\\*.el\\'" . emacs-lisp-mode)
         ("Cask\\'" . emacs-lisp-mode)))

(use-package elisp-slime-nav
  :straight t
  :diminish
  :hook (emacs-lisp-mode  . elisp-slime-nav-mode))

(use-package eval-sexp-fu
  :straight t
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

(use-package ielm
  :straight '(:type built-in)
  :defer t
  :hook (ielm-mode . smartparens-strict-mode)
  :hook (ielm-mode . rainbow-delimiters-mode)
  :hook (ielm-mode . eldoc-mode)
  :hook (ielm-mode . highlight-quoted-mode)
  :hook (ielm-mode . highlight-numbers-mode))

(use-package dired
  :straight '(:type built-in)
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask))

(use-package dired-async
  :straight '(:type built-in)
  :config
  :hook (dired-mode . dired-async-mode))

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
  :straight t
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-aux
  :straight '(:type built-in)
  :defer t
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

(use-package fd-dired
  :straight t
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired))

(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode))

(use-package dired-git-info
  :straight t
  :hook (dired-mode . (lambda ()
                        (dired-git-info-mode t))))

(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(use-package all-the-icons-dired
  :straight t
  :if (display-graphic-p)
  :hook (dired-mode . (lambda () (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

(use-package magit
  :straight t
  :defer t
  :hook (magit-post-refresh  . diff-hl-magit-post-refresh)
  :init
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq git-commit-fill-column 72)
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
  :straight t
  :after magit)

(use-package ghub
  :straight t
  :defer t
  :after magit)

(use-package forge
  :straight t
  :defer t
  :after magit)

(use-package orgit
  :straight t
  :after org)

(use-package orgit-forge
  :straight t
  :defer t
  :after forge)

(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

(use-package treesit
  :straight '(:type built-in)
  :init
  (setq-default treesit-font-lock-level 4)
  :config
  (when (boundp 'treesit-extra-load-path)
    (add-to-list 'treesit-extra-load-path "/usr/lib64/")
    (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
    (add-to-list 'treesit-extra-load-path "~/.local/lib/"))
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

(straight-use-package 'tree-sitter-langs)
(straight-use-package 'tree-sitter-indent)

(use-package tree-sitter
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'tree-sitter)
                         (require 'tree-sitter-langs)
                         (require 'tree-sitter-hl)
                         (tree-sitter-indent-mode t)))
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t)
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(lisp-interaction-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(python-mode . python))
  (global-tree-sitter-mode t))

;;; When /not in a rush/, this is a /principle-guided/ way.

(use-package eshell
  :straight '(:type built-in)
  :defer t
  :after company
  :hook (eshell-mode .  smartparens-strict-mode)
  :hook (eshell-mode .  company-mode)
  :hook (eshell-mode .  hide-mode-line-mode)
  :hook (eshell-mode . (lambda () (semantic-mode -1)))
  :bind (:map eshell-mode-map ("C-r"  . counsel-esh-history))
  :init
  (setq eshell-cmpl-cycle-completions nil
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-destroy-buffer-when-process-dies t
        eshell-highlight-prompt t)
  :config
  (setq pcomplete-cycle-completions nil)
  (require 'esh-opt)
  (require 'em-rebind)
  (require 'em-glob)
  (require 'em-prompt)
  (require 'em-ls)
  (require 'em-term)
  (require 'em-unix)
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize)
  (add-hook 'eshell-preoutput-filter-functions  'ansi-color-apply))

(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'vterm)

(use-package esh-help
  :straight t
  :after esh-mode
  :hook (eshell-mode . eldoc-mode)
  :config (setup-esh-help-eldoc))

(use-package shrink-path
  :straight t
  :defer t)

(use-package eshell-did-you-mean
  :straight t
  :defer t
  :after esh-mode
  :config
  (eshell-did-you-mean-setup))

(use-package eshell-syntax-highlighting
  :straight t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package shell-pop
  :straight t
  :defer t)

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

(use-package lsp-pyright
  :straight t
  :config
  (setq lsp-pyright-disable-language-service nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t))

;;; a comint-mode
(use-package python
  :straight '(:type built-in)
  :defer t
  :hook (python-mode . (lambda ()
                         (semantic-mode t)
                         (python-mode t)
                         (elpy-mode t)))
    :interpreter "ipython -i"
  :config
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"
        python-shell-prompt-detect-failure-warning nil))

;;; a major mode
(use-package python-mode
  :straight t
  :defer t
  :hook (python-mode . lsp-deferred)
  :hook (python-mode . elpy-mode)
  :config
  (setq tab-width     4
        python-indent 4)
  (setq indent-tabs-mode nil))

;;; an actual mode which uses it all
(use-package elpy
  :straight t
  :mode "\\.py\\'"
  :bind
  (:map elpy-mode-map
        ("C-M-n" . elpy-nav-forward-block)
        ("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode)
         (elpy-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             '((elpy-company-backend :with
                                                     company-yasnippet company-lsp))))))
  :init
  (elpy-enable)
  :config
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '72))

(use-package python-docstring
  :straight t
  :hook (python-mode . python-docstring-mode))

(use-package ein
  :straight t
  :defer t
  :load-path "lisp"
  :config
  (require 'ob-ein))

;;; defer
(use-package rust-mode
  :straight t
  :defer t)

;;; the actual fancy mode
(use-package rustic
  :straight t
  :mode "\\.rs\\'")

(straight-use-package 'ob-rust)
(straight-use-package 'ob-sml)
(use-package ob-erlang
  :straight '(ob-erlang :type git :host github :repo "xfwduke/ob-erlang")
  :defer t)

(use-package ob-ein
  :straight (:type built-in))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (org . t)
   (shell . t)
   (awk . t)
   (scheme . t)
   (ocaml . t)
   (python . t)
   (gnuplot . t)
   (octave . t)
   (rust . t)
   (haskell . t)
   (sml . t)
   (erlang . t)
   (ein . t)
   ))
