;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package magit
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))


;;;; basic ui stuff
(let ((font
       ;;"Input Mono Compressed"
       ;;"Monoid"
       ;; "SF Mono"
       "Input"
       ))
  (set-fontset-font "fontset-default"
                    'unicode
                    `(,font . "iso10646-1"))
  (set-face-attribute 'default nil
                      :family font
                      :height 140
                      :weight 'light
                      ))

(blink-cursor-mode -1)
(global-subword-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq ring-bell-function 'ignore)
(setq major-mode 'text-mode)

(setq-default bidi-display-reordering nil)

;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
(setq set-mark-command-repeat-pop t)

;; delete the selection with a keypress
(delete-selection-mode 1)

;; try to complete at point if already indented
(setq tab-always-indent 'complete)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)

;; enabled change region case commands
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; utf-8 stuff
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(setq require-final-newline t)

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq tab-width 8)                    ;; but maintain correct appearance

;; ediff defaults
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;;; macOS stuff
(when (eq system-type 'darwin)
  (when (eq window-system 'mac)
    ;; Emacs users obviously have little need for Command and Option keys,
    ;; but they do need Meta and Super
    (setq mac-pass-command-to-system nil
          mac-pass-control-to-system nil)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-mouse-wheel-smooth-scroll t)
    (mac-auto-operator-composition-mode)
    (setq mac-frame-tabbing nil)
    (setq mac-mouse-wheel-smooth-scroll nil)
    ;; (defun my/reset-frame ()
    ;;   (make-frame)
    ;;   (select-frame (get-other-frame))
    ;;   (delete-frame (get-other-frame)))
    ;; (add-hook 'after-init-hook 'my/reset-frame)
    )
  (setq ns-command-modifier 'super
	ns-option-modifier 'meta
	ns-use-native-fullscreen t)
  (bind-key "<s-return>" #'toggle-frame-fullscreen)
  (bind-key "s-`" #'other-frame)
  (bind-key "s-c" #'kill-ring-save)
  (bind-key "s-n" #'make-frame)
  (bind-key "s-w" #'delete-frame)
  (bind-key "s-v" #'yank)
  (bind-key "s-s" #'save-buffer)
  (bind-key "s-q" #'save-buffers-kill-emacs)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;; Rectangle-aware commands
(require 'rect)
(defun my/kill-region (beg end &optional region)
  "Kill text between BEG and END.

Use `kill-rectangle' if `rectangle-mark-mode' is set."
  (interactive (list (mark) (point) 'region))
  (if rectangle-mark-mode
      (kill-rectangle beg end)
    (kill-region beg end region)))
(defadvice my/kill-region (before slick-kill activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun my/copy-region (beg end &optional region)
  "Copy text between BEG and END.

Use `copy-rectangle-as-kill' if `rectangle-mark-mode' is set."
  (interactive (list (mark) (point) 'region))
  (if rectangle-mark-mode
      (copy-rectangle-as-kill beg end)
    (kill-ring-save beg end region)))
(defadvice my/copy-region (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(bind-key "C-w" #'my/kill-region)
(bind-key "M-w" #'my/copy-region)

(bind-key "M-n" #'next-error)
(bind-key "M-p" #'previous-error)

(bind-key "C-x \\" #'align-regexp)

(bind-key "C-x C-b" #'ibuffer)

(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)

(use-package winner
  :config
  (winner-mode +1))

;;;; packages

(use-package no-littering
  :demand t
  :config
  (setq auth-sources
        (list (expand-file-name "authinfo.gpg" no-littering-etc-directory)
              "~/.authinfo.gpg")))

(use-package undo-tree
  :config
  (global-undo-tree-mode +1)
  :delight undo-tree-mode)

(use-package epa
  :demand t
  :config
  (setq epa-pinentry-mode 'loopback))

(use-package magit
  :defer t
  :config
  (setq magit-diff-refine-hunk t))
(use-package forge
  :after magit)
(use-package magit-todos
  :disabled t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-require-colon nil)
  ;;(define-key magit-todos-section-map "j" nil)
  )
(use-package magithub
  :disabled t
  :after (magit no-littering)
  :preface
  ;; Magithub is not well-behaved, so this needs to be set early
  (setq magithub-dir (concat no-littering-var-directory "magithub/"))
  :config
  (magithub-feature-autoinject 'all))

(use-package leuven-theme
  :disabled t
  :config
  (load-theme 'leuven t)
  (global-hl-line-mode +1)
  (add-hook 'after-init-hook (lambda () (set-face-attribute 'hl-line nil :underline nil)))
  )

(use-package solarized-theme
  :disabled t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-light t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic nil)
  (load-theme 'doom-solarized-light t))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root))

(use-package smart-mode-line
  :disabled t
  :config
  (sml/setup))

(use-package zerodark-theme
  :disabled t
  :config
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format))

(use-package beacon
  :disabled t
  :config
  (beacon-mode +1))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  ;; :bind (([remap next-error] . flycheck-next-error)
  ;;        ([remap previous-error] . flycheck-previous-error))
  :config
  (delq 'emacs-lisp flycheck-checkers)
  (delq 'emacs-lisp-checkdoc flycheck-checkers)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (add-to-list 'flycheck-disabled-checkers 'emacs-lisp)
  ;; (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  ;;(add-hook 'prog-mode-hook #'flycheck-mode)
  )
(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))

(use-package company
  :delight company-mode
  :bind (("<M-tab>" . company-manual-begin))
  :config
  (setq company-idle-delay nil)
  (setq company-require-match 'never)
  (global-company-mode +1))
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple)
        company-box-icons-elisp
        (list (all-the-icons-material "functions"                  :height 0.8 :face 'all-the-icons-red)
              (all-the-icons-material "check_circle"               :height 0.8 :face 'all-the-icons-blue)
              (all-the-icons-material "stars"                      :height 0.8 :face 'all-the-icons-orange)
              (all-the-icons-material "format_paint"               :height 0.8 :face 'all-the-icons-pink))
        company-box-icons-lsp
        `((1  . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
          (2  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
          (3  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
          (4  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
          (5  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
          (6  . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
          (7  . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
          (8  . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
          (9  . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
          (10 . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
          (11 . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
          (12 . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
          (13 . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
          (14 . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
          (15 . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
          (16 . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
          (17 . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
          (18 . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
          (19 . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
          (20 . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
          (21 . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
          (22 . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
          (23 . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
          (24 . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
          (25 . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red)))))

(use-package deadgrep
  :bind ("<f5>" . deadgrep))

;;;; expand-region
(use-package expand-region
  :bind (("M-m"   . er/expand-region)
         ("M-S-m" . er/contract-region)))

;;;; change-inner
(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

;;;; ivy
(use-package ivy
  :hook ((after-init . ivy-mode))
  :delight ivy-mode
  :bind (("C-r" . ivy-resume))
  :config
  ;; Don't use ^ as initial input
  (setq ivy-initial-inputs-alist nil)
  ;; highlight til EOL
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (setq magit-completing-read-function #'ivy-completing-read))
(use-package counsel
  :bind (("C-h a"                          . counsel-apropos)
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap find-library]             . counsel-find-library)
         ("C-c i"                          . counsel-imenu)
         ([remap org-capture]              . counsel-org-capture)
         ([remap yank-pop]                 . counsel-yank-pop)
         ("C-s"                            . counsel-grep-or-swiper))
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))
(use-package swiper)
(use-package smex
  :config
  (smex-initialize))
(use-package ivy-posframe
  :disabled t
  :after ivy
  :hook (ivy-mode . ivy-posframe-enable)
  :preface
  ;; This function searches the entire `obarray' just to populate
  ;; `ivy-display-functions-props'. There are 15k entries in mine! This is
  ;; wasteful, so...
  (advice-add #'ivy-posframe-setup :override #'ignore)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height)
          (internal-border-width . 10)))
  ;; ... let's do it manually instead
  (unless (assq 'ivy-posframe-display-at-frame-bottom-left ivy-display-functions-props)
    (dolist (fn (list 'ivy-posframe-display-at-frame-bottom-left
                      'ivy-posframe-display-at-frame-center
                      'ivy-posframe-display-at-point
                      'ivy-posframe-display-at-frame-bottom-window-center
                      'ivy-posframe-display
                      'ivy-posframe-display-at-window-bottom-left
                      'ivy-posframe-display-at-window-center
                      ))
      (push (cons fn '(:cleanup ivy-posframe-cleanup)) ivy-display-functions-props)))
  ;; default to posframe display function
  (setf (alist-get t ivy-display-functions-alist) #'ivy-posframe-display-at-frame-bottom-window-center)

  ;; posframe doesn't work well with async sources
  (dolist (fn '(swiper counsel-ag counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-display-functions-alist) #'ivy-display-function-fallback)))

;;;; projectile
(use-package projectile
  :defer t
  :delight projectile-mode
  :bind-keymap (("C-c p" . projectile-command-map))
  :bind (("C-x m" . projectile-run-eshell))
  :config
  (add-to-list 'projectile-project-search-path "~/Source")
  (add-to-list 'projectile-project-search-path "~/Work")
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recentf)
  (projectile-mode +1))

(use-package smartparens
  :defer 1
  :bind (("C-k"   . sp-kill-hybrid-sexp)
         ("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-M-d" . sp-splice-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ("C-M-p" . sp-forward-slurp-sexp)
         ("C-M-o" . sp-forward-barf-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-paren-mode -1)
  (show-smartparens-global-mode +1)
  :delight (smartparens-mode show-smartparens-mode))

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success))))
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package visual-regexp
  :bind (("M-%"   . vr/query-replace)
         ("C-M-%" . vr/query-replace)))

(use-package helpful
  :defer t
  ;; :bind (("C-h f" . helpful-callable)
  ;;        ("C-h k" . helpful-key)
  ;;        ("C-h v" . helpful-variable))
  )

(use-package which-key
  :delight which-key-mode
  :config
  (which-key-mode +1))

(use-package nav-flash
  :hook ((imenu-after-jump         . nav-flash-show)
         (counsel-grep-post-action . nav-flash-show)
         (save-place-find-file     . nav-flash-show)))

;;;; haskell
(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'"                . literate-haskell-mode)
         ("\\.cabal\\'"              . haskell-cabal-mode))
  :config
  (setq haskell-process-show-overlays nil)
  ;; (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (setq haskell-process-args-ghci
        '("-ferror-spans" "-fshow-loaded-modules"))
  (setq haskell-process-args-cabal-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (setq haskell-process-args-stack-ghci
        '("--ghci-options=-ferror-spans -fshow-loaded-modules"
          "--no-build" "--no-load"))
  (setq haskell-process-args-cabal-new-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  :delight interactive-haskell-mode)
(use-package intero
  :after haskell-mode
  :delight intero-mode
  :config
  (add-hook 'haskell-mode-hook #'intero-mode))
;; (use-package dante
;;   :after haskell-mode
;;   :config
;;   (add-hook 'haskell-mode-hook #'dante-mode)
;;   (add-hook 'dante-mode-hook
;;             #'(lambda ()
;;                 (flycheck-add-next-checker 'haskell-dante
;;                                            '(warning . haskell-hlint))))
;;   :delight dante-mode)
;; (use-package attrap)

;;(use-package agda2-mode
;;  :load-path "/usr/local/share/emacs/site-lisp/agda/")

;;;; C/C++
(use-package rtags
  :disabled t
  :load-path "/usr/local/share/emacs/site-lisp/rtags/"
  :config
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (setq rtags-display-result-backend 'ivy)
  (add-hook 'c-mode-hook #'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook #'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook #'rtags-start-process-unless-running)
  (use-package flycheck-rtags
    :load-path "/usr/local/share/emacs/site-lisp/rtags/"
    :config
    (defun my-flycheck-rtags-setup ()
      "Configure flycheck-rtags for better experience."
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-check-syntax-automatically nil)
      (setq-local flycheck-highlighting-mode nil))
    (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)))

(use-package lsp
  :hook ((c-mode    . lsp)
         (c++-mode  . lsp)
         (objc-mode . lsp))
  :init
  (require 'lsp-clients)
  (when (equal system-type 'darwin)
    (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  (lsp-clients-register-clangd)
  (defun lsp-clients--clangd-command ()
    "Generate the language server startup command."
    (if (locate-dominating-file (buffer-file-name) ".bloomberg")
        '("mmit-docker-clangd")
      `(,lsp-clients-clangd-executable ,@lsp-clients-clangd-args)))
  ;;(setq lsp-print-io t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-prefer-flymake nil)
  ;; :config
  ;; (use-package lsp-clangd
  ;;   :disabled t
  ;;   :init
  ;;   (when (equal system-type 'darwin)
  ;;     (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  ;;   :hook ((c-mode    . lsp-clangd-c-enable)
  ;;          (c++-mode  . lsp-clangd-c++-enable)
  ;;          (objc-mode . lsp-clangd-objc-enable)))
  )
(use-package lsp-ui
  ;; :disabled t
  ;;:hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil))
(use-package ccls
  :disabled t
  :config
  (setq ccls-args '("-log-file=/Users/eseidel13/.emacs.d/var/ccls/ccls.log")))
(use-package cquery
  :disabled
  :hook ((c-mode    . lsp-cquery-enable)
         (c++-mode  . lsp-cquery-enable)
         (objc-mode . lsp-cquery-enable))
  :config
  (setq cquery-extra-args '("--log-file=~/.emacs.d/var/cquery/cquery.log"))
  ;; (setq cquery-extra-init-params '(:compilationDatabaseDirectory "cmake.bld/Linux"))
  )

(use-package eglot
  :disabled t
  :hook ((c-mode    . eglot-ensure)
         (c++-mode  . eglot-ensure)
         (objc-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode objc-mode) . ("make" "d-clangd"))))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '(nil "^;;;; \\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression '(nil "^(use-package \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(use-package rust-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package groovy-mode
  :mode (("Jenkinsfile'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode)))

(use-package dockerfile-mode
  :mode (("Dockerfile'" . dockerfile-mode)))

(use-package docker-tramp
  :defer t)

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(use-package org
  :bind (("C-c a"   . org-agenda)
         ("C-c c"   . org-capture)
         ("C-c s l" . org-store-link))
  :config
  (setq org-startup-indented t)
  (setq org-agenda-files '("~/Work/org/notes.org"
                           "~/Work/org/todo.org"))
  (setq org-default-notes-file "~/Work/org/notes.org")
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-src-fontify-natively t)
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-log-refile 'time)
  (setq org-todo-keywords '((sequence "SOMEDAY(s)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,org-default-notes-file "Tasks")
           "* TODO %?\n%U\n%i\n%a"))))
(use-package ox-reveal
  :after org)

;;;; misc

(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))
(bind-key "M-v" #'scroll-half-page-down)
(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))
(bind-key "C-v" #'scroll-half-page-up)

(use-package misc
  :bind (("M-z" . zap-up-to-char)))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package autorevert :delight auto-revert-mode)
(use-package outline    :delight outline-minor-mode)
(use-package reveal     :delight reveal-mode)
(use-package eldoc      :delight eldoc-mode)

(use-package ws-butler
  :config
  (ws-butler-global-mode +1)
  :delight ws-butler-mode)

(use-package avy
  :bind (("M-g w" . avy-goto-word-1)))
(use-package ace-window
  :disabled t
  :bind (("C-x o"   . ace-window)
         ("C-x C-o" . ace-window))
  :config
  (setq aw-dispatch-always t))

(use-package switch-window
  ;; :disabled t
  :bind (("C-x o"   . switch-window)
         ("C-x C-o" . switch-window)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
