;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check statup time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; startup time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s seconds with %d garbage collections."
   (emacs-init-time)
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic configure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directories
(setq home-dir (expand-file-name "~/")
      user-dir user-emacs-directory
      temp-dir (concat user-dir ".cache/")
      elpa-dir (concat user-dir "elpa/"))
(unless (file-exists-p temp-dir)
  (make-directory temp-dir t))

;; exec path
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path (concat home-dir "AppData/Roaming/emacs/bin/")))
(when (eq system-type 'darwin)
  (add-to-list 'exec-path (concat home-dir "bin/"))
  (add-to-list 'exec-path "/opt/homebrew/bin/"))

;; proxy
(when nil
  (setq url-proxy-services
	'(("no_proxy" . "^\\(localhost\\|192\\.168\\..*\\)")
	  ("http" . "proxy.com:8080")
	  ("https" . "proxy.com:8080"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
;(setq use-package-compute-statistics t)

;; use package
(eval-when-compile
  (unless (require 'use-package nil t)
    (straight-use-package 'use-package)))

;; load path
(when nil
  (let ((default-directory (concat user-dir "lisp/")))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

;; reload .emacs
(defun s/reload-init-file ()
  "reload .emacs"
  (interactive)
  (when (y-or-n-p "Rebuild Packages?")
    (byte-recompile-directory user-emacs-directory 0))
  ;; early-init.el
  (when (file-exists-p (expand-file-name "early-init.elc" user-dir))
    (delete-file (expand-file-name "early-init.elc" user-dir))
    (byte-compile-file (expand-file-name "early-init.el" user-dir))
    (load-file (expand-file-name "early-init.elc" user-dir)))
  ;; init.el
  (when (file-exists-p (expand-file-name "init.elc" user-dir))
    (delete-file (expand-file-name "init.elc" user-dir))
    (byte-compile-file (expand-file-name "init.el" user-dir))
    (load-file (expand-file-name "init.elc" user-dir))))
;(global-set-key (kbd "C-c l") 's/reload-init-file)
(global-set-key (kbd "C-c r") 'restart-emacs)

;; benchmark
(use-package benchmark-init
  :disabled
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server
(use-package server
  :if (not (display-graphic-p))
  :config
  (server-mode 1)

  ;; use tcp server
;  (setq server-use-tcp t
;	server-host "ip")

  ;; start server
  (unless (server-running-p) (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup frame
(setq default-frame-alist '((tool-bar-lines . 0)
			    (menu-bar-lines . 0)
			    (vertical-scroll-bars)
			    (mouse-color . "blue")
			    (line-spacing . 0)
			    (left-fringe . 8)
			    (right-fringe . 13)
			    (internal-border-width . 10)
			    ;(fullscreen . maximized)
			    ;(background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
(menu-bar-mode t)

;; disable useless information
(setq inhibit-startup-message t			; disable startup message
      display-time-default-load-average nil)

;; auto refresh
(setq auto-revert-avoid-polling t
      auto-revert-interval 5
      auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; line number
(global-display-line-numbers-mode 1)
(setopt display-line-numbers-width 3)		; set a minimum width

;; buffer
(auto-image-file-mode t)			; display inline image
(setopt x-underline-at-descent-line nil		; pretty underlines
	switch-to-buffer-obey-display-actions t	; make switching buffers more consistent
	show-trailing-whitespace t		; underline trailing spaces
	indicate-buffer-boundaries 'left	; show buffer top and bottom in the margin
	inhibit-splash-screen t			; turn off the welcome screen
	sentence-end-double-space nil
	truncate-lines t)			; turn off word wrap

;; fonts
(when (display-graphic-p)
  (set-fontset-font "fontset-default" 'latin
 		    (font-spec :name "Inconsolata Nerd Font" :size 32 :height 32))
  (set-fontset-font "fontset-default" 'han
 		    (font-spec :name "Inconsolata Nerd Font" :size 32 :height 32))
  (set-fontset-font "fontset-default" 'kana
 		    (font-spec :name "Noto Sans Mono CJK JP" :size 32 :height 32))
  (set-fontset-font "fontset-default" 'hangul
 		    (font-spec :name "D2Coding" :size 32 :height 32))
  (set-frame-font "fontset-default" nil t))

;; theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dark+ t)

  (doom-themes-visual-bell-config)		; enable flashing mode-line on errors
  (doom-themes-org-config))			; org-mode's native fontification.

;; mode line theme
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 16))

;; mode line
(setq line-number-mode t			; display line number in mode line
      column-number-mode t			; display column number in mode line
      size-indication-mode t			; display file size in mode line
      display-time-format "%F %a %T"		; display time in mode line
      display-time-interval 1)
(display-time-mode)

;; right-click menu
(when (display-graphic-p)
  (context-menu-mode))

;; scroll
(pixel-scroll-precision-mode)                   ; smooth scrolling
(setopt mouse-wheel-tilt-scroll t
	mouse-wheel-flip-direction t)
(blink-cursor-mode -1)                          ; Steady cursor

;; tab bar
(tab-bar-mode 1)                                ; enable tab bar
(setq tab-bar-show 1                            ; hide bar if <= 1 tabs open
      tab-bar-close-button-show t               ; show tab close / X button
      tab-bar-new-tab-choice "*dashboard*"      ; buffer to show in new tabs
      tab-bar-tab-hints t                       ; show tab numbers
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(set-face-attribute 'tab-bar-tab nil
                    :foreground "#f0f0f0"
                    :background "#672179")
(set-face-attribute 'tab-bar-tab-inactive nil
                    :foreground "#339cda"
                    :background "#1e1e1e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default encoding
(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8)
(set-file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "" 'utf-8-unix)

;; locale
(setq system-time-locale "C")

;; input method
(setq default-input-method "korean-hangul")
(global-set-key [?\S- ] 'toggle-input-method)

;; default indent
(setq indent-tabs-mode t
      tab-width 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (display-graphic-p)
  ;; auto save session
  (desktop-save-mode 1)

  ;; save recent files
  (recentf-mode t)
  (setq recentf-save-file (expand-file-name "recentf" temp-dir)
	recentf-auto-cleanup 'never))

;; auto saving
(setq backup-inhibited t		; disable backaup
      auto-save-default nil		; disable auto save
      auto-save-list-file-prefix temp-dir)

;; save last position
(save-place-mode t)
(setq save-place-file (expand-file-name "places" temp-dir)
      save-place-forget-unreadable-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mini buffer and completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'yes-or-no-p 'y-or-n-p)       ; yes/no to y/n
(savehist-mode)                         ; save history of minibuffer
(setopt enable-recursive-minibuffers t  ; use the minibuffer whilst in the minibuffer
	completion-cycle-threshold 1    ; cycles candidates with tab
	completions-detailed t          ; show annotations
	tab-always-indent 'complete     ; tab tries to complete first, otherwise indent
	completion-styles '(basic initials substring) ; Different styles to match input to candidates
	completion-auto-help 'always    ; open completion always
	completions-max-height 20
	completions-detailed t
	completions-format 'one-column
	completions-group t
	completion-auto-select 'second-tab
	completion-auto-select t)       ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

;; help message for minibuffer commands
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; fancy completion-at-point functions
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major mode
(use-package emacs
  :config
  ;; use treesitter enabled mode than normal mode
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          ;(typescript-mode . typescript-ts-mode)
          (typescript-mode . tsx-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

(use-package rust-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(use-package coverlay
  :straight (coverlay
	     :type git
	     :host github
	     :repo "twada/coverlay.el")
  :ensure t)

(use-package css-in-js-mode
  :straight (css-in-js-mode
	     :type git
	     :host github
	     :repo "orzechowskid/tree-sitter-css-in-js")
  :ensure t)

(use-package origami
  :straight (origami
	     :type git
	     :host github
	     :repo "gregsexton/origami.el")
  :ensure t)

(use-package tsx-mode
  :straight (tsx-mode
	     :type git
	     :host github
	     :repo "orzechowskid/tsx-mode.el")
  :requires (coverlay css-in-js-mode origami)
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; folding
(use-package hideshow
  :hook
  ((prog-mode . hs-minor-mode)
   (org-mode . hs-minor-mode))
  :bind
  (("<C-tab>"     . hs-toggle-hiding)
   ("<backtab>"   . hs-hide-all)
   ("<C-backtab>" . hs-show-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; version control system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(use-package magit
  :ensure t
  :bind (("C-x g s" . magit-status)
	 ("C-x g l" . magit-log-all)))

;; git-gutter
(use-package git-gutter
  :config (global-git-gutter-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :hook ((org-mode . flyspell-mode))      ; spell checking!
  :config
  (setq org-directory (concat home-dir "org/")
	org-agenda-dir (concat home-dir "org/")
	org-agenda-files (directory-files-recursively org-agenda-dir "\\.org$")
	org-agenda-show-all-dates nil		; hide empty day
	org-agenda-start-on-weekday 0		; agenda starts on sunday
	org-agenda-span 31			; number of days for agenda
	org-agenda-time-leading-zero t
	org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-timestamp-if-done t
	org-agenda-todo-ignore-scheduled 'future
	org-agenda-todo-ignore-deadlines nil
	org-agenda-todo-ignore-timestamp t
	org-agenda-tags-todo-honor-ignore-options t)

  ;; tags
  (setq org-tag-alist '(
			;; locale
			(:startgroup)
			("home" . ?h)
			("work" . ?w)
			("school" . ?s)
			(:endgroup)
			(:newline)
			;; scale
			(:startgroup)
			("one-shot" . ?o)
			("project" . ?j)
			("tiny" . ?t)
			(:endgroup)
			;; misc
			("meta")
			("review")
			("reading")))

  ;; todo keyword
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "DROP(p)"))
	org-hide-leading-stars t
	org-startup-indented t
	org-startup-folded t)

  ;; heading font size
  (custom-set-faces '(org-level-1 ((t (:height 1.0))))
		    '(org-level-2 ((t (:height 1.0))))
		    '(org-level-3 ((t (:height 1.0))))
		    '(org-level-4 ((t (:height 1.0))))
		    '(org-level-5 ((t (:height 1.0))))
		    '(org-level-6 ((t (:height 1.0)))))

  ;; citation
  (require 'oc-csl)
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
             ((agenda)
              (todo)))
            ("w" "Work" agenda ""
             ((org-agenda-files '("work.org"))))))

    ;; Advanced: Custom link types
    ;; This example is for linking a person's 7-character ID to their page on the
    ;; free genealogy website Family Search.
    (setq org-link-abbrev-alist
	  '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

    (setq org-display-inline-images t
	  org-redisplay-inline-images t
	  org-startup-with-inline-images t)
    (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t))) ; this line activates dot

    :bind (:map global-map
		("C-c l s" . org-store-link)          ; Mnemonic: link → store
		("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  )

(use-package org-roam
  :disabled
  :ensure t
  :init
  (setq org-roam-directory "~/org/"
	org-roam-index-file "~/org/index.org")

  :config
  (org-roam-db-autosync-mode)
  ;; Dedicated side window for backlinks
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer))))

(use-package org-transclusion
  :ensure t

  :bind (("C-c i a" . org-transclusion-add)
	 (:map org-transclusion-map
               ("C-c i A" . org-transclusion-add-all)
               ("C-c i r" . org-transclusion-remove)
               ("C-c i R" . org-transclusion-remove-all)
               ("C-c i T" . org-transclusion-activate)
               ("C-c i D" . org-transclusion-deactivate)
               ("C-c i d" . org-transclusion-detach))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun s/tmux (command &rest args)
  "Execute COMMAND in tmux"
  (let ((bin (executable-find "tmux")))
    (unless bin
      (error "Could not find tmux executable"))
    (let* ((args (mapcar #'shell-quote-argument (delq nil args)))
           (cmdstr (format "%s %s" bin (if args (apply #'format command args) command)))
           (output (get-buffer-create " *tmux stdout*"))
           (errors (get-buffer-create " *tmux stderr*"))
           code)
      (unwind-protect
          (if (= 0 (setq code (shell-command cmdstr output errors)))
              (with-current-buffer output
                (setq +tmux-last-command `(,(substring cmdstr (+ 1 (length bin))) ,@args))
                (buffer-string))
            (error "[%d] tmux $ %s (%s)"
                   code
                   (with-current-buffer errors
                     (buffer-string))
                   cmdstr))
        (and (kill-buffer output)
             (kill-buffer errors))))))

(defun s/tmux/run (command &optional noreturn)
  "Run COMMAND in tmux. If NORETURN is non-nil, send the commands as keypresses
but do not execute them."
  (interactive
   (list (read-string "tmux $ ")
         current-prefix-arg))
  (s/tmux (concat "send-keys C-u "
                 (shell-quote-argument command)
                 (unless noreturn " Enter"))))

(defun s/tmux-execute-current-line-or-region ()
  "execute text of current line or region in tmux"
  (interactive)
  (let* ((current-line (buffer-substring
                        (save-excursion
                          (beginning-of-line)
                          (point))
                        (save-excursion
                          (end-of-line)
                          (point))))
         (buf (current-buffer))
         (command (string-trim
               (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 current-line))))
    (message command)
    (if (string-prefix-p "$ " command)
	(s/tmux (concat "send-keys C-u "
			(shell-quote-argument (replace-regexp-in-string "^$ " "" command))
			" Enter"))
      (s/tmux command))))
(global-set-key (kbd "C-c t") 's/tmux-execute-current-line-or-region)

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda ()
			(set (make-local-variable 'buffer-face-mode-face)
			     '(:family "Inconsolata Nerd Font" :size 16 :height 16))
			'(add-text-properties '(line-spacing 0 line-height 1))
			(buffer-face-mode t)
			(setq-default line-spacing 0.0)
			(setq line-spacing 0.0
			      line-height 1.0)))
  :config
  (if (file-exists-p "/bin/zsh")
      (setq vterm-shell "/bin/zsh")
    (setq vterm-shell "/bin/bash"))
  (defun s/vterm-execute-current-line-or-region ()
    "Insert text of current line or region in vterm and execute."
    (interactive)
    (let* ((current-line (buffer-substring
                          (save-excursion
                            (beginning-of-line)
                            (point))
                          (save-excursion
                            (end-of-line)
                            (point))))
           (buf (current-buffer))
           (raw (string-trim
                     (if (use-region-p)
                         (buffer-substring (region-beginning) (region-end))
                       current-line)))
	   (command (replace-regexp-in-string "^$ " "" raw)))
      (unless (get-buffer vterm-buffer-name)
        (vterm))
      (display-buffer vterm-buffer-name t)
      (switch-to-buffer-other-window vterm-buffer-name)
      (vterm--goto-line -1)
      (vterm-send-string command t)
      (vterm-send-return)
      (switch-to-buffer-other-window buf)
      (when (featurep 'beacon)
        (beacon-blink))))
  :bind (("C-c v" . s/vterm-execute-current-line-or-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package nerd-icons
  :ensure t)

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("o" "~/org/"                      "org")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; fix for Listing directory failed but ‘access-file’ worked
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/opt/homebrew/bin/gls"))
  ;(dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-header-line-height '(16 . 16)
	dirvish-header-line-format
	'(:left (path) :right (free-space))
  	dirvish-mode-line-height '(16 . 16)
	dirvish-mode-line-format
	'(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (add-to-list
   'dirvish-open-with-programs
   '(("docx" "xlsx" "pptx") . ("open" "%f")))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("<left>"  . dired-up-directory)
   ("<right>" . dired-find-file)
   ("u"       . dired-up-directory)
   ("o"       . dired-find-file-other-window)
   ("a"       . dirvish-quick-access)
   ("f"       . dirvish-file-info-menu)
   ("y"       . dirvish-yank-menu)
   ("N"       . dirvish-narrow)
   ("^"       . dirvish-history-last)
   ("h"       . dirvish-history-jump) ; remapped `describe-mode'
   ("s"       . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"       . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB"     . dirvish-subtree-toggle)
   ("M-f"     . dirvish-history-go-forward)
   ("M-b"     . dirvish-history-go-backward)
   ("M-l"     . dirvish-ls-switches-menu)
   ("M-m"     . dirvish-mark-menu)
   ("M-t"     . dirvish-layout-toggle)
   ("M-s"     . dirvish-setup-menu)
   ("M-e"     . dirvish-emerge-menu)
   ("M-j"     . dirvish-fd-jump)))

;; beginning of line
(defun s/beginning-of-line()
  "beginning of line like vscode"
  (interactive)
  (setq prev-point (point))
  ;(message "first: %s" (point))
  (beginning-of-line-text)
  ;(message "second: %s" (point))
  (if (eq prev-point (point))
    (beginning-of-line)))
(global-set-key (kbd "C-a") 's/beginning-of-line)

(use-package notmuch
  :disabled
  :defer t
  :commands notmuch)
