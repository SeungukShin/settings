;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic configure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; home directory
(setq home-emacs-directory (expand-file-name "~/"))

;; temp directory
(setq temp-emacs-directory (concat user-emacs-directory ".cache/"))
(unless (file-exists-p temp-emacs-directory)
  (make-directory temp-emacs-directory t))

;; exec path
(if (eq system-type 'windows-nt)
    (add-to-list 'exec-path (concat home-emacs-directory "AppData/Roaming/emacs/bin/")))

;; proxy
(when nil
  (setq url-proxy-services
	'(("no_proxy" . "^\\(localhost\\|192\\.168\\..*\\)")
	  ("http" . "proxy.com:8080")
	  ("https" . "proxy.com:8080"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "use-package/"))
  (require 'use-package))

;; package
(use-package package
  :config
  (setq package-archives
	       '(("melpa" . "http://melpa.org/packages/")
		 ("gnu" . "http://elpa.gnu.org/packages/")
		 ("marmalade" . "http://marmalade-repo.org/packages/")))
;  (package-refresh-contents)
  (package-initialize))

;; quelpa
(use-package quelpa
  :ensure t
  :config
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :requires (quelpa use-package)
  :ensure t
  :config
  (setq use-package-ensure-function 'quelpa))

;; el-get
(when nil
  (add-to-list 'load-path (concat user-emacs-directory "el-get/"))
  (require 'el-get)
  (setq el-get-dir (concat user-emacs-directory "lisp/"))
  (add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes/"))
  (setq el-get-bundle-sync nil			; allow async. operation
	el-get-allow-insecure t))		; allow local file
(when nil
  ;; load path
  (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
  (let ((default-directory (concat user-emacs-directory "lisp/")))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-loader
(use-package init-loader
  :disabled
  :ensure t
  :config
  (setq init-loader-show-log-after-init t
	init-loader-byte-compile t)
  (init-loader-load (concat user-emacs-directory "init.d/")))

;; reload .emacs
(defun b/reload-dotemacs-file()
  "reload .emacs"
  (interactive)
  (load-file (expand-file-name ".emacs" home-emacs-directory)))
(global-set-key (kbd "C-c C-l") 'b/reload-dotemacs-file)

;; diminish mode name on modeline
(use-package delight
  :ensure t)

;; bind-key
(use-package bind-key
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server
(use-package server
  :ensure t
  :delight (server-buffer-clients "Ⓢ")
  :config
  (server-mode 1)

  ;; use tcp server
;  (setq server-use-tcp t
;	server-host "ip")

  ;; start server
  (unless (server-running-p) (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main frame
(setq inhibit-startup-message t)	; disable startup message
(menu-bar-mode -1)			; hide menu bar
(tool-bar-mode -1)			; hide tool bar
;(scroll-bar-mode -1)			; hide scroll bar
(auto-image-file-mode t)		; show inline image
(global-auto-revert-mode t)		; auto refresh
;(global-linum-mode)			; line number

;; mode line
(line-number-mode t)			; display line number in mode line
(column-number-mode t)			; display column number in mode line
(size-indication-mode t)		; display file size in mode line

;; fonts
(create-fontset-from-fontset-spec
 "-monotype-courier-medium-r-normal-*-*-130-*-*-m-*-fontset-frame,
 korean-ksc5601:-hanyang-gothic-medium-r-normal-*-*-150-*-*-*-*-ksc5601*-0,
 japanese-jisx0208:-ricoh-gothic-medium-r-normal-*-*-150-*-*-*-*-jisx0208*-0,
 japanese-jisx0212:-ricoh-mincho-medium-r-normal-*-*-150-*-*-*-*-jisx0212*-0,
 chinese-big5-1:-dynalab-ming-medium-r-normal-*-*-150-*-*-*-*-big5*-0,
 chinese-big5-2:-dynalab-ming-medium-r-normal-*-*-150-*-*-*-*-big5*-0,
 chinese-gb2312:-zhuhai-song-medium-r-normal-*-*-150-*-*-*-*-gb2312*-*,
 thai-tis620:-*-fixed-medium-r-normal-*-*-160-*-*-*-*-tis620.2529-1,
 vietnamese-viscii-lower:-monotype-courier-medium-r-normal-*-*-130-*-*-*-*-viscii1.1-1,
 lao:-*-fixed-medium-r-normal-*-*-160-*-*-*-*-mulelao-1,
 indian-is13194:-*-fixed-medium-r-normal-*-*-160-*-*-*-*-is13194-devanagari,
 indian-1-column:-*-fixed-medium-r-normal-*-*-160-*-*-*-*-muleindian-1,
 indian-2-column:-*-fixed-medium-r-normal-*-*-160-*-*-*-*-muleindian-2")
(set-fontset-font "fontset-frame" 'latin (font-spec :name "D2Coding" :size 16))
(set-fontset-font "fontset-frame" 'han (font-spec :name "Noto Sans Mono CJK SC" :size 16))
(set-fontset-font "fontset-frame" 'kana (font-spec :name "Noto Sans Mono CJK JP" :size 16))
(set-fontset-font "fontset-frame" 'hangul (font-spec :name "Noto Sans Mono CJK KR" :size 16))
(set-face-font 'default "fontset-frame")
(set-face-attribute 'default nil
		    :font "fontset-frame"
		    :height 120)
(add-to-list 'default-frame-alist '(font . "fontset-frame"))	;; for daemon
(add-to-list 'face-font-rescale-alist '("*" . 1.0))

;; theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;; fill column indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "dark blue")
  (setq-default fill-column 80)
  (add-hook 'after-change-major-mode-hook 'fci-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto saving
(setq backup-inhibited t		; disable backaup
      auto-save-default nil		; disable auto save
      auto-save-list-file-prefix temp-emacs-directory)

;; save last position
(save-place-mode t)
(setq save-place-file (expand-file-name "places" temp-emacs-directory)
      save-place-forget-unreadable-files nil)

;; save recent files
(recentf-mode t)
(setq recentf-save-file (expand-file-name "recentf" temp-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'highlight nil)
(set-face-background 'highlight "#444444")
(set-face-underline  'highlight nil)

;; empty line
(setq indicate-empty-lines t)

;; whitespace
(use-package whitespace
  :ensure t
  :delight (global-whitespace-mode "Ⓦ")
  :config
  (global-whitespace-mode 1)
  (setq whitespace-style
	'(face spaces tabs newline space-mark tab-mark newline-mark))
  (setq whitespace-display-mappings
	'(
	  (space-mark 32 [183] [46])	; space 32 「 」, 183 moddle dot 「·」, 46 full stop 「.」
	  (newline-mark 10 [182 10])	; newline
	  (tab-mark 9 [8614 9] [92 9])	; tab
	  )))

;; highlight current word
(defvar highlight-current-word-color-index
  0
  "highlight color index")
(defvar highlight-current-word-color-list
  (list 'hi-yellow 'hi-pink 'hi-blue 'hi-green 'hi-red)
  "highlight color list")
(defun highlight-current-word()
  "highlight current word"
  (interactive)
  (highlight-regexp (current-word) (nth highlight-current-word-color-index highlight-current-word-color-list))
  (incf highlight-current-word-color-index)
  (when (= highlight-current-word-color-index 4)
    (setq highlight-current-word-color-index 0)))
(defun unhighlight-current-word()
  "unhighlight current word"
  (interactive)
  (unhighlight-regexp (current-word)))

(global-set-key (kbd "C-=") 'highlight-current-word)
(global-set-key (kbd "C--") 'unhighlight-current-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paren
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show paren
(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'mixed)
  (set-face-attribute 'show-paren-match nil
		      :background "blue"))

;; smartparens
(use-package smartparens
  :ensure t
  :delight (smartparens-mode "Ⓟ")
  :config
  (smartparens-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; selection
(cua-mode t)
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "C-SPC")     'set-mark-command)
(global-set-key (kbd "<C-kanji>") 'set-mark-command)

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; which-key
(use-package which-key
  :ensure t
  :delight (which-key-mode "Ⓚ")
  :config (which-key-mode))

;; eldoc
(use-package eldoc
  :ensure t
  :delight (eldoc-mode "Ⓓ"))

;; auto complete
(use-package auto-complete
  :ensure t
  :delight (auto-complete-mode "Ⓐ"))

;; auto-correction
(use-package abbrev
  :delight (abbrev-mode "Ⓑ"))

;; dired
(use-package dired
  :config
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

;; async
(use-package async
  :ensure t
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(use-package helm
  :ensure t
  :defer nil
  :delight (helm-mode "Ⓗ")
  :config
  (helm-mode t)
  (helm-autoresize-mode t)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (setq helm-split-window-inside-p t
	helm-move-to-line-cycle-in-source t
	helm-ff-search-library-in-sexp t
	helm-scroll-amount 8
	helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-ff-file-name-history-use-recentf t)

  (global-unset-key (kbd "C-x c"))

  ;; evil binding
  (with-eval-after-load 'evil
    (evil-leader/set-key
     "x" 'helm-M-x
     "e" 'helm-find-files
     "b" 'helm-mini
     "k" 'kill-buffer))
  :bind
  (;; emacs binding
   ("C-c h"   . helm-command-prefix)
   ("C-h"     . helm-command-prefix)
   ("M-x"     . helm-M-x)
   ("M-y"     . helm-show-kill-ring)
   ("C-x b"   . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-, ,"   . helm-resume)
   :map helm-map
   ("<tab>"   . helm-execute-persistent-action)
   ("C-i"     . helm-execute-persistent-action)
   ("C-z"     . helm-select-action)))

;; helm-ag
(use-package helm-ag
  :requires helm
  :ensure t
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case")
  (setq helm-ag-insert-at-point 'symbol)
  :bind
  (("C-, aa" . helm-do-ag-project-root)
   ("C-, ad" . helm-do-ag)
   ("C-, af" . helm-do-ag-this-file)
   ("C-, ab" . helm-do-ag-buffers)
   ("C-, ao" . helm-ag-pop-stack)))

;; helm-google
(use-package helm-google
  :requires helm
  :ensure t
  :config
  :bind ("C-c g" . helm-google))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cscope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xcscope
(use-package xcscope
  :ensure t)

;; helm-cscope
(use-package helm-cscope
  :requires (xcscope helm)
  :ensure t
  :delight (helm-cscope-mode "Ⓒ")
  :config
  ;; disable auto database update
  (setq cscope-option-do-not-update-database t)

  ;; emacs binding
  (add-hook 'c-mode-common-hook 'helm-cscope-mode)
  (add-hook 'helm-cscope-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-, cs") 'helm-cscope-find-this-symbol)
	      (local-set-key (kbd "C-, cg") 'helm-cscope-find-global-definition)
	      (local-set-key (kbd "C-, cd") 'helm-cscope-find-called-this-function)
	      (local-set-key (kbd "C-, cc") 'helm-cscope-find-calling-this-function)
	      (local-set-key (kbd "C-, ct") 'helm-cscope-find-this-text-string)
	      (local-set-key (kbd "C-, ce") 'helm-cscope-find-egrep-pattern)
	      (local-set-key (kbd "C-, cf") 'helm-cscope-find-this-file)
	      (local-set-key (kbd "C-, ci") 'helm-cscope-find-files-including-file)
	      (local-set-key (kbd "C-, co") 'helm-cscope-pop-mark)))

  ;; evil binding
  (with-eval-after-load 'evil
    (evil-leader/set-key-for-mode 'c-mode
				  "cs" 'helm-cscope-find-this-symbol
				  "cg" 'helm-cscope-find-global-definition
				  "cd" 'helm-cscope-find-called-this-function
				  "cc" 'helm-cscope-find-calling-this-function
				  "ct" 'helm-cscope-find-this-text-string
				  "ce" 'helm-cscope-find-egrep-pattern
				  "cf" 'helm-cscope-find-this-file
				  "ci" 'helm-cscope-find-files-including-file
				  "co" 'helm-cscope-pop-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; style
(setq c-default-style "bsd"
      c-basic-offset 8)
(which-function-mode 1)

;; tab
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq-default indent-tabs-mode t
			  tab-width 8
			  tab-always-indent t)
	    (setq indent-tabs-mode t
		  tab-width 8
		  tab-always-indent t)))

;; auto-correction
(when (featurep 'abbrev)
  (add-hook 'c-mode-common-hook (function (lambda nil (abbrev-mode 1)))))

;; compile
(setq compilation-scroll-output 'first-error)
(eval-when-compile
  (defvar work-directory)
  (defvar backup-directory))

(defun compile-default-directory ()
  (interactive)
  (setq work-directory default-directory)
  (call-interactively 'compile))

(defun compile-work-directory ()
  (interactive)
  (if (boundp 'work-directory) nil
    (setq work-directory default-directory))
  (setq backup-directory default-directory)
  (setq default-directory work-directory)
  (call-interactively 'compile)
  (setq default-directory backup-directory))

(global-set-key "\C-c\C-m" 'compile-default-directory)
(global-set-key "\C-cm"    'compile-work-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t
		  python-indent 8
		  tab-width 8
		  tab-always-indent t)))

;; jupyter
(use-package skewer-mode
  :ensure t)

(use-package ein
  :requires skewer-mode
  :ensure t
  :defer t
  :config
  (setq request-backend 'url-retrieve)
  (setq ein:jupyter-default-server-command "jupyter"
	ein:jupyter-server-args (list "--no-browser")
	ein:jupyter-default-notebook-directory "~/src/jupyter"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
(use-package org
  :ensure t
  :defer t
  :config
  ;; basic
  (add-to-list 'auto-mode-alist		; org mode extension
	       '("\\.org$" . org-mode))
  (setq org-todo-keywords			; todo keyword
	'((sequence "TODO(t)" "NEXT(n)" "DONE(d)")))
  (setq org-hide-leading-stars t
	org-odd-levels-only t)
  (add-to-list 'org-emphasis-alist
	       '("*" (:foreground "red")))

  ;; image
  (setq org-startup-with-inline-images t	; show inline image
	org-image-actual-width			; resize inline image (1/3)
	(/ (display-pixel-width) 3))

  ;; agenda
  (load-library "find-lisp")
  (setq org-agenda-files			; set agenda files
;	(file-expand-wildcards (concat home-emacs-directory "Org/*.org"))
	(find-lisp-find-files (concat home-emacs-directory "Org/Task") "\.org$")
	org-agenda-start-on-weekday 0		; agenda starts on sunday
	org-agenda-span 31)			; number of days for agenda
  (defun b/org-agenda-redo ()
    (interactive)
    (setq org-agenda-files
	  (find-lisp-find-files (concat home-emacs-directory "Org/Task") "\.org$"))
    (org-agenda-redo t))

  ;; babel
  (if (>= emacs-major-version 26)
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
	 (shell . t)
	 (latex . t)
	 (dot . t)
	 (R . nil)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (sh . t)
       (latex . t)
       (dot . t)
       (R . nil))))

  (custom-set-faces '(org-level-1 ((t (:height 1.0))))
		    '(org-level-2 ((t (:height 1.0))))
		    '(org-level-3 ((t (:height 1.0))))
		    '(org-level-4 ((t (:height 1.0))))
		    '(org-level-5 ((t (:height 1.0))))
		    '(org-level-6 ((t (:height 1.0)))))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (setq-default indent-tabs-mode nil
			    tab-width 8
			    tab-always-indent nil)
	      (setq indent-tabs-mode nil
		    tab-width 8
		    tab-always-indent nil)))

  (setq org-startup-indented t)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (visual-line-mode t)
	      (t)))

  ;; beamer
  (add-to-list 'org-latex-packages-alist '("" "listings" nil))
  (setq org-latex-listings t
	org-latex-listings-options '(("basicstyle" "\\tiny")
				     ("frame" "single")
				     ("keywordstyle" "\\color{cyan}")
				     ("stringstyle" "\\color{orange}")
				     ("commentstyle" "\\color{gray}")
				     ("frame" "noney")
				     ("breaklines" "true")))

  ;; binding
  (eval-after-load "org-agenda"
    '(progn
       (define-key org-agenda-mode-map "r" 'b/org-agenda-redo)))
  :bind
  (("\C-ca" . org-agenda)
   ("\C-cl" . org-store-link)
   ("\C-cc" . org-capture)
   ("\C-cb" . org-iswitchb)
   ("\C-cr" . org-remember)))

(use-package org-indent
  :defer t
  :delight
  (org-indent-mode "Ⓘ")
  (visual-line-mode "Ⓥ"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq calendar-week-start-day 0)	; 0:Sunday, 1:Monday

;; korean holidays
(use-package korean-holidays
  :requires holidays
  :ensure t
  :defer t
  :config
  (setq calendar-holidays korean-holidays))

;; calfw
(use-package calfw
  :ensure t
  :defer t)

;; calfw-org
(use-package calfw-org
  :requires (org calfw)
  :ensure t
  :defer t
  :config
  ;; remove warning message from compiler
  (declare-function org-bookmark-jump-unhide "org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :delight
  (markdown-mode "Ⓜ")
  (gfm-mode "Ⓜ")
  :config
  (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
  (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (setq-default indent-tabs-mode nil
			    tab-width 8
			    tab-always-indent nil)
	      (setq indent-tabs-mode nil
		    tab-width 8
		    tab-always-indent nil))))

;; markdown preview
(use-package markdown-preview-mode
  :ensure t
  :defer t
  :config
  (setq browse-url-browser-function 'browse-url-firefox
	browse-url-new-window-flag  t
	browse-url-firefox-new-window-is-tab t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
(use-package auctex
  :ensure t
  :defer t
  :config
  (load "auctex.el" nil t t))

;; latex preview pane
(use-package latex-preview-pane
  :ensure t
  :defer t
  :config
  (latex-preview-pane-enable))

;; doc view
(use-package doc-view
  :ensure t
  :defer t
  :config
  (setq doc-view-resolution 240)
  (setq doc-view-continuous t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; version control system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(use-package magit
  :ensure t
  :defer t)

;; git-gutter
(use-package git-gutter
  :ensure t
  :delight (git-gutter-mode "Ⓖ")
  :config (global-git-gutter-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check & build
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gradle
(use-package gradle-mode
  :ensure t
  :defer t
  :delight (gradle-mode "Ⓡ")
  :config
  (if (eq system-type 'windows-nt)
      (setq gradle-executable-path "\"C:/Program Files/Android/Android Studio/gradle/gradle-4.1/bin/gradle.bat\""))
  (gradle-mode 1))

;; ispell
(use-package ispell
  :ensure t)

;; flyspell
(use-package flyspell
  :requires ispell
  :ensure t
  :delight (flyspell-mode "Ⓕ")
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (if (eq system-type 'windows-nt)
		  (setq ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell"))
	      (flyspell-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m
(use-package w3m
  :ensure t
  :defer t
  :config
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (autoload 'w3m-find-file "w3m" "w3m interface function for local file." t)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
  (autoload 'w3m-weather "w3m-weather" "Display weather report." t)
  (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-use-cookies t)
  (setq w3m-default-display-inline-images t)
  (setq w3m-coding-system 'utf-8
	w3m-file-coding-system 'utf-8
	w3m-file-name-coding-system 'utf-8
	w3m-input-coding-system 'utf-8
	w3m-output-coding-system 'utf-8
	w3m-terminal-coding-system 'utf-8))

;; wanderlust
(use-package wandrlust
  :disabled
  :ensure t
  :defer t
  :config
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
  (autoload 'wl-user-agent-compose "wl-draft" "Compose with Wanderlust." t)

  ;; inline image
  (setq mime-w3m-safe-url-regexp nil
	mime-w3m-display-inline-images t)
  (setq mime-edit-split-message nil)

  ;; directory
  (setq elmo-msgdb-directory (concat user-emacs-directory ".elmo/")
	elmo-cache-directory (concat elmo-msgdb-directory "cache/")
	wl-temporary-file-directory (concat elmo-msgdb-directory "tmp/")
	wl-folders-file (concat user-emacs-directory ".folders"))

  ;; user information
  (setq wl-from "user <user@gmail.com>")

  ;; local maildir
  (setq maildir-path (concat home-emacs-directory "Mail/")
	elmo-maildir-folder-path maildir-path
	elmo-localdir-folder-path maildir-path
	elmo-search-namazu-default-index-path maildir-path
	elmo-archive-folder-path maildir-path)

  ;; imap
  (setq elmo-imap4-default-server "imap.gmail.com"
	elmo-imap4-default-port 993
	elmo-imap4-default-stream-type 'ssl
	elmo-imap4-default-user "user"
	elmo-imap4-default-authenticate-type 'clear
	elmo-imap4-use-modified-utf7 t)

  ;; pop3
  (setq elmo-pop3-default-server "pop.gmail.com"
	elmo-pop3-default-port 995
	elmo-pop3-default-stream-type 'ssl
	elmo-pop3-default-user "user")

  ;; default folder
  (setq wl-default-folder "%Inbox"
	wl-fcc "%[Gmail]/Sent Mail"
	wl-draft-folder "%[Gmail]/Drafts"
	wl-trash-folder "%[Gmail]/Trash"
	wl-quicksearch-folder "%[Gmail]/All Mail"
	wl-spam-folder ".Spam"
	wl-queue-folder ".Queue"
	wl-fcc-force-as-read t
	wl-default-spec "%")
  (setq wl-folder-check-async t)

  ;; prefetch
  (setq wl-summary-incorporate-marks '("N" "U" "!" "A" "F" "$")
	wl-prefetch-threshold nil)

  ;; smtp
  (setq wl-smtp-posting-server "smtp.gmail.com"
	wl-smtp-posting-port 587
	wl-smtp-connection-type 'starttls
	wl-smtp-posting-user "user"
	wl-smtp-authenticate-type "plain"
	wl-local-domain "gmail.com"
	wl-message-id-domain "smtp.gmail.com")

  ;; summary
  (setq wl-auto-select-next 'unread
	wl-summary-auto-sync-marks nil
	wl-summary-width nil
	wl-summary-fix-timezone nil
	wl-summary-weekday-name-lang "en"
	wl-summary-showto-folder-regexp ".Sent.*"
	wl-summary-line-format "%T%P%Y-%M-%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"
	wl-message-mode-line-format (propertize "%f" 'face 'powerline-active1)
	wl-thread-insert-opened t
	wl-thread-open-reading-thread t)

  ;; message
  (setq wl-message-mode-line-format (propertize "%f/%n %F" 'face 'powerline-active1)
	wl-message-ignored-field-list '("^.*:")
	wl-message-visible-field-list
	'("^\\(To\\|Cc\\):"
	  "^Subject:"
	  "^\\(From\\|Reply-To\\):"
	  "^Organization:"
	  "^X-Attribution:"
	  "^\\(Posted\\|Date\\):"
	  "^User-Agent:"
	  )
	wl-message-sort-field-list
	'("^From"
	  "^Organization:"
	  "^X-Attribution:"
	  "^Subject"
	  "^Date"
	  "^To"
	  "^Cc")))

;; epa-file for encryption
(use-package epa-file
  :config
  (epa-file-enable))

;; notmuch
(use-package notmuch
  :ensure t
  :defer t
  :config
  (autoload 'notmuch "notmuch" "notmuch mail" t))

;; gnus
(use-package gnus
  :ensure t
  :defer t
  :config
  ;; user
  (setq user-mail-address "user@gmail.com"
	user-full-name "user")

  ;; server
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
	'((nnimap "user@gmail.com"
		  (nnimap-address "imap.gmail.com")
		  (nnimap-server-port 993)
		  (nnimap-stream ssl)
		  (nnir-search-engine imap))
	  (nnmaildir "Archives"
		     (directory (concat home-emacs-directory "Mail/Local"))
		     (get-new-mail nil)
		     (nnir-search-engine notmuch))))

  (setq smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587
	send-mail-function 'smtpmail-send-it
	message-send-mail-function 'smtpmail-send-it)

  (require 'nnir)
  (setq nnir-notmuch-program "notmuch"
	nnir-notmuch-remove-prefix (concat home-emacs-directory "Mail/Local/"))

  (setq gnus-asynchronous t
	gnus-nntp-server nil
	gnus-fetch-old-headers t
	gnus-auto-select-first nil
	gnus-check-new-newsgroups nil
	gnus-check-bogus-newsgroups nil
	gnus-check-new-news nil
	gnus-read-active-file nil)

  (setq mm-text-html-renderer 'w3m
	mm-inline-text-html-with-images t
	mm-w3m-safe-url-regexp nil)

  ;; summary
  (setq gnus-parameters
	'((".*"
	   (display . all))))

  (setq-default gnus-summary-line-format "%U%R%z %(%-15,15f  %B%s%)\n"
		gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
		gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
		gnus-thread-sort-functions '(gnus-thread-sort-by-date)
		gnus-sum-thread-tree-false-root ""
		gnus-sum-thread-tree-indent " "
		gnus-sum-thread-tree-leaf-with-other "├► "
		gnus-sum-thread-tree-root ""
		gnus-sum-thread-tree-single-leaf "╰► "
		gnus-sum-thread-tree-vertical "│"))

;; mew
(use-package mew
  :disabled
  :ensure t
  :defer t
  :config
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)

  (add-to-list 'exec-path (concat user-emacs-directory "lisp/mew/bin/"))

  ;; read mail menu
  (setq read-mail-command 'mew)

  ;; sending a message command
  (autoload 'mew-user-agent-compose "mew" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'mew-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'mew-user-agent
        'mew-user-agent-compose
        'mew-draft-send-message
        'mew-draft-kill
        'mew-send-hook))

  ;; icon
  (setq mew-icon-directory
	(expand-file-name "etc" (file-name-directory (locate-library "mew.el"))))

  ;; network
  (setq mew-prog-ssl "/usr/bin/stunnel"
	mew-ssl-verify-level 0
	mew-use-cached-passwd t)

  ;; user
  (setq mew-name "user"
	mew-user "user")

  (setq mew-config-alist
	'(
	  (default
	    (mailbox-type imap)
	    (proto "%")
	    ;; imap
	    (imap-server "imap-mail.outlook.com")
	    (imap-ssl-port "993")
	    (imap-user "user")
	    (name "user")
	    (imap-ssl t)
	    (imap-auth t)
	    (imap-size 0)
	    (imap-delete t)
	    (imap-trash-folder "%Deleted")
	    ;; smtp
	    (smtp-server "smtp-mail.outlook.com")
	    (smtp-ssl-port "465")
	    (smtp-user "user")
	    (smtp-ssl t)
	    (smtp-auth t)
	    )))

  ;; encoding
  (setq mew-cs-database-for-encoding
	'(((ascii) nil "7bit" "7bit")
	  (nil utf-8 "base64" "B")))

  ;; html
  (require 'mew-w3m)
  (setq mew-mime-multipart-alternative-list '("Text/Html" "Text/Plain" ".*"))
  (setq mew-use-text/html t))
