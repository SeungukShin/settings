;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server
(require 'server)

;; use tcp server
;(setq server-use-tcp t
;      server-host "ip")

;; start server
;(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; default directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; home directory
(setq home-emacs-directory (expand-file-name "~/"))

;; temp directory
(setq temp-emacs-directory (concat user-emacs-directory ".cache/"))
(unless (file-exists-p temp-emacs-directory)
  (make-directory temp-emacs-directory t))

;; load path
;(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
;(let ((default-directory (concat user-emacs-directory "lisp/")))
;  (normal-top-level-add-to-load-path '("."))
;  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package
;(require 'package)
;(add-to-list 'package-archives
;	     '(("melpa" . "http://melpa.org/packages/")
;	       ("gnu" . "elpa.gnu.org/packages/")
;	       ("marmalade" . "marmalade-repo.org/packages/")))
;(package-refresh-contents)
;(package-initialize)

;; el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/"))

(require 'el-get)

(setq el-get-dir (concat user-emacs-directory "lisp/"))
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes/"))

(setq el-get-bundle-sync nil		; allow async. operation
      el-get-allow-insecure t)		; allow local file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-loader
;(el-get-bundle init-loader
;  (require 'init-loader nil t)
;  (setq init-loader-show-log-after-init t
;	init-loader-byte-compile t)
;  (init-loader-load (concat user-emacs-directory "init.d/")))

;; reload .emacs
(defun b/reload-dotemacs-file()
  "reload .emacs"
  (interactive)
  (load-file (expand-file-name ".emacs" home-emacs-directory)))
(global-set-key (kbd "C-c C-l") 'b/reload-dotemacs-file)

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
;(global-linum-mode)                    ; line number

;; mode line
(line-number-mode t)			; display line number in mode line
(column-number-mode t)			; display column number in mode line
(size-indication-mode t)		; display file size in mode line

;; fonts
(set-fontset-font "fontset-default" 'latin (font-spec :name "D2Coding"))
(set-fontset-font "fontset-default" 'hangul (font-spec :name "D2Coding"))
(set-face-attribute 'default nil
		    :font "fontset-default"
		    :height 120)
(create-fontset-from-fontset-spec
"-*-fixed-medium-r-normal-*-16-*-*-*-c-*-fontset-frame,
latin-jisx0201:-unknown-D2Coding-normal-normal-*-*-*-*-*-d-0-iso10646-1,
korean-ksc5601:-unknown-D2Coding-normal-normal-*-*-*-*-*-d-0-iso10646-1,
japanese-jisx0208:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1983-*,
japanese-jisx0208-1978:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1978-*")
(set-fontset-font "fontset-frame" 'latin (font-spec :name "D2Coding"))
(set-fontset-font "fontset-frame" 'hangul (font-spec :name "D2Coding"))
(add-to-list 'default-frame-alist '(font . "fontset-frame"))	;; for daemon

;; theme
(el-get-bundle monokai-theme
  :url "https://github.com/oneKelvinSmith/monokai-emacs"
  (load-theme 'monokai t))

;; fill column indicator
(el-get-bundle fill-column-indicator)
(when (require 'fill-column-indicator nil t)
  (setq fci-rule-width 1)
  (setq fci-rule-color "dark blue")
  (setq-default fill-column 80)
  (add-hook 'after-change-major-mode-hook 'fci-mode))

;; diminish mode name on modeline
(el-get-bundle delight
  (require 'delight)
  (delight 'server-buffer-clients "Ⓢ" 'server)
  (delight 'auto-complete-mode "Ⓐ" 'auto-complete)
  (delight 'abbrev-mode "Ⓑ" 'abbrev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default encoding
(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8)

;; locale
(setq system-time-locale "C")

;; input method
(setq default-input-method "korean-hangul")
(global-set-key [?\S- ] 'toggle-input-method)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto saving
(setq backup-inhibited t                ; disable backaup
      auto-save-default nil             ; disable auto save
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
(show-paren-mode t)
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "blue")

;; smartparens
(el-get-bundle smartparens)
(when (require 'smartparens-config nil t)
  (smartparens-global-mode t)

  (when (require 'delight nil t)
    (delight 'smartparens-mode "Ⓟ" 'smartparens)))

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
(el-get-bundle which-key)
(when (require 'which-key)
  (which-key-mode)

  (when (require 'delight nil t)
    (delight 'which-key-mode "Ⓚ" 'which-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; async
(el-get-bundle emacs-async)

;; helm
(el-get-bundle helm)
(when (and (require 'helm-config nil t)
	   (require 'helm nil t))
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

  ;; emacs binding
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-, ,") 'helm-resume)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")   'helm-select-action)

  ;; evil binding
  (with-eval-after-load 'evil
    (evil-leader/set-key
      "x" 'helm-M-x
      "e" 'helm-find-files
      "b" 'helm-mini
      "k" 'kill-buffer))

  (when (require 'delight nil t)
    (delight 'helm-mode "Ⓗ" 'helm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cscope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xcscope & helm-cscope
(el-get-bundle xcscope)
(el-get-bundle helm-cscope
  :type github :pkgname "alpha22jp/helm-cscope.el")
(when (and (require 'xcscope nil t)
	   (require 'helm-cscope nil t))

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
      "co" 'helm-cscope-pop-mark))

  (when (require 'delight nil t)
    (delight 'helm-cscope-mode "Ⓒ" 'helm-cscope)))

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
(add-hook 'c-mode-common-hook (function (lambda nil (abbrev-mode 1))))

;; compile
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
            (setq-default indent-tabs-mode t
			  tab-width 8
			  py-indent-tabs-mode t
			  tab-always-indent t)
            (setq indent-tabs-mode t
		  tab-width 8
		  py-indent-tabs-mode t
		  tab-always-indent t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
(require 'org)

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
      org-image-actual-width		; resize inline image (1/3)
      (/ (display-pixel-width) 3))

;; agenda
(load-library "find-lisp")
(setq org-agenda-files			; set agenda files
      ;(file-expand-wildcards (concat home-emacs-directory "Org/*.org"))
      (find-lisp-find-files (concat home-emacs-directory "Org/Task") "\.org$")
      org-agenda-start-on-weekday 0	; agenda starts on sunday
      org-agenda-span 31)		; number of days for agenda
(defun b/org-agenda-redo ()
  (interactive)
  (setq org-agenda-files
	(find-lisp-find-files (concat home-emacs-directory "Org/Task") "\.org$"))
  (org-agenda-redo t))

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . nil)))

;; fonts
(custom-set-faces '(org-level-1 ((t (:height 1.0))))
		  '(org-level-2 ((t (:height 1.0))))
		  '(org-level-3 ((t (:height 1.0))))
		  '(org-level-4 ((t (:height 1.0))))
		  '(org-level-5 ((t (:height 1.0))))
		  '(org-level-6 ((t (:height 1.0)))))

;; binding
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "r" 'b/org-agenda-redo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq calendar-week-start-day 0)	; 0:Sunday, 1:Monday

;; korean holidays
(with-eval-after-load 'holidays
  (el-get-bundle korean-holidays
    :type github :pkgname "tttuuu888/korean-holidays"
    (require 'korean-holidays)
    (setq calendar-holidays korean-holidays)))

;; calfw
(el-get-bundle calfw
  :type github :pkgname "kiwanami/emacs-calfw"
  (require 'calfw)
  (require 'calfw-org)

  ;; remove warning message from compiler
  (declare-function org-bookmark-jump-unhide "org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
(el-get-bundle auctex
  :url "https://git.savannah.gnu.org/git/auctex.git"
  (load "auctex.el" nil t t))

;; latex preview pane
(el-get-bundle latex-preview-pane
  (require 'latex-preview-pane)
  (latex-preview-pane-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; version control system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(el-get-bundle magit
  (load (concat el-get-dir "magit/lisp/magit-autoloads")))

;; git-gutter
(el-get-bundle git-gutter
  (require 'git-gutter)
  (global-git-gutter-mode t)

  (when (require 'delight nil t)
    (delight 'git-gutter-mode "Ⓖ" 'git-gutter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check & build
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gradle
(el-get-bundle gradle-mode
  (require 'gradle-mode)
  (if (eq system-type 'windows-nt)
      (setq gradle-executable-path "\"C:/Program Files/Android/Android Studio/gradle/gradle-4.1/bin/gradle.bat\""))
  (gradle-mode 1)

  (when (require 'delight nil t)
    (delight 'gradle-mode "Ⓡ" 'gradle-mode)))

;; flyspell
(el-get-bundle flyspell
  (require 'ispell)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (if (eq system-type 'windows-nt)
		  (setq ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell"))
	      (flyspell-mode 1)))

  (when (require 'delight nil t)
    (delight 'flyspell-mode "Ⓕ" 'flyspell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-w3m
(el-get-bundle emacs-w3m
  :type git
  :url "https://github.com/ecbrown/emacs-w3m"
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-use-cookies t)
  (setq w3m-default-display-inline-images t)
  (setq w3m-coding-system 'utf-8
	w3m-file-coding-system 'utf-8
	w3m-file-name-coding-system 'utf-8
	w3m-input-coding-system 'utf-8
	w3m-output-coding-system 'utf-8
	w3m-terminal-coding-system 'utf-8))

;; gnus
(setq user-mail-address "user@gmail.com"
      user-full-name "user")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-w3m-safe-url-regexp nil)

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(gnus-add-configuration
 '(article
   (horizontal 1.0
	       (vertical 25
			 (group 1.0))
	       (vertical 1.0
			 (summary 0.25 point)
			 (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
	       (vertical 25
			 (group 1.0))
	       (vertical 1.0
			 (summary 1.0 point)))))
