;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server
(require 'server)

;; use tcp server
;(setq server-use-tcp t)
;(setq server-host "ip")

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

(setq el-get-bundle-sync nil)		; allow async. operation
(setq el-get-allow-insecure t)		; allow local file

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
(defun reload-dotemacs-file()
  "reload .emacs"
  (interactive)
  (load-file (expand-file-name ".emacs" home-emacs-directory)))
(global-set-key (kbd "C-c C-l") 'reload-dotemacs-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)	; disable startup message
(menu-bar-mode -1)			; hide menu bar
(tool-bar-mode -1)			; hide tool bar
;(scroll-bar-mode -1)			; hide scroll bar
(auto-image-file-mode t)		; show inline image
(global-auto-revert-mode t)		; auto refresh
;(global-linum-mode)                    ; line number

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(line-number-mode t)			; display line number in mode line
(column-number-mode t)			; display column number in mode line
(size-indication-mode t)		; display file size in mode line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (add-to-list 'default-frame-alist '(font . "-outline-D2Coding-normal-normal-normal-mono-16-*-*-*-c-*-iso10646-1"))
  (add-hook 'after-make-frame-functions
	    (lambda (frame)
	      (with-selected-frame frame
		(set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))))))
(when window-system
  (set-face-font 'default "-outline-D2Coding-normal-normal-normal-mono-16-*-*-*-c-*-iso10646-1")
  (set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; input method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-input-method "korean-hangul")
(global-set-key [?\S- ] 'toggle-input-method)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto saving
(setq backup-inhibited t)               ; disable backaup
(setq auto-save-default nil)            ; disable auto save
(setq auto-save-list-file-prefix temp-emacs-directory)

;; save last position
(save-place-mode t)
(setq save-place-file (expand-file-name "places" temp-emacs-directory))
(setq save-place-forget-unreadable-files nil)

;; save recent files
(recentf-mode t)
(setq recentf-save-file (expand-file-name "recentf" temp-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight current line
(global-hl-line-mode 1)
(set-face-foreground  'highlight nil)
(set-face-background  'highlight "#444444")
(set-face-underline-p 'highlight nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; selection
(global-set-key (kbd "C-SPC")     'set-mark-command)
(global-set-key (kbd "C-S-SPC")   'rectangle-mark-mode)
(global-set-key (kbd "<C-kanji>") 'set-mark-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get-bundle monokai-theme
  :url "https://github.com/oneKelvinSmith/monokai-emacs"
  (load-theme 'monokai t))

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
  (require 'calfw-org))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(require 'org)

(add-to-list 'auto-mode-alist		; org-mode extension
	     '("\\.org$" . org-mode))

(load-library "find-lisp")		; set agenda files
(setq org-agenda-files
      (find-lisp-find-files "/home/backup/Org" "\.org$"))

(setq org-todo-keywords			; todo keyword
      '((sequence "TODO(t)" "NEXT(n)" "DONE(d)")))

(setq org-hide-leading-stars t)

(setq org-startup-with-inline-images t)	; show inline image
(setq org-image-actual-width		; resize inline image (1/3)
      (/ (display-pixel-width) 3))

(setq org-agenda-start-on-weekday 0)	; agenda starts on sunday
