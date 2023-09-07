(setq ns-command-modifier 'meta)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(add-to-list 'load-path "/Users/fregeholy/.emacs.d/lisp/")

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package dash
  :ensure t)
(eval-when-compile
  (require 'cl))
(setq auto-save-default nil)
;; 매번 버퍼 설정시 저장하는것을 disable
(setq-default buffer-save-without-query nil)

(use-package exec-path-from-shell
   :ensure t
   )
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))
 ;; for eshell
 (defun eshell-mode-hook-func ()
   ;; (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
   (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
   (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))

 (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

 ;; (when (daemonp)
 ;;   (exec-path-from-shell-initialize))

(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
  (setq org-adapt-indentation t)
  )

(defun holy/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'holy/org-babel-tangle-config)))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(fset 'yes-or-no-p 'y-or-n-p)

(defun show-emacs-conf()
  (interactive)
  (find-file "~/.emacs.d/init.org"))

(setq user-full-name "Holy Frege")
(setq user-mail-address "hoyoul.park@gmail.com")

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

(setq-default shell-file-name "/bin/zsh")

(setq org-return-follows-link t)

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
(straight-use-package 'use-package)

(use-package rg
:ensure t)

(setq inhibit-startup-message t)     
(scroll-bar-mode -1)   
(tool-bar-mode -1)     
(tooltip-mode -1)      
(set-fringe-mode 10)   
(menu-bar-mode -1)     
(setq visible-bell t)

(use-package doom-themes
  :init (load-theme 'doom-monokai-spectrum t)
  )
(doom-themes-neotree-config)
;; this isn't necessary, because 'simple is the default
(setq doom-neotree-file-icons 'simple)

(use-package all-the-icons
     :ensure t)

   (use-package minions
     :hook (doom-modeline-mode . minions-mode))

   (use-package doom-modeline
     :ensure t
     :init (doom-modeline-mode 1)
     :custom-face
     ;; modeline font size
     (mode-line ((t (:height 1.05))))
     (mode-line-inactive ((t (:height 0.95))))
;;; add to $DOOMDIR/config.el
     (setq all-the-icons-scale-factor 1.1)
     :custom
     (doom-modeline-major-mode-icon t)
     (doom-modeline-project-detection 'auto)
     (doom-modeline-buffer-file-name-style 'auto)
     (doom-modeline-major-mode-color-icon t)
     (doom-modeline-buffer-state-icon t)
     (doom-modeline-minor-modes t)
     (doom-modeline-indent-info t)
     (doom-modeline-vcs-max-length 12)
     (doom-modeline-persp-name t)
     (doom-modeline-display-misc-in-all-mode-lines t)
     (doom-modeline-env-version t)
     (doom-modeline-display-default-persp-name nil)
     )

(defun holy-daemon/set-font-faces ()		
  (message "Setting face on daemon")
  (set-face-attribute 'region nil :background "#834")
  (set-face-attribute 'default nil :font "Fira Code" :height 180)
  (dolist (charset '(hangul symbol cjk-misc ))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "D2Coding")))
  )
(defun holy-client/set-font-faces ()		
  (message "Setting face on client")
  (set-face-attribute 'region nil :background "#834")
  (set-face-attribute 'default nil :font "Fira Code" :height 180)
  )
;; 추가 test
   ;; (when (display-graphic-p) 
   ;;   ;; Do any keybindings and theme setup here
   ;;   (progn
   ;;       (message "GUI!!")
   ;;       (add-hook 'after-make-frame-functions
   ;;           (lambda (frame)
   ;;             (setq doom-modeline-icon t)
   ;;             (with-selected-frame frame
   ;;               (holy-daemon/set-font-faces))))
   ;;   )
   ;;   (holy-client/set-font-faces))

;; daemon일때는 수행하지 않기 때문에 이것을 주석처리


(if (daemonp)
    (progn
      (message "Daemon")
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (setq doom-modeline-icon t)
                  (with-selected-frame frame
                    (holy-daemon/set-font-faces)))))
  (holy-client/set-font-faces))

(use-package ace-window
:ensure t
:init
(progn
(global-set-key [remap other-window] 'ace-window)
(custom-set-faces
'(aw-leading-char-face
((t (:inherit ace-jump-face-foreground :height 3.0)))))
))

(global-display-fill-column-indicator-mode 1)
(setq-default display-fill-column-indicator-column 119)

(use-package pretty-symbols
  :ensure t)

(use-package org-superstar
    :ensure t
    ;; :hook
    ;; (org-mode . (lambda () (org-superstar-mode 1)))
    )
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-headline-bullets-list '("🅐" "🅑" "🅒" "🅓"  "🅔"  "🅕"  "🅖"))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-todo-bullet-alist '(("TODO" . ?➽)
                                          ("CURRENTLY" . ?⌛)
                                          ("SOMEDAY" . ?⏰)
                                          ("CANCELLED" . ?✘)
                                          ("DONE" . ?✓)))
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•)))
  (setq org-ellipsis " ▼ ")

  ;; (load-file "~/.emacs.d/lisp/my-capture-template.el")
(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))))

(use-package lorem-ipsum
:config
(lorem-ipsum-use-default-bindings))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(use-package counsel
  :ensure t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :after counsel
  :init
  (ivy-rich-mode 1))

(use-package neotree
  :ensure t
  :config
    (setq neo-theme 'icons)
  ) 

(global-set-key (kbd "C-c t") 'neotree-toggle)

(use-package undo-tree
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history")))
   (global-undo-tree-mode))

(use-package highlight-parentheses
  :ensure t
  :hook
  ((prog-mode org-mode) . highlight-parentheses-mode)
  :init
  ;; (setq highlight-parentheses-colors '("green" "red" "red orange" ))

  (setq highlight-parentheses-colors '("#00e1ff" "red" "green" "blue" ))
  :custom-face
  ;; (highlight-parentheses-highlight ((t (:weight bold))))
  )

(use-package gnuplot
    :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path (expand-file-name "/Users/fregeholy/.emacs.d/lisp/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  )

(use-package graphviz-dot-mode
  :ensure t
  :init
  ;; (require 'company-graphviz-dot)
  :config
  (setq graphviz-dot-indent-width 4))
;; (use-package company-graphviz-dot
;;   )

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))

;; global activation of the unicode symbol completion
(use-package company-math
  :ensure t
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  )
;; (use-package company-graphviz-dot
;;   )

(if (daemonp)
    (message "Daemon projectile")
    (use-package projectile
      :diminish projectile-mode
      :config
      (projectile-mode +1)
      (setq projectile-enable-caching t)
      (setq projectile-indexing-method 'alien)
      :custom
      ((projectile-completion-system 'ivy))
      :bind-keymap
      ("C-c p" . projectile-command-map)
      :init
      (setq projectile-switch-project-action #'projectile-dired)
      (when (file-directory-p "/Users/fregeholy/Documents/Projects/")
        (setq projectile-project-search-path '("/Users/fregeholy/Documents/Projects/"))))


  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode))
  )

(setq elfeed-db-directory "~/Dropbox/shared/elfeeddb")
(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
	      ("q" . bjm/elfeed-save-db-and-bury)
	      ("Q" . bjm/elfeed-save-db-and-bury)
	      ("m" . elfeed-toggle-star)
	      ("M" . elfeed-toggle-star)
	      )
  )
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/shared/elfeed.org")))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  ;; (add-to-list 'yas-snippet-dirs "~/Dropbox/WorkSpace/emacs/snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  :bind
  ("C-c s" . yas-insert-snippet)
  ("C-c n" . yas-new-snippet)
  ("C-c v" . yas-visit-snippet-file))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package buffer-move
:ensure t
:config
(global-set-key (kbd "<C-up>")     'buf-move-up)
(global-set-key (kbd "<C-down>")   'buf-move-down)
(global-set-key (kbd "<C-left>")   'buf-move-left)
(global-set-key (kbd "<C-right>")  'buf-move-right)
)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq magit-branch-read-upstream-first 'fallback)

(use-package forge
  :ensure t)

(use-package gist
:ensure t)

(use-package perspective
  :ensure t
  :bind
  (("C-x b" . persp-ivy-switch-buffer)         ; or use a nicer switcher, see below
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package ein
  :ensure t
  :config
  (require 'ein-notebook))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package gif-screencast
  :ensure t
  :commands (gif-screencast-mode)
  :config
  (setq gif-screencast-scale-factor 2.0)
  (add-hook 'after-init-hook #'gif-screencast-mode)
  :bind ("C-x C-g" . 'gif-screencast-start-or-stop))

(with-eval-after-load 'gif-screencast
  (setq gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (setq gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
  (setq gif-screencast-capture-format "ppm")) ;; Optional: Required to crop captured images.
