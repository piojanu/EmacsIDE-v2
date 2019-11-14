;;; new-init.el --- PJ emacs configuration file

;;; Commentary:
;;; Emacs basic configuration.

;;; Code:

;;;; CORE
;;;;------


;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;; Make emacs frame maximized at startup and add shortcut for full-screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)

;; Add line numbering
(setq linum-format "%4d ");; \u2502 ")
(add-hook 'prog-mode-hook 'linum-mode)

;; Highlight lines that exceed 119 characters (width of GitHub code-review window)
(require 'whitespace)
(setq whitespace-line-column 119) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Auto pair brackets
(electric-pair-mode 1)

;; Make electric-pair-mode work on more brackets
(setq electric-pair-pairs '(
  (?\" . ?\")
  (?\{ . ?\})
))

;; Shorten 'yes' and 'no' answers to one letter
(defalias 'yes-or-no-p 'y-or-n-p)

;; Wrap at word boundaries
(setq-default word-wrap t)

;; Set up backups and change backup files storage directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 7   ;; Number of newest versions to keep.
      kept-old-versions 3   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Change auto-save storage directory
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))

;; Turn on spell checking...
;; ...when in programming mode with flyspell-prog-mode
(dolist (hook_prog_modes_list '(lisp-mode-hook
		           emacs-lisp-mode-hook
			   yaml-mode
			   python-mode-hook
			   shell-mode-hook
			   css-mode-hook
			   javascript-mode-hook
			   LaTeX-mode-hook
			   c-mode-common-hook))
(add-hook hook_prog_modes_list 'flyspell-prog-mode))
;; ...when in text mode with flyspell-mode
(dolist (hook_modes_list '(text-mode-hook
			   fundamental-mode-hook))
(add-hook hook_modes_list 'flyspell-mode))

;; Use aspell for spellcheck
(setq ispell-program-name "/usr/local/bin/aspell")

;; Key-bind for word at cursor spell check
(global-set-key (kbd "M-s") 'ispell-word)	    

;; Turn on recentf
(recentf-mode)
(setq recentf-max-saved-items 1000)

;; Make mac cmd key work as meta key
(cond ((string-equal system-type "darwin") ;; Mac OS X
  (progn
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil))))


;;;; MELPA & USE-PACKAGE
;;;;---------------------


;; Use the MELPA repository
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Always download and install package if not present
(setq use-package-always-ensure t)

;; Install packages from melpa-stable by default,
;; if not explicitly pointed differently (by ":pin" keyword)
(setq use-package-always-pin "melpa-stable")


;;;; APPEARANCE
;;;;------------


;; Load Dracula/Tomorrow Theme...
(use-package doom-themes
  :ensure t
  :load-path "~/.emacs.d/themes"
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (if (display-graphic-p)
    (progn
      (load-theme 'doom-one t)                ;; ...for GUI
      (doom-themes-org-config))
    (load-theme 'tomorrow-night-eighties t))  ;; ...for CMD
)

;; Change mode-line theme 
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq find-file-visit-truename t)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq column-number-mode t
        line-number-mode t)
)

;; Pair brackets with colors
(use-package rainbow-delimiters
  :delight
  :hook (prog-mode . rainbow-delimiters-mode)
  :defer t
)


;;;; NAVIGATION
;;;;------------

;; Vim-like navigation
(use-package evil
  :delight undo-tree-mode
  :bind
  (:map evil-insert-state-map
    ;; Leave insert-state after save
    ("C-x C-s" . (lambda () (interactive) (save-buffer) (evil-normal-state))))
  (:map evil-motion-state-map
    ("C-u" . scroll-down-command)
    ("C-d" . scroll-up-command))
  ("C-j" . evil-jump-backward)
  ("C-M-j" . evil-jump-forward)
  :config
  ;; Remove all keybindings from insert-state keymap, so I can use emacs ones
  (setcdr evil-insert-state-map nil)
  ;; But [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (evil-mode 1)
  :demand
)

;;; Enable navigation to sections starting with ";;;;" through imenu
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;; Fuzzy search of projects, files and in files, etc.
(use-package counsel ;; it'll install ivy and swiper as dependencies
  :delight (ivy-mode) (counsel-mode)
  :bind*
  ("C-s" . swiper)
  ("C-c s s" . counsel-ag)
  ("C-c s g" . counsel-grep)
  ("C-c i" . counsel-imenu)
  ("C-c y" . counsel-yank-pop)
  ("C-c r" . ivy-resume)
  ("C-c C-f" . counsel-projectile-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-c p" . counsel-projectile-switch-project)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-on-del-error-function nil)
  (counsel-mode)

  ;; Remove '^' from the beginning of input match
  (setq ivy-initial-inputs-alist nil)

  ;; Allow for input not in order
  (setq ivy-re-builders-alist
        '((t   . ivy--regex-ignore-order)))

  ;; Manage projects
  (use-package projectile
    :delight
    :config
    (projectile-global-mode t)
  )
  
  ;; Get projectile integration
  (use-package counsel-projectile
    :delight
    :after projectile
    :config
    (counsel-projectile-mode)
  )
  :demand
)

;; Goto windows fast
(use-package ace-window
  :commands ace-window
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
)

;; Goto words fast
(use-package avy
  :commands
  (avy-goto-char-2 avy-goto-char-timer ivy-avy)
  :bind
  ("C-'" . avy-goto-char-2)
  ("C-\"" . avy-goto-char-timer)
)

;; Allow you to move around lines, regions, etc.
(use-package drag-stuff
  :delight
  :hook (prog-mode . drag-stuff-mode)
  :config
  ;; Apply default key bindings
  (drag-stuff-define-keys)
)

;; Prompt commands hotkeys
(use-package which-key
  :delight
  :after evil ;; for integration with Evil
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
)


;;;; ENVIRONMENTS
;;;;--------------


;; Emacs library for working with conda environments
(use-package conda
  :config
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode nil))


;;;; CODING
;;;;--------


;; The CC modes (C, C++, Java, etc.) indention settings
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Install exec-path-from-shell, for path consistency between shell and GUI emacs
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
)

;; Git porcelain
(use-package magit
  :commands magit-status
  :bind
  ("C-c g" . magit-status)
  :config 
  ;; Better integration between Evil and Magit
  (use-package evil-magit)
  :demand
)

;; Multi-language jump to definition and references
(use-package dumb-jump
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-k" . dumb-jump-quick-look))
  :after ivy evil
  :config
  ;; Enable ivy support
  (setq dumb-jump-selector 'ivy)
  ;; Enable silver searcher support, it will still use git-grep if it's a git project
  (setq dumb-jump-prefer-searcher 'ag)
  :demand
)

;; Auto completion
(use-package company
  :delight
  :bind
  (:map company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous))
  :bind*
  ("C-M-i" . company-complete)
  :config
  ;; Enable company globally
  (global-company-mode)

  ;; company-jedi wires up jedi to be a backend for the company
  (use-package company-jedi
    :ensure t
    :pin melpa
    :bind
    ("C-M-h" . jedi:show-doc))

  ;; Customise company
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)) ;; Weight by frequency
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-align-annotations t
    ;; Easy navigation to candidates with M-<n>
    company-show-numbers t)

  ;; Use Company for completion
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)

  ;; Disable company-dabbrev downcasing
  (setq company-dabbrev-downcase nil)
  
  ;; Enable company Tab and Go feature
  (company-tng-configure-default)
    
  ;; Company and yasnippet integration
  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
	   (apply 'company-complete-common nil)))
      (yas-expand)))
  (add-hook 'company-mode-hook
   (lambda ()
     (substitute-key-definition
      'company-complete-common
      'company-yasnippet-or-completion
      company-active-map)))

  ;; set default `company-backends'
  (setq company-backends
        '((company-jedi
           company-files)
          (company-dabbrev-code
           company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)
          (company-abbrev company-dabbrev)))
  
  ;; Install company quickhelp
  (use-package company-quickhelp
    :bind (:map company-active-map
	   ("C-c h" . company-quickhelp-manual-begin))
    :config
    ;; Enable globally
    (company-quickhelp-mode)
    ;; Don't automatically pop up help dialog
    (setq company-quickhelp-delay nil)
  )
  :demand
)

;; Templates expansions
(use-package yasnippet
  :delight yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :defer t
  :config
  (use-package yasnippet-snippets
    :pin melpa)
  (yas-reload-all)
)

;; Syntax checking
(use-package flycheck
  :delight
  :config
  (global-flycheck-mode)

  ;; Disable error indication on fringe
  (setq flycheck-indication-mode nil)
  ;; Check after save
  (setq flycheck-check-syntax-automatically '(mode-enabled save)) 
)

;; Show function declaration on the top of the buffer
(use-package stickyfunc-enhance
  :delight
  :pin melpa
  :hook (prog-mode . semantic-mode)
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (require 'stickyfunc-enhance)
)


;;;; WRITING
;;;;---------


;; Add LaTeX support
(add-hook 'latex-mode-hook
  (function (lambda ()
    (run-hooks 'prog-mode-hook)
    (whitespace-mode -1))))

;; Allows for taking extended notes in Org-mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
  :bind ("C-c l" . org-store-link)
  :bind* ("C-'" . avy-goto-char-2)
  :config
  ;; Set fold symbol to be arrow pointing right and then curving downwards
  (setq org-ellipsis
    (if (char-displayable-p ?\u2935) " \u2935"
         'org-ellipsis))

  ;; Code buffers configuration
  (setq org-src-fontify-natively t)           ;; Highlight natively
  (setq org-src-tab-acts-natively t)          ;; TAB acts natively to that language buffer
  (setq org-src-window-setup 'current-window) ;; Use the current window for editing code snippets

  ;; Save the clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Wrap lines
  (add-hook 'org-mode-hook #'toggle-truncate-lines)

  ;; Add workflow states
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

  ;; Add success/failure tags
  (setq org-tag-alist
        '(("success" . ?s)))
  (setq org-tag-alist
        '(("failure" . ?f)))
  (setq org-tags-column 0)

  ;; Allows to embed a TODO within text without treating it as an outline heading
  (require 'org-inlinetask)

  ;; Better (utf-8) Org bullets
  (use-package org-bullets
    :commands org-bullets-mode
    :hook (org-mode . org-bullets-mode))
  
  ;; Markdown backend for export engine
  (use-package ox-md
    :load-path "~/.emacs.d/elisp")
)

;;;; USER CONFIG
;;;;-------------

;; Load user configuration if available
(if (file-readable-p "~/.user-locals.el")
    (load "~/.user-locals.el")
  nil)

;;; new-init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/piotr/anaconda3")
 '(package-selected-packages
   (quote
    (org-bullets company-jedi yasnippet-snippets which-key use-package typescript-mode stickyfunc-enhance spaceline rainbow-delimiters pyvenv py-autopep8 markdown-mode flycheck expand-region exec-path-from-shell evil-tutor evil-magit dumb-jump drag-stuff doom-themes doom-modeline delight cuda-mode counsel-projectile company-quickhelp company-anaconda ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
