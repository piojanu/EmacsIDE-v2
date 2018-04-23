;;; init.el --- PJ emacs configuration file

;; Enable navigation to sections starting with ";;;;" through imenu
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


;;;; DEFAULTS - emacs basic configs

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; No splash screen please... jeez
(setq inhibit-startup-screen t)


;; Make emacs frame maximized at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Add line numbering
(setq linum-format "%4d ");; \u2502 ")
(add-hook 'prog-mode-hook 'linum-mode)


;; Set tab width
(setq-default c-basic-offset 4)                  ;; Default is 2
(setq-default c-indent-level 4)                  ;; Default is 2


;; Highlight lines that exceed 100 characters
(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)


;; Auto pair brackets
(electric-pair-mode 1)

;; Make electric-pair-mode work on more brackets
(setq electric-pair-pairs '(
  (?\" . ?\")
  (?\{ . ?\})
))


;; Change default font to bigger one
(set-frame-font "Hack 14")


;; Shorten 'yes' and 'no' answers to one letter
(defalias 'yes-or-no-p 'y-or-n-p)


;; Enables moving to other windows with Shift-<arrow key>
(windmove-default-keybindings)


;; Wrap at word boundaries
(setq-default word-wrap t)


;; Set up backups and change backup files storage directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 7   ;; Number of newest versions to keep.
      kept-old-versions 2   ;; Number of oldest versions to keep.
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

(setq ispell-program-name "/usr/local/bin/aspell")  ;; Use aspell for spellcheck
(global-set-key (kbd "M-s") 'ispell-word)	    ;; Key-bind for word at cursor spell check.


;; Turn on recentf
(recentf-mode)
(setq recentf-max-saved-items 250)


;; Make mac cmd key work as meta key
(cond ((string-equal system-type "darwin") ;; Mac OS X
  (progn
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil))))


;;;; MELPA & USE-PACKAGE

;; Set up MELPA repository
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
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

;; Add delight package early on
(use-package delight
  :pin gnu
  :config
  (delight 'flyspell-mode "" 'flyspell)
  (delight 'whitespace-mode "" 'whitespace)
  (delight 'auto-revert-mode "" 'autorevert)
  (delight 'abbrev-mode "" 'abbrev)
  
)


;;;; APPEARANCE

;; Load Dracula/Tomorrow Theme...
(use-package color-theme-tomorrow
  :load-path "~/.emacs.d/themes"
  :config
  (if (display-graphic-p)
    (load-theme 'dracula t)                   ;; ...for GUI
    (load-theme 'tomorrow-night-eighties t))  ;; ...for CMD
)


;; Change mode-line theme to that from Spacemacs
(use-package spaceline-config
  :if window-system
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  (setq ns-use-srgb-colorspace nil)
  (cond ((string-equal system-type "darwin") ;; Mac OS X
	 (progn (setq powerline-height 20))))
  (setq powerline-default-separator 'wave)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (set-face-attribute 
   'spaceline-evil-normal nil :background "#F181C0" :foreground "black")
  (set-face-attribute 
   'spaceline-evil-insert nil :background "#FFBC74" :foreground "black")
  (set-face-attribute 
   'spaceline-evil-visual nil :background "#86CBD3" :foreground "black")
  (spaceline-compile)
)


;;;; C++
(use-package cmake-ide
  :hook ((c++-mode . cmake-ide-setup)
	 (c-mode . cmake-ide-setup))
  :after rtags
)

(use-package cuda-mode
  :delight
  :mode ("\\.cu\\'" . cuda-mode))

(use-package irony
  :hook ((c++-mode . irony-mode)
	 (c-mode . irony-mode))
  :defer t
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package company-irony
    :after company
    :config (add-to-list (make-local-variable 'company-backends) 'company-irony))

  (use-package irony-eldoc
    :delight eldoc-mode
    :pin melpa
    :hook (irony-mode . irony-eldoc))

  (use-package flycheck-irony
    :after flycheck
    :hook (flycheck-mode . flycheck-irony-setup))
  :delight abbrev-mode
)

(use-package rtags
  :commands (rtags-location-stack-back
	     rtags-find-symbol-at-point
	     rtags-find-references-at-point)
  :bind (:map c-mode-base-map
  	 ("M-*" . rtags-location-stack-back)
  	 ("M-." . rtags-find-symbol-at-point)
  	 ("M-r" . rtags-find-references-at-point)) 
  :after evil
  :config
  (evil-define-key 'normal c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))

  (use-package ivy-rtags
    :config
    (setq rtags-display-result-backend 'ivy))

  (use-package flycheck-rtags
    :config
    (defun my-flycheck-rtags-setup ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil)) ;; RTags creates more accurate overlays.
    (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup))
  :demand
)


;;;; GIT

;; Install Magit
(use-package magit
  :commands magit-status
  :bind
  ("C-c g" . magit-status)
  :config 
  ;; Better integration between Evil and Magit
  (use-package evil-magit)
  :demand
)


;;;; MARKDOWN

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (custom-set-variables '(markdown-command "/usr/local/bin/pandoc"))
  (setq markdown-fontify-code-blocks-natively t)
)


;;;; Org-mode

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
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

  ;; org-todo-list for current file
  (defun org-todo-list-current-file (&optional arg)
  "Like `org-todo-list', but using only the current buffer's file."
  (interactive "P")
  (let ((org-agenda-files (list (buffer-file-name (current-buffer)))))
    (if (null (car org-agenda-files))
        (error "%s is not visiting a file" (buffer-name (current-buffer)))
      (org-todo-list arg))))

  ;; Add workflow states
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

  ;; Allows to embed a TODO within text without treating it as an outline heading
  (require 'org-inlinetask)

  ;; Better (utf-8) Org bullets
  (use-package org-bullets
    :commands org-bullets-mode
    :hook (org-mode . org-bullets-mode))

  ;; JIRA backend for export engine
  (use-package ox-jira
    :load-path "~/.emacs.d/elisp")
  
  ;; Markdown backend for export engine
  (use-package ox-md
    :load-path "~/.emacs.d/elisp")
)


;;;; PYTHON

(use-package anaconda-mode
  :delight eldoc-mode
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode))
  :defer t
  :after evil
  :config
  (evil-define-key 'normal python-mode-map (kbd "M-.") (function anaconda-mode-find-definitions))
  
  ;; Anaconda backend for company completion
  (use-package company-anaconda
    :after company
    :config (add-to-list (make-local-variable 'company-backends) 'company-anaconda))
  
  ;; Virtualenv support in emacs
  (use-package pyvenv
    :commands pyvenv-workon
    :init (defalias 'workon 'pyvenv-workon))

  ;; Auto Python PEP8 formatting
  (use-package py-autopep8
    :hook (python-mode . py-autopep8-enable-on-save)
    :config (setq py-autopep8-options '("--max-line-length=100")))
)


;;;; UTILITIES

;; Install ace-window
(use-package ace-window
  :commands ace-window
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
)


;; Install Avy - jumping to visible text using a char-based decision tree
(use-package avy
  :commands
  (avy-goto-char-2 avy-goto-char-timer ivy-avy)
  :bind
  ("C-'" . avy-goto-char-2)
  ("C-\"" . avy-goto-char-timer)
)


;; Install Company
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
  
  ;; Set default backends globally
  (setq company-backends
	'((company-files company-keywords company-capf company-yasnippet) (company-abbrev company-dabbrev)))

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


;; Drag Stuff - so you can move around lines, regions, etc.
(use-package drag-stuff
  :delight
  :hook (prog-mode . drag-stuff-mode)
  :config
  ;; Apply default key bindings
  (drag-stuff-define-keys)
)


;; Install Evil
(use-package evil
  :delight undo-tree-mode
  :bind
  (:map evil-insert-state-map
    ;; Leave insert-state after save
    ("C-x C-s" . (lambda () (interactive) (save-buffer) (evil-normal-state))))
  (:map evil-motion-state-map
    ("C-u" . evil-scroll-up))
  :config
  ;; Remove all keybindings from insert-state keymap, so I can use emacs ones
  (setcdr evil-insert-state-map nil)
  ;; But [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (evil-mode 1)
  :demand
)


;; Install Evil tutor
(use-package evil-tutor
  :after evil
  :commands evil-tutor-start
)


;; Install exec-path-from-shell, for path consistency between shell and GUI emacs
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
)


;; Intall expand-region, for selection region by semantic units
(use-package expand-region
  :commands
  er/expand-region
  :bind
  ("C-@" . er/expand-region)
)


;; Install Flycheck, replacement for the older Flymake
(use-package flycheck
  :delight
  :config
  (global-flycheck-mode)

  ;; Check after save
  (setq flycheck-check-syntax-automatically '(mode-enabled save)) 
)


;; Install Ivy, minibuffer completion
(use-package counsel ;; it'll install ivy and swiper as dependencies
  :delight (ivy-mode) (counsel-mode)
  :bind
  ("C-c r" . ivy-resume)
  ("C-s" . swiper)
  ("C-c s s" . counsel-ag)
  ("C-c s g" . counsel-grep)
  ("C-c i" . counsel-imenu)
  ("C-c y" . counsel-yank-pop)
  ("C-x C-r" . counsel-recentf)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (counsel-mode)

  ;; Remove '^' from the beginning of input match
  (setq ivy-initial-inputs-alist nil)

  ;; Allow for input not in order
  (setq ivy-re-builders-alist
        '((t   . ivy--regex-ignore-order)))
  
  ;; Get projectile integration
  (use-package counsel-projectile
    :delight
    :after projectile
    :config
    (counsel-projectile-mode)
  )
  :demand
)


;; Install Projectile
(use-package projectile
  :delight
  :config
  (projectile-global-mode t)
)


;; Install Rainbow delimiters
(use-package rainbow-delimiters
  :delight
  :hook (prog-mode . rainbow-delimiters-mode)
  :defer t
)


;; Install Stickyfunc Enhance
(use-package stickyfunc-enhance
  :delight
  :pin melpa
  :hook (prog-mode . semantic-mode)
  :defer t
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
)


;; Install which-key, propose keybind after entered prefix
(use-package which-key
  :delight
  :after evil ;; for integration with Evil
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
)


;; Install yasnippet and yasnippet-snippets
(use-package yasnippet
  :delight yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :defer t
  :config
  (use-package yasnippet-snippets
    :pin melpa)
  (yas-reload-all)
)


;;;; CUSTOM

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (company-anaconda org flycheck-rtags ivy-rtags irony-eldoc py-autopep8 pyvenv evil-magit yasnippet-snippets use-package)))
 '(safe-local-variable-values (quote ((cmake-ide-build-dir . "./build/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; init.el ends here
