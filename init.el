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


;; Highlight lines that exceed 80 characters
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
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
(set-default-font "Hack 14")


;; Shorten 'yes' and 'no' answers to one letter
(defalias 'yes-or-no-p 'y-or-n-p)


;; Enables moving to other windows with Shift-<arrow key>
(windmove-default-keybindings)


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

;; Add diminish package early on
(use-package diminish)


;;;; APPEARANCE

;; Load Tomorrow Theme...
(use-package color-theme-tomorrow
  :load-path "~/.emacs.d/themes"
  :config
  (if (display-graphic-p)
    (load-theme 'tomorrow-night t)             ;; ...for GUI
    (load-theme 'tomorrow-night-eighties t)))  ;; ...for CMD


;; Change mode-line theme to that from Spacemacs
(use-package spaceline-config
  :if window-system
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-height 20)
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


;;;; UTILITIES

;; Install Evil
(use-package evil
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
  :diminish undo-tree-mode
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


;; Install Ivy, minibuffer completion
(use-package counsel ;; it'll install ivy and swiper as dependencies
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-s" . swiper)
  ("C-c s s" . counsel-ag)
  ("C-c s g" . counsel-grep)
  ("C-c i" . counsel-imenu)
  ("C-c y" . counsel-yank-pop)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (counsel-mode)

  ;; Make tab key do indent first then completion
  (setq-default tab-always-indent 'complete)
  
  ;; Get projectile integration
  (use-package counsel-projectile
    :after projectile
    :config
    (counsel-projectile-mode)
    :diminish counsel-projectile-mode
  )
  :diminish (ivy-mode counsel-mode)
  :demand
)


;; Install Projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode t)
)


;; Install which-key, propose keybind after entered prefix
(use-package which-key
  :after evil ;; for integration with Evil
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
)


;;;; CUSTOM

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; init.el ends here
