;;; init.el --- PJ emacs configuration file

;;;; DEFAULTS - emacs basic configs

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; No splash screen please... jeez
(setq inhibit-startup-screen t)


;; Add line numbering
(setq linum-format "%4d ");; \u2502 ")
(add-hook 'prog-mode-hook 'linum-mode)


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


;; Turn on spell checking with flyspell(-prog)-mode
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

(dolist (hook_modes_list '(text-mode-hook
			   fundamental-mode-hook))
(add-hook hook_modes_list 'flyspell-mode))

(setq ispell-program-name "/usr/local/bin/aspell")  ;; Use aspell for spellcheck
(global-set-key (kbd "M-s") 'ispell-word)	    ;; Key-bind for word at cursor spell check.


;; Make mac right cmd key work as meta key
(cond ((string-equal system-type "darwin") ; Mac OS X
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
  (spaceline-emacs-theme)
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


;; Change default font to bigger one
(set-default-font "Hack 14")


;;;; EMACS LISP

;; Enable navigation to sections starting with ";;;;" through imenu
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


;;;; CUSTOM

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; init.el ends here
