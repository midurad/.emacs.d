;; initialize the packages
(require 'package)

;; list the repositories to use
;; note: elpa.gnu.org times out so is not included
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; list the packages needed to proceed
(defvar package-list '(use-package))

;; install missing packages from the list
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

(use-package graphene
  :ensure t)

(use-package hideshow-org
  :ensure t
  :init (progn
          (add-hook 'c++-mode-hook (lambda () (progn (hs-minor-mode 1) (hs-org/minor-mode 1))))
          (add-hook 'c-mode-hook (lambda () (hs-minor-mode 1)))
          (add-hook 'java-mode-hook (lambda () (hs-minor-mode 1)))
          (add-hook 'lisp-mode-hook (lambda () (hs-minor-mode 1)))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config (progn
	    (use-package elpy
	      :config (elpy-enable)
	      :ensure t)))

(use-package ample-zen-theme
  :ensure t)

;; line highlight ON
(require 'highlight-current-line)
(highlight-current-line-on t)

;; blinking cursor ON
(blink-cursor-mode t)

;; change colors of the whitespace mode
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-current-line-face ((t (:background "gray22"))))
 '(whitespace-indentation ((t (:background "gray13" :foreground "#CC5542"))) t)
 '(whitespace-space ((t (:background "gray13" :foreground "white smoke"))) t))

;; scroll by a single line up 
(defun scroll-up-one-line()
  (interactive)
  (scroll-up 1))

(global-set-key (kbd "C-.") 'scroll-up-one-line)

;; scroll by a single line down 
(defun scroll-down-one-line()
  (interactive)
  (scroll-down 1))

(global-set-key (kbd "C-,") 'scroll-down-one-line)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :ensure t)

(use-package quack
  :init (progn (setq quack-default-program "mit-scheme")
               (setq quack-fontify-style 'emacs)
               (setq quack-global-menu-p 'nil)
               (add-hook 'scheme-mode-hook 'quack)
               (add-hook 'lisp-mode-hook 'quack))
  :ensure t)

(use-package rainbow-delimiters
  :config (global-rainbow-delimiters-mode t)
  :ensure t)
  
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(use-package tramp
  :ensure t)

(use-package cl
  :ensure t)

(use-package cl-lib
  :ensure t)

;; (use-package god-mode
;;   :init (progn
;;           (global-set-key (kbd "<escape>") 'god-local-mode)
;;           (defun god-update-cursor ()
;;             (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                                   'box
;;                                 'bar)))
;;           (add-hook 'god-mode-enabled-hook 'god-update-cursor)
;;           (add-hook 'god-mode-disabled-hook 'god-update-cursor))
;;   :ensure t)

(use-package info+
  :ensure t)

(use-package iedit
  :init (define-key global-map (kbd "C-c ;") 'iedit-mode)
  :ensure t)

(use-package expand-region
  :init (progn (global-set-key (kbd "C-0") 'er/expand-region)
               (global-set-key (kbd "C-9") 'er/contract-region))
  :ensure t)

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(use-package evil
  :init (evil-mode 1)
  :config (progn (quelpa '(powerline :fetcher github :repo "Dewdrops/powerline"))
                 (use-package powerline
                   :init (powerline-evil-theme)))
  :ensure t)

(use-package magit
  :ensure t)
