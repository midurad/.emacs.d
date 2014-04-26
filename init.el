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

;; define "previous window" function
(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;; now redefine few key bindings
(global-set-key (kbd "C-x C-p") 'other-window-backward)
(global-set-key (kbd "C-x C-n") 'other-window)

(defun move-point-to-top ()
  "Put point on top line of the window."
  (interactive)
  (move-to-window-line 0))

(global-set-key (kbd "M-,") 'move-point-to-top)

(defun move-point-to-bottom ()
  "Put point on the botton line of the window."
  (interactive)
  (move-to-window-line -1))

(global-set-key (kbd "M-.") 'move-point-to-bottom)

(defun move-line-to-top ()
  "Move current line to top of the window."
  (interactive)
  (recenter 0))

(global-set-key (kbd "M-!") 'move-line-to-top)

;; initialize the packages
(require 'package)

;; list the repositories to use
;; note: elpa.gnu.org times out so is not included
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; activate the package system
(package-initialize)

;; fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; list the packages needed to proceed
(defvar package-list '(use-package))

;; install missing packages from the list
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(require 'use-package)

;; starter-kit
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

(use-package info+
  :ensure t)

(use-package iedit
  :init (define-key global-map (kbd "C-c ;") 'iedit-mode)
  :ensure t)

(use-package expand-region
  :init (progn (global-set-key (kbd "C-0") 'er/expand-region)
               (global-set-key (kbd "C-9") 'er/contract-region))
  :ensure t)

(use-package evil
  :config (progn
            (evil-mode)
            (setq el-get-sources
                  '((:name powerline
                           :type github
                           :pkgname "Dewdrops/powerline"))
                  )
            (el-get 'sync 'powerline)
            (use-package powerline
              :init (powerline-evil-theme))
            )
  :ensure t)

(use-package scala-mode2
  :mode ("\\.scala\\'" . scala-mode)
  :config (progn
            (setq el-get-sources
                  '((:name ensime
                           :type github
                           :pkgname "aemoncannon/ensime-original"
                           :build '(("sbt" "update" "stage"))
                           :load-path "./dist/elisp/")))
            (el-get 'sync 'ensime)
            (use-package ensime
              :init (progn
                      (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))))
  :ensure t)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(use-package org
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.org$". org-mode))
  :ensure t)

(use-package magit
  :ensure t)

