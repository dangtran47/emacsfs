(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

;; (setq visible-bell t)
(set-face-attribute 'default nil :font "Fira Code" :height 170)
(load-theme 'tango-dark)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("local" . "~/projects/myemacs/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; START: My configs for Copilot
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package dash
  :ensure t)
(use-package s
  :ensure t)
;; END: My configs for Copilot

;; Use C-c C-f to refresh ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-l" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package evil
  :config
  (evil-mode 1))

;; My extra configs

;; scroll line-by-line instead of half page
(setq scroll-step 1)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; highline region of last operation
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))
(setq evil-goggles-duration 0.200)

;; Auto fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Copilot
(add-to-list 'load-path "~/projects/myemacs/copilot.el")
(use-package copilot
  :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
  ;; don't show in mode line
  :diminish
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
         ("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))
