;; package --- My Emacs config -*- lexical-binding:t -*-
;; Author: Andrea Ciceri <andrea.ciceri@autistici.org>
;;; Commentary:
;; TODO
;; - org-roam
;; - org goodies
;; - persp-mode?
;; - understand how to configure cape
;;; Code:

(use-package flymake
  :config
  ;; TODO write "E", "W" or "N" in margins overriding the margin created by diff-hl
  ;; (push `(before-string . ,(propertize " " 'display '((margin left-margin) "E"))) (get :error 'flymake-overlay-control))
  ;; (push `(before-string . ,(propertize " " 'display '((margin left-margin) "W"))) (get :warning 'flymake-overlay-control))
  ;; (push `(before-string . ,(propertize " " 'display '((margin left-margin) "N"))) (get :note 'flymake-overlay-control))
  (set-face-attribute 'flymake-error  nil :inverse-video t)
  (set-face-attribute 'flymake-warning  nil :inverse-video t)
  (set-face-attribute 'flymake-note  nil :inverse-video t)
  :hook prog-mode)

(use-package emacs
  :bind (("<mouse-4>" . scroll-down-line)
	 ("<mouse-5>" . scroll-up-line))
  :hook (server-after-make-frame . (lambda () (xterm-mouse-mode +1))) ;; FIXME why is this needed?
  :custom
  (use-dialog-box nil)
  (use-short-answers t)
  (native-comp-async-report-warnings-errors nil)
  (inhibit-startup-message t)
  (visible-bell t)
  (scroll-conservatively 101) ;; more than 100 => redisplay doesn't recenter point)
  (scroll-margin 3)
  (temporary-file-directory "~/.emacs-saves/")
  (backup-directory-alist `(("." . ,temporary-file-directory)))
  (auto-save-files-name-transforms `((".*" ,temporary-file-directory t)))
  (backup-by-copying t)
  :config
  (set-face-background 'vertical-border (face-background 'default))
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))
  (menu-bar-mode -1)
  (xterm-mouse-mode +1)
  (tool-bar-mode -1)
  (global-hl-line-mode -1)
  (global-auto-revert-mode t)
  (show-paren-mode +1)
  (column-number-mode +1)
  (defun ccr/reload-emacs ()
    (interactive)
    (load-file "~/.config/emacs/init.el"))
)

(use-package dracula-theme
  :init
  (require 'ansi-color)
  (let ((colors '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")))
    (dolist (color colors)
      (set-face-attribute (intern (format "ansi-color-%s" color)) nil :foreground color :background color)))
    (let ((colors '("red" "green" "yellow" "blue" "cyan")))
    (dolist (color colors)
      (set-face-attribute (intern (format "ansi-color-bright-%s" color)) nil :foreground (format "dark %s" color) :background (format "dark %s" color))))
    ;; TODO find better colors (bright white and bright black should be different!)
    (set-face-attribute 'ansi-color-bright-white nil :foreground "grey" :background "grey")
    (set-face-attribute 'ansi-color-bright-black nil :foreground "grey" :background "grey")
    (set-face-attribute 'ansi-color-bright-magenta nil :foreground "magenta" :background "magenta")
    :config
  (load-theme 'dracula t))

(use-package solaire-mode
  :hook (server-after-make-frame . (lambda () (load-theme 'dracula t))) ;; FIXME when this is closed: https://github.com/hlissner/emacs-solaire-mode/issues/46
  :init
  (solaire-global-mode +1)
  :config
  (set-face-background 'solaire-default-face "#1c1d26"))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode +1)
  :hook (
	 (marginalia-mode . nerd-icons-completion-marginalia-setup)
	 (ibuffer-mode . nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-dired
    :hook
      (dired-mode . nerd-icons-dired-mode))

(use-package indent-bars
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-highlight-current-depth '(:blend 0.4))
  (indent-bars-no-stipple-char (string-to-char "┋")))

(use-package diredf
  :config (diredfl-global-mode))

(use-package treemacs
  :config
  ;; FIXME when this is closed: https://github.com/hlissner/emacs-solaire-mode/issues/51
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
  :custom
  (treemacs-show-cursor nil)
  (treemacs-display-current-project-exclusively t)
  (treemacs-project-followlinto-home nil)
  (treemacs-display-current-project-exclusively t)
  (treemacs-git-mode 'deferred)
  :bind (("C-c w t" . treemacs-select-window)
	 ("C-c o T" . treemacs))
  )

(use-package meow
  :hook (server-after-make-frame . (lambda () (meow--prepare-face)))
  :init
  (meow-global-mode 1)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '(">" . meow-indent)
   '("<" . meow-back-to-indentation)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("/" . meow-visit)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))


(use-package windmove
  :config
  (windmove-mode +1)
  (defcustom ccr/v-resize-amount 4
    "How smany rows move when calling `ccr/v-resize`"
    :type 'integer)
  (defcustom ccr/h-resize-amount 4
    "How many columns move when calling `ccr/h-resize`"
    :type 'integer) ;; (defcustom ccr/resize-rows 4)
  (defun ccr/v-resize (key)
    "Interactively vertically resize the window"
    (interactive "cHit >/< to enlarge/shrink")
    (cond ((eq key (string-to-char ">"))
	   (enlarge-window-horizontally ccr/v-resize-amount)
	   (call-interactively 'ccr/v-resize))
	  ((eq key (string-to-char "<"))
	   (enlarge-window-horizontally (- ccr/v-resize-amount))
	   (call-interactively 'ccr/v-resize))
	  (t (push key unread-command-events))))
  (defun ccr/h-resize (key)
    "Interactively horizontally resize the window"
    (interactive "cHit >/< to enlarge/shrink")
    (cond ((eq key (string-to-char ">"))
	   (enlarge-window ccr/h-resize-amount)
	   (call-interactively 'ccr/h-resize))
	  ((eq key (string-to-char "<"))
	   (enlarge-window (- ccr/h-resize-amount))
	   (call-interactively 'ccr/h-resize))
	  (t (push key unread-command-events))))
  :bind (("C-c w k" . windmove-up)
	   ("C-c w l" . windmove-right)
	   ("C-c w j" . windmove-down)
	   ("C-c w h" . windmove-left)
	   ("M-k" . windmove-up)
	   ("M-l" . windmove-right)
	   ("M-j" . windmove-down)
	   ("M-h" . windmove-left)
	   ("C-c w <up>" . windmove-up)
	   ("C-c w <right>" . windmove-right)
	   ("C-c w <down>" . windmove-down)
	   ("C-c w <left>" . windmove-left)
	   ("C-c w q" . delete-window)
	   ("C-c w K" . windmove-delete-up)
	   ("C-c w L" . windmove-delete-right)
	   ("C-c w J" . windmove-delete-down)
	   ("C-c w H" . windmove-delete-left)
	   ("C-c w x" . kill-buffer-and-window)
	   ("C-c w v" . split-window-right)
	   ("C-c w s" . split-window-below)
	   ("C-c w V" . ccr/v-resize)
	   ("C-c w S" . ccr/h-resize)))

(use-package vertico
  :custom
  (vertico-mouse-mode t)
  (vertico-reverse-mode t)
  (vertico-count 12)
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-mode t)
  :bind (:map vertico-map
    (("DEL" . vertico-directory-delete-char)
     ("C-DEL" . vertico-directory-delete-word))))

(use-package marginalia
  :init
  (marginalia-mode +1)
  :custom
  (marginalia-aligh 'right))

(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remazp imenu] . consult-imenu)
         ([remap project-switch-to-buffer] . consult-project-buffer)
          ("C-c b b" . consult-project-buffer)
           ("C-c b B" . consult-buffer)
           ("C-c g l" . consult-goto-line)
           ("C-c b i" . consult-imenu)
           ("C-c f f" . consult-find)
           ("C-c F" . consult-ripgrep)
           ("C-c f" . consult-find)
           ("C-c l" . consult-line)
           ("C-c m" . wconsult-mark)
           ("C-c o o" . consult-outline)
           ("C-c e" . consult-flymake))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package embark
  :bind (("C-'" . embark-act)
         ("C-=" . embark-dwim)))

(use-package corfu
  :config
  (corfu-terminal-mode)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :custom
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (kind-icon-default-face 'corfu-default)
  :bind (:map corfu-map
    (("M-d" . corfu-doc-toggle)
    ("M-l" . corfu-show-location)
    ("SPC" . corfu-insert-separator)))
  :init
  (global-corfu-mode))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package prog-mode
  :hook ((prog-mode . hl-line-mode)
	 (prog-mode . display-line-numbers-mode)))

(use-package which-key :config (which-key-mode))

(use-package magit
  :hook (magit-mode . magit-delta-mode)
  :bind (("C-c o g" . magit)))

(use-package sideline
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line)
  (sideline-backends-right '(sideline-flymake)))

(use-package nix-mode
  :hook	((nix-mode . eglot-ensure)
	 (nix-mode . tree-sitter-hl-mode)
	 (nix-mode . (lambda () (setq indent-bars-spacing-override 2) (indent-bars-mode))))
  :config
  (global-nix-prettify-mode))

(use-package haskell-mode
  :hook ((haskell-mode . eglot-ensure)
	 (haskell-mode . tree-sitter-hl-mode)))

(use-package purescript-mode
  :hook (purescript-mode .  turn-on-purescript-indentation))

(use-package terraform-mode
  :hook ((terraform-mode . eglot-ensure)
	 (terraform-mode . tree-sitter-hl-mode)))

(use-package yaml-mode
  :hook (yaml-mode . tree-sitter-hl-mode))

(use-package sh-mode
  :hook (sh-mode . tree-sitter-hl-mode))

(use-package lisp
  :hook ((lisp-mode . enable-paredit-mode)
	 (emacs-lisp-mode . enable-paredit-mode)))

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1))

(use-package envrc
  :after (inheritenv)
  (envrc-global-mode))

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package eat
  :custom
  (eat-kill-buffer-on-exit t)
  :bind (("C-c o t" . eat-project)))

(use-package popper
  :custom
   (popper-reference-buffers '("\*Messages\*"
			      "Output\*$"
			      "\\*Async Shell Command\\*"
			      help-mode
			      compilation-mode
			      "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
			      "^\\*shell.*\\*$" shell-mode ;shell as a popup
			      "^\\*term.*\\*$" term-mode ;term as a popup
			      "^\\*eat.*\\*$" eat-mode ;eat as a popup
			       ))
   (popper-window-height 0.33)
   (popper-echo-lines 1)
   (popper-mode-line nil)
  :init
  (popper-mode 1)
  (popper-echo-mode 1)
  :bind (("C-c t t" . popper-toggle-latest)
           ("C-c t c" . popper-cycle)
           ("C-c t p" . popper-toggle-type)))

(provide 'init)
;;; init.el ends here
