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
  ;; TODO set following only when on terminal (where wavy underlines are not shown)
  ;; (set-face-attribute 'flymake-error  nil :inverse-video t)
  ;; (set-face-attribute 'flymake-warning  nil :inverse-video t)
  ;; (set-face-attribute 'flymake-note  nil :inverse-video t)
  :custom
  (flymake-mode-line-lighter "Fly")
  :hook prog-mode)

(use-package eglot
  :custom
  ;; Tricks that should make Emacs faster
  (eglot-events-buffer-size 0) ; disable events logging, it should be enabled only when debuggigng LSP servers
  (eglot-sync-connect-nil 0) ; disable UI freeze when opening big files
  (eglot-connect-timeout nil) ; never timeout
  )

(use-package emacs
  :bind (("<mouse-4>" . scroll-down-line)
	 ("<mouse-5>" . scroll-up-line)
	 (("C-x F" . recentf-open)))
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
  (scroll-bar-mode -1)
  (xterm-mouse-mode +1)
  (tool-bar-mode -1)
  (global-hl-line-mode -1)
  (global-auto-revert-mode t)
  (show-paren-mode +1)
  (column-number-mode +1)
  (add-to-list 'default-frame-alist '(font . "Iosevka Comfy-13"))
  (recentf-mode +1)
  (fset #'jsonrpc--log-event #'ignore) ; this should be enabled only when needed, otherwise makes Emacs slower
  (defun ccr/reload-emacs ()
    (interactive)
    (load-file "~/.config/emacs/init.el"))
  )

(use-package tramp
  :config
  ;; TODO ugly `ccr' hardcoded
  (add-to-list 'tramp-remote-path "/home/ccr/.nix-profile/bin" 't)
  (add-to-list 'tramp-remote-path "/etc/profiles/per-user/ccr/bin" 't)
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin" 't)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

(use-package dracula-theme
  :init
  ;; TODO find better colors (bright white and bright black should be different!)
  ;; this is used by auto suggestions while typing in eat
  (set-face-attribute 'ansi-color-bright-white nil :foreground "grey" :background "grey")
  (set-face-attribute 'ansi-color-bright-black nil :foreground "grey" :background "grey")
  ;; (set-face-attribute 'ansi-color-bright-magenta nil :foreground "magenta" :background "magenta")
  (set-face-background 'match (color-lighten-name (face-background 'menu) 30))
  :config
  ;; TODO abstract the following paradigm in a new use-package keyword :after-frame-one-time
  (defvar ccr/theme-loaded nil "Indicate if the theme has already been loaded")
  ;; load the theme only when a frame is created for the first time (not every time)
  :hook (server-after-make-frame . (lambda ()
				     (when (not ccr/theme-loaded)
				       (setq ccr/theme-loaded 't)
				       (load-theme 'dracula t)
				       (custom-theme-set-faces 'dracula '(default ((t (:background "black")))))
				       (load-theme 'dracula t)
				       ;; HACK Since dracula doesn't directly expose colors as faces we load
				       ;; term in order to load them as term faces (which instead itq provides)
				       ;; Then we assign these faces to eat faces
				       ;; TODO shouldn't this be moved to eat's use-package section?
				       ;; (require 'term)
				       ;; (let ((colors '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")))
				       ;; 	 (dolist (color colors)
				       ;; 	   (set-face-attribute (intern (format "eat-term-color-%s" color)) nil :inherit (intern (format "term-color-%s" color)))))
				       ))))

(use-package solaire-mode
  :init
  (solaire-global-mode +1)
  :custom ((solaire-mode-themes-to-face-swap '(dracula))))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package clipetty
  :delight
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
  (indent-bars-treesit-support t)
  (indent-bars-spacing-override 2)
  ;; (indent-bars-treesit-wrap '())
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.4))
  (indent-bars-no-stipple-char (string-to-char "┋"))
  (indent-bars-prefer-character 't) ;; so it works also in terminal
  )

(use-package diredfl
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
    :type 'integer
    :group 'ccr)
  (defcustom ccr/h-resize-amount 4
    "How many columns move when calling `ccr/h-resize`"
    :type 'integer
    :group 'ccr
    )
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
  (corfu-popupinfo-mode)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :custom
  (completion-cycle-threshold nil)
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

(use-package which-key :delight :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package magit
  :hook ((magit-mode . magit-delta-mode))
  :custom
  (magit-todos-keyword-suffix "([^)]+):")
  :config
  (magit-todos-mode +1)
  :bind (("C-c o g" . magit)))

(use-package sideline
  :delight
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line)
  (sideline-backends-right '(sideline-flymake))
  :config
  ;; FIXME https://github.com/emacs-sideline/sideline/issues/13
  (require 'sideline)
  (defun ccr-sideline--align (&rest lengths)
    "Align sideline string by LENGTHS from the right of the window."
    (list (* (window-font-width)
	    (+ (apply #'+ lengths) (if (display-graphic-p) 1 3)))))
  (advice-add 'sideline--align :override #'ccr-sideline--align))

;; FIXME there is something deeply wrong about how nix is configured here
;; (use-package nix-mode
;;   :delight nix-prettify-mode
;;   :config
;;   (global-nix-prettify-mode))

(use-package agenix
  :after (inheritenv)
  :custom
  ((agenix-age-program "/nix/store/d1gkdszgmmcabz4pb06h8pvzkzml69g5-age-1.1.1/bin/age")
  (agenix-key-files '("~/.ssh/id_rsa" "~/.ssh/id_ed25519" "~/.ssh/mlabs")))
  :config
  (inheritenv-add-advice 'agenix--process-exit-code-and-output)
)

(use-package nix-ts-mode
  :custom ((nix-ts-mode--embed-bash nil))
  :hook (
	 (nix-ts-mode . (lambda ()
			  (require 'eglot)
			  (add-to-list 'eglot-server-programs
				       '(nix-ts-mode . ("nil")))
			  (eglot-ensure)))
	 (nix-ts-mode . electric-pair-mode)
	 (nix-ts-mode . (lambda () (setq indent-bars-spacing-override 2) (indent-bars-mode)))
	 )
  :mode "\\.nix\\'"
  )

(use-package python-ts-mode
  :hook ((python-ts-mode . (lambda ()
			  (require 'eglot)
			  (add-to-list 'eglot-server-programs
				       '(python-ts-mode . ("jedi-language-server")))
			  (eglot-ensure))))
  :mode "\\.py\\'")

(use-package haskell-mode
  :hook ((haskell-mode . eglot-ensure)
	 (haskell-mode . tree-sitter-hl-mode)))

(use-package purescript-mode
  :hook (purescript-mode .  turn-on-purescript-indentation))

(use-package terraform-mode
  :after eglot
  :config
  (add-to-list 'eglot-server-programs
	       '(terraform-mode . ("terraform-lsp")))
  :hook ((terraform-mode . eglot-ensure)
	 (terraform-mode . tree-sitter-hl-mode)))

(use-package yaml-mode
  :hook (yaml-mode . tree-sitter-hl-mode))

(use-package sh-mode
  :hook (sh-mode . tree-sitter-hl-mode))

;; FIXME
;; (use-package mmm-mode
;;   :config
;;   (mmm-add-group 'nix-sh
;; 		 '((sh-command
;; 		    :submode sh-mode
;; 		    :face mmm-output-submode-face
;; 		    :front "[^'a-zA-Z]''[^']"
;; 		    :back "''[^$\\']"
;; 		    :include-front t
;; 		    :front-offset 4
;; 		    :end-not-begin t)))
;;   (mmm-add-mode-ext-class 'nix-mode "\\.nix\\'" 'nix-sh))

(use-package paredit
  :delight
  :hook ((lisp-mode . enable-paredit-mode)
	 (emacs-lisp-mode . enable-paredit-mode)))

(use-package eldoc
  :delight)

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1))

(use-package envrc
  :config
  (envrc-global-mode +1))

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package eat
  :init
  ;; FIXME if we not load eat on startup then adding more non bound keys in :config
  ;; will a cause "nesting exceeds `max-lisp-eval-depth'" on (eat-reload)
  (eat)
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (add-to-list 'eat-semi-char-non-bound-keys '[?\e 104]) ; M-h
  (add-to-list 'eat-semi-char-non-bound-keys '[?\e 106]) ; M-j
  (add-to-list 'eat-semi-char-non-bound-keys '[?\e 107]) ; M-k
  (add-to-list 'eat-semi-char-non-bound-keys '[?\e 108]) ; M-l
  (eat-update-semi-char-mode-map)
  (eat-reload)
  :bind (("C-c o t" . eat-project))
  ;; FIXME doesn't work well
  ;; ((eat-mode . compilation-shell-minor-mode))
  )

(use-package eshell
  :init (require 'eshell) ;; this slows down Emacs startup but it's needed when starting eshell with
  			  ;; emacsclient --eval before opening another eshell buffer directly from inside Emacs
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  :custom ((eshell-prefer-lisp-functions t)
	   (eshell-history-size 10000))
  :config
  (defun ccr/start-eshell () ;; Used from outside Emacs by emacsclient --eval
    (eshell 'N)
    (add-hook 'kill-buffer-hook 'delete-frame nil 't)) ;; destroy frame on exit

  (defun ccr/eshell-history ()
    "Interactive search eshell history."
    (interactive)
    (require 'em-hist)
    (save-excursion
      (eshell-bol)
      (let* ((start-pos (point))
    	     (end-pos (line-end-position))
    	     (input (buffer-substring-no-properties start-pos end-pos)))
	(message input)
        (let* ((history (delete-dups (when (> (ring-size eshell-history-ring) 0)
    				       (ring-elements eshell-history-ring))))
	       (history-highlighted (mapcar #'(lambda (cmd)
					       (with-temp-buffer
							      (insert cmd)
							      (forward-line 0)
							      (eshell-syntax-highlighting--parse-and-highlight 'command (point-max))
							      (add-face-text-property (point-min) (point-max) '(:background nil))
							      (buffer-string)))
						history))
	       (command (completing-read
			 "Command: "
			 history-highlighted
			 nil
			 nil
			 input
			 )))
    	  (kill-region start-pos end-pos)
	  (insert command)
    	  )))
    (end-of-line))

  (defun ccr/wrap-eshell-write-history (orig-fun &optional filename &rest _)
    (apply orig-fun `(,filename 't)))

  ;; Wrapping this in order to merge histories from different shells
  (advice-add 'eshell-write-history
	      :around #'ccr/wrap-eshell-write-history)
  
  (add-to-list 'eshell-modules-list 'eshell-tramp) ;; to use sudo in eshell
  ;; :hook ((eshell-load . eat-eshell-mode)
  ;; 	 (eshell-load . eat-eshell-visual-command-mode))
  :bind (("C-c o e" . project-eshell)
	 :map eshell-mode-map
	 ("C-r" . ccr/eshell-history))) ;; i.e. just C-r in semi-char-mode

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-syntax-highlighting
  :custom
  ((eshell-syntax-highlighting-highlight-in-remote-dirs nil))
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package popper
  :custom
  (popper-reference-buffers '("\*Messages\*"
			      "Output\*$"
			      "\\*Async Shell Command\\*"
			      (completion-list-mode . hide)
			      help-mode
			      compilation-mode
			      "^\\*.+-eshell.*\\*$" eshell-mode ;eshell as a popup
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

(use-package org
  :hook ((org-mode . variable-pitch-mode)
	 (org-mode . visual-line-mode)
	 (org-mode . visual-fill-column-mode))
  :custom ((org-hide-emphasis-markers t)
	   (visual-fill-column-center-text t)
	   (visual-fill-column-width 100)
	   (fill-column 100))
  :config
  ;; FIXME the following doesn't work when using the daemon, it should be executed only
  ;; one time after the first frame is created 
  (set-face-font 'variable-pitch "Dejavu Serif 14")
  (set-face-font 'fixed-pitch "Iosevka 14")

  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-tag nil :inherit 'fixed-pitch :weight 'bold :height 0.8)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  )

(use-package org-roam)

(use-package consult-org-roam
  :delight
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ("C-c n f" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(use-package chatgpt
  :config
  (dolist (e '(("spiega" . "Spiega il seguente")
	       ("documenta" . "Documenta il seguente usando la sintassi appropriata in modo che possa essere inserito nel codice")
	       ))
    (push e chatgpt-code-query-map))
  :bind
  ("C-c i" . chatgpt-query))

(use-package copilot
  :custom
  (copilot-max-char -1)
  :hook (prog-mode org-mode)
  :bind (("C-<tab>" . copilot-accept-completion)))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved)
;; End:
