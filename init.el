;; package --- Summary
;;; Commentary:
;; TODO
;; - org-roam
;; - org goodies
;; - persp-mode
;; - understand how cape works
;; - dirvish
;; - prettify eshell
;; - projectile

;;; Code:

(require 'setup)

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")


(setup ligatures
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't)
)


(setup flycheck
  (:require flycheck)
  (:hook-into prog-mode)
  (if (display-graphic-p) (:hook flycheck-posframe-mode) (:hook flycheck-inline-mode)))

(setup eglot
  (:with-mode eglot--managed-mode (:hook (lambda () (flymake-mode -1)))))

(setup emacs
  (:option use-dialog-box nil
	   use-short-answers t
	   native-comp-async-report-warnings-errors nil
	   inhibit-startup-message t
	   visible-bell t
	   scroll-conservatively 101 ;; more than 100 => redisplay doesn't recenter point
           scroll-margin 3
	   temporary-file-directory "~/.emacs-saves/"
	   backup-directory-alist `(("." . ,temporary-file-directory))
	   auto-save-files-name-transforms `((".*" ,temporary-file-directory t))
	   backup-by-copying t)
  (defun ccr/set-faces ()
    (set-face-attribute 'default nil :font "Fira Code 12")
    (set-face-background 'vertical-border (face-background 'default))
    (meow--prepare-face)
    (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃)))
  (if (daemonp)
    (add-hook 'server-after-make-frame-hook #'ccr/set-faces)
    (ccr/set-faces))
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (global-hl-line-mode -1)
  (global-auto-revert-mode t)
  (show-paren-mode 1)
  (column-number-mode 1)
  (set-frame-parameter (selected-frame) 'alpha '(98 98))
  (add-to-list 'default-frame-alist '(alpha 98 98))
  ;; FIXME when running running Emacs as daemon
  ;; (advice-add 'enable-theme
  ;; 	      :after
  ;; 	      #'(lambda (&rest rest)
  ;; 		  (set-face-background 'vertical-border (face-background 'default))
  ;; 		  (let* ((bg-color (face-attribute 'default :background))
  ;; 			 (ansi-command (format "\033]11;#%s\007"
  ;; 					       (string-remove-prefix "#" bg-color))))
  ;; 		    (send-string-to-terminal ansi-command))))
  (defun reset-terminal () (send-string-to-terminal "\033c"))
  (add-hook 'kill-emacs-hook #'reset-terminal)
  (ef-themes-select 'ef-day)

  (defun ccr/reload-emacs ()
    (interactive)
    (load-file "~/.config/emacs/init.el"))

  (defun ccr/run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

  (defun ccr/run-in-vterm (command &optional buffer-name)
    "Run command in vterm"
    (interactive)
    (with-current-buffer (vterm (if buffer-name buffer-name (concat "*" command "*")))
      (set-process-sentinel vterm--process #'ccr/run-in-vterm-kill)
      (vterm-send-string command)
      (vterm-send-return)))

  (defun ccr/nixos-rebuild-test ()
    "Run `nixos-rebuild test`"
    (interactive)
    (ccr/run-in-vterm
     "sudo nixos-rebuild test --flake /home/ccr/fleet"
     "*vterm-nixos-rebuild-test*"))

  (defun ccr/nixos-rebuild-switch ()
    "Run `nixos-rebuild switch`"
    (interactive)
    (ccr/run-in-vterm
     "sudo nixos-rebuild switch --flake /home/ccr/fleet"
     "*vterm-nixos-rebuild-switch*")))
  
(setup prog-mode (:hook display-line-numbers-mode hl-line-mode))

(setup which-key :option (which-key-mode))

(setup magit-mode (:hook magit-delta-mode))

(setup nix-mode (:hook eglot-ensure tree-sitter-hl-mode)
       (global-nix-prettify-mode))

(setup haskell-mode (:hook eglot-ensure tree-sitter-hl-mode))

(setup purescript-mode)

(setup rustic-mode
  (:hook eglot-ensure tree-sitter-hl-mode)
  (push 'rustic-clippy flycheck-checkers)
  (:option rustic-lsp-client 'eglot))

(setup terraform-mode (:hook eglot-ensure tree-sitter-hl-mode))

(setup yaml-mode (:hook tree-sitter-hl-mode))

(setup sh-mode (:hook tree-sitter-hl-mode))

(setup lisp
  (:hook enable-paredit-mode)
  (:with-mode emacs-lisp-mode (:hook enable-paredit-mode)))

(setup envrc
  (require 'inheritenv)
  (envrc-global-mode))

(setup windmove
  (defcustom ccr/v-resize-amount 4
    "How many rows move when calling `ccr/v-resize`"
    :type 'integer)
  (defcustom ccr/h-resize-amount 4
    "How many columns move when calling `ccr/h-resize`"
    :type 'integer)  ;; (defcustom ccr/resize-rows 4)
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
  (:global "C-c w k" #'windmove-up
	   "C-c w l" #'windmove-right
	   "C-c w j" #'windmove-down
	   "C-c w h" #'windmove-left
	   "M-k" #'windmove-up
	   "M-l" #'windmove-right
	   "M-j" #'windmove-down
	   "M-h" #'windmove-left
	   "C-c w <up>" #'windmove-up
	   "C-c w <right>" #'windmove-right
	   "C-c w <down>" #'windmove-down
	   "C-c w <left>" #'windmove-left
	   "C-c w q" #'delete-window
	   "C-c w K" #'windmove-delete-up
	   "C-c w L" #'windmove-delete-right
	   "C-c w J" #'windmove-delete-down
	   "C-c w H" #'windmove-delete-left
	   "C-c w x" #'kill-buffer-and-window
	   "C-c w v" #'split-window-right
	   "C-c w s" #'split-window-below
	   "C-c w V" #'ccr/v-resize
	   "C-c w S" #'ccr/h-resize))

(setup (:package vertico marginalia consult orderless embark corfu)
  ;; Vertico
  (:option vertico-mouse-mode t
           vertico-reverse-mode t
           vertico-count 8
           vertico-resize t
           vertico-cycle t
           vertico-mode t)
  (:bind-into vertico-map
    "DEL" #'vertico-directory-delete-char
    "C-DEL" #'vertico-directory-delete-word)
  ;; Marginalia
  (:option marginalia-mode t
	   marginalia-aligh 'right)
  (:bind-into minibuffer-local-map
    "M-A" marginalia-cycle)
  ;; Consult
  (:global [remap switch-to-buffer] #'consult-buffer
           [remap goto-line] #'consult-goto-line
           [remap imenu] #'consult-imenu
           [remap project-switch-to-buffer] #'consult-project-buffer
           "C-c b b" #'consult-project-buffer
           "C-c b B" #'consult-buffer
           "C-c g l" #'consult-goto-line
           "C-c b i" #'consult-imenu
           "C-c f f" #'consult-find
           "C-c F" (if (executable-find "rg")
		    #'consult-ripgrep
		    #'consult-grep)
           "C-c f" consult-find
           "C-c l" consult-line
           "C-c m" consult-mark
           "C-c o o" consult-outline
           "C-c e" #'consult-flycheck)
  (:option xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref)
  ;; Orderless
  (:option completion-styles '(orderless))
  ;; Embark
  (:global "C-'" #'embark-act
           "C-=" #'embark-dwim)
  ;; Corfu
  (global-corfu-mode)
  (unless (display-graphic-p) (corfu-terminal-mode))
  (:option completion-cycle-threshold 3
	   tab-always-indent 'complete
	   kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (:bind-into corfu-map
    "M-d" #'corfu-doc-toggle
    "M-l" #'corfu-show-location
    "SPC" #'corfu-insert-separator)
  ;; Cape
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; All the icons completion
  (all-the-icons-completion-mode)
  )

;; TODO configure corfu, cape, dabbrev, kind-icons
;; Work in progress:

(defun ccr/ignore-elisp-keywords (cand)
  (or (not (keywordp cand))
      (eq (char-after (car completion-in-region--data)) ?:)))

(defun ccr/setup-elisp ()
  (setq-local completion-at-point-functions
              `(,(cape-super-capf
                  (cape-capf-predicate
                   #'elisp-completion-at-point
                   ;; #'my/ignore-elisp-keywords
		   )
                  #'cape-dabbrev)
                cape-file)
              cape-dabbrev-min-length 5))

(setup eshell
  (:require projectile)
  (:hook esh-autosuggest-mode)
  (:with-mode eshell-load (:hook #'eat-eshell-mode #'eat-eshell-visual-command-mode))
  (eshell-syntax-highlighting-global-mode +1)
  (defun ccr/projectile-run-eat (&optional arg)
    (interactive "P")
    (let ((project (projectile-acquire-root)))
      (projectile-with-default-dir project
				   (let ((eat-buffer-name (projectile-generate-process-name "eat" arg project)))
				     (eat)))))
  (:global "C-c o e" #'projectile-run-eshell
	   "C-c o t" #'ccr/projectile-run-eat
	   "C-c o v" #'projectile-run-vterm))

(setup vterm
  (:option vterm-timer-delay 0.01))

(setup magit
  (:global "C-c o g" magit))

(setup yaml-mode
  (add-to-list 'auto-mode-alist '("\\.y(a?)ml\\'" . yaml-mode))
   (add-hook 'yaml-mode-hook ;; TODO use a more idiomatic style according to setup.el
	     #'(lambda ()
		(define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(setup hl-todo (global-hl-todo-mode))

(setup projectile
  (:option projectile-project-search-path '("~/projects/" "~/mlabs/"))
  ;; (:bind-into projectile-command-map
  ;;   "DEL" #'vertico-directory-delete-char
  ;;   "C-DEL" #'vertico-directory-delete-word)
  )

(setup meow
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

(setup popper
  (:option
   popper-reference-buffers '("\*Messages\*"
			      "Output\*$"
			      "\\*Async Shell Command\\*"
			      "\*nixos-rebuild-(test|switch)\*"
			      help-mode
			      compilation-mode
			      "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
			      "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
			      "^\\*term.*\\*$"   term-mode   ;term as a popup
			      "^\\*eat.*\\*$"   term-mode   ;eat as a popup
			      "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
			      )
   popper-echo-lines 1
   popper-mode-line nil
   )
  (popper-mode 1)
  (popper-echo-mode 1)
  (:global "C-c t t" popper-toggle-latest
         "C-c t c" popper-cycle
         "C-c t p" popper-toggle-type))

(setup org-roam
  (:option org-roam-directory "~/roam/"
	   org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (:global "C-c n l" org-roam-buffer-toggle
	   "C-c n f" org-roam-node-find
	   "C-c n g" org-roam-graph
	   "C-c n i" org-roam-node-insert
	   "C-c n c" org-roam-capture
	   "C-c n j" org-roam-dailies-capture-today)
  (org-roam-db-autosync-mode))



(setup diff-hl
  (global-diff-hl-mode 1)
  (defun maybe-diff-hl-margin-mode ()
    (unless (display-graphic-p) (diff-hl-margin-mode)))
  (when (daemonp)
    ;; FIXME not optimal, after the first time that a client creates a frame all the other frames
    ;; will use `diff-hl-margin-mode`, even if they are graphical
    (add-hook 'server-after-make-frame-hook #'maybe-diff-hl-margin-mode)))

(setup dirvish
  (dirvish-override-dired-mode)
  (diredfl-global-mode)
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
  (pdf-tools-install)
  (:option dirvish-use-header-line nil
	   dirvish-attributes '(all-the-icons
				file-time
				file-size
				collapse
				subtree-state
				vc-state
				git-msg)
	   delete-by-moving-to-trash t
	   dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (:with-mode dirvish-directory-view-mode (:hook diredfl-mode)))

;; TODO play with skacle/sway.el

;; (setq shackle-default-rule '(:frame t)
;;       shackle-display-buffer-frame-function 'sway-shackle-display-buffer-frame)

;; (sway-socket-tracker-mode)
;; (sway-undertaker-mode) ;; If you want to use :dedicate, read below.
;; (sway-x-focus-through-sway-mode) ;; Temporary workaround for Sway bug 6216

;; ;; To use with pgtk Emacs, you *must* ensure that frame names are
;; ;; unique.  This is a way to achieve this:
;; (setq frame-title-format '("%b — GNU Emacs [" (:eval (frame-parameter (selected-frame) 'window-id)) "]"))
;; ;; You may want to make this nicer, feel free to improve.  The only
;; ;; requirement is that each frame must have a strictly unique name on
;; ;; that Emacs instance (name collisions with other programs, including
;; ;; other Emacs processes, are not an issue )

;; (setq shackle-rules
;;       `(("*Help*" :select t :frame t :dedicate t))

;;       shackle-default-rule '(:frame t)
;;       shackle-default-size 0.4
;;       shackle-inhibit-window-quit-on-same-windows t
;;       shackle-display-buffer-frame-function 'sway-shackle-display-buffer-frame)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode diff-hl org-roam-ui consult-flycheck dirvish flycheck-inline orderless diredfl rustic ef-themes corfu-doc which-key popper envrc flycheck-posframe pdf-tools embark-consult vertico tree-sitter-langs paredit ligature hl-todo vterm yaml-mode eat kind-icon terraform-mode sway corfu-terminal shackle consult-projectile magit-delta nix-mode code-review haskell-mode delight marginalia meow eshell-syntax-highlighting all-the-icons-completion esh-autosuggest cape setup)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
