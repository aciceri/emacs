pkgs: epkgs:
with epkgs; [
  meow
  dracula-theme
  solaire-mode
  nerd-icons
  nerd-icons-completion
  nerd-icons-ibuffer
  nerd-icons-dired
  treemacs-nerd-icons
  eat
  eshell-syntax-highlighting
  clipetty
  sideline
  sideline-flymake
  rainbow-delimiters
  delight
  vertico
  marginalia
  consult
  orderless
  embark
  embark-consult
  magit
  magit-delta
  magit-todos
  diff-hl
  corfu
  corfu-terminal
  kind-icon
  cape
  which-key
  # nix-mode
  (nix-ts-mode.overrideAttrs (_: {
    src = pkgs.nix-ts-mode-source;
  }))
  unisonlang-mode
  purescript-mode
  dhall-mode
  envrc
  inheritenv
  popper
  paredit
  yaml-mode
  hl-todo
  markdown-mode
  haskell-mode
  terraform-mode
  diredfl
  (pkgs.callPackage ./indent-bars.nix {
    src = pkgs.indent-bars-source;
    inherit (epkgs) trivialBuild compat;
  })
  (pkgs.callPackage ./combobulate.nix {
    src = pkgs.combobulate-source;
    inherit (epkgs) trivialBuild;
  })
  org-roam
  consult-org-roam

  ement
]
