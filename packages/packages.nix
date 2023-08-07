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
  clipetty
  sideline
  sideline-flymake
  delight
  vertico
  marginalia
  consult
  orderless
  embark
  embark-consult
  magit
  magit-delta
  diff-hl
  corfu
  corfu-terminal
  kind-icon
  cape
  which-key
  nix-mode
  unisonlang-mode
  purescript-mode
  dhall-mode
  envrc
  inheritenv
  popper
  paredit
  tree-sitter
  tree-sitter-langs
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
]
