pkgs: epkgs:
with epkgs; let
  inherit (pkgs) lib;

  depsPerPackage = {
    indent-bars = [compat];
    chatgpt = [polymode];
    copilot = [editorconfig dash s];
    notmuch-notify = [alert notmuch];
  };

  overrideAttrsPerPackage = {
    copilot = old: {
      postInstall = ''
        cp -r "$src/dist" "$LISPDIR"
      '';
    };
  };

  # *Attrset* containig extra emacs packages from flake inputs
  extraPackages = lib.mapAttrs (inputName: input: let
    deps = depsPerPackage.${inputName} or [];
    build = trivialBuild {
      pname = inputName;
      src = input.outPath;
      version = input.shortRev;
      propagatedUserEnvPkgs = deps;
      buildInputs = deps;
    };
  in
    build.overrideAttrs (overrideAttrsPerPackage.${inputName} or (_: _)))
  pkgs.extra-package-inputs;

  # *List* containing emacs packages from (M)ELPA
  mainPackages =
    builtins.filter
    # if an extra package has the same name then give precedence to it
    (package: ! builtins.elem package.pname (builtins.attrNames extraPackages))
    [
      meow
      dracula-theme
      modus-themes
      # solaire-mode
      nerd-icons
      nerd-icons-completion
      nerd-icons-ibuffer
      nerd-icons-dired
      ligature
      treemacs-nerd-icons
      eat
      eshell-syntax-highlighting
      fish-completion # fish completion for eshell
      eshell-prompt-extras
      esh-autosuggest
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
      with-editor
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
      yaml-mode
      hl-todo
      markdown-mode
      haskell-mode
      terraform-mode
      diredfl
      org-roam
      visual-fill-column
      consult-org-roam
      pass
      password-store-otp
      vertico-posframe
      eldoc-box
      go-translate
      notmuch
    ];
in
  mainPackages ++ (builtins.attrValues extraPackages)
