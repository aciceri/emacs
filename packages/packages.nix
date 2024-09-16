pkgs: epkgs: let
  inherit (pkgs) lib;
  inherit (epkgs) melpaPackages nongnuPackages elpaPackages;

  depsPerPackage = {
    indent-bars = [elpaPackages.compat];
    notmuch-notify = [melpaPackages.alert melpaPackages.notmuch];
    copilot = [melpaPackages.editorconfig melpaPackages.dash melpaPackages.s melpaPackages.f];
  };

  overrideAttrsPerPackage = {
    meow-tree-sitter = old: {
      installPhase = old.installPhase + ''
	mkdir -p $out/share/emacs/site-lisp
        cp -R ${old.src}/queries $out/share/emacs/site-lisp
      '';
    };
  };

  # *Attrset* containig extra emacs packages from flake inputs
  extraPackages = lib.mapAttrs (inputName: input: let
    deps = depsPerPackage.${inputName} or [];
    build = epkgs.trivialBuild {
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
    (with epkgs.melpaPackages; [
      meow
      meow-tree-sitter
      dracula-theme
      nord-theme
      catppuccin-theme
      modus-themes
      # solaire-mode
      nerd-icons
      nerd-icons-completion
      nerd-icons-ibuffer
      nerd-icons-dired
      ligature
      treemacs-nerd-icons
      eshell-syntax-highlighting
      fish-completion # fish completion for eshell
      eshell-prompt-extras
      eshell-atuin
      eshell-command-not-found
      clipetty
      sideline
      consult-eglot
      # sideline-flymake
      rainbow-delimiters
      vertico
      marginalia
      consult
      orderless
      embark
      embark-consult
      magit
      magit-delta
      magit-todos
      difftastic
      with-editor
      diff-hl
      corfu
      cape
      which-key
      nix-mode
      nix-ts-mode
      agenix
      zig-mode
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
      org-modern
      org-roam
      visual-fill-column
      consult-org-roam
      pass
      password-store-otp
      eldoc-box
      go-translate
      notmuch
      consult-notmuch
      poly-org
      casual-calc
      gptel
      agenix
      # org-re-reveal # FIXME very not nice hash mismatch when building
      # gptel # TODO uncomment when there will be a new release including GPT-4o 
    ]) ++ (with elpaPackages; [
      delight
      kind-icon
      vertico-posframe
      ef-themes
    ]) ++ (with nongnuPackages; [
      eat
      corfu-terminal
    ]); 
in
mainPackages
  ++ (builtins.attrValues extraPackages)
  # Playing with EAF
  ++ [
    # Disabled because pymupdf was broken
    # (pkgs.callPackage ./eaf.nix {
    #   inherit (epkgs) melpaBuild;
    #   inherit (melpaPackages) ctable deferred epc s;
    # })
  ]
