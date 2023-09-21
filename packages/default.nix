{inputs, ...}: {
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay
  ];
  perSystem = {
    config,
    self',
    inputs',
    pkgs,
    final,
    lib,
    system,
    ...
  }: let
    inherit (inputs.emacs-overlay.overlays.default pkgs pkgs) emacsPackagesFor;
  in {
    # Some tree-sitter grammars in nixpksg are built with a too new ABI
    # https://github.com/NixOS/nixpkgs/issues/209114
    _module.args.pkgs = inputs.nixpkgs.legacyPackages.${system}.extend (self: super: {
      indent-bars-source = inputs.indent-bars;
      nix-ts-mode-source = inputs.nix-ts-mode;
      combobulate-source = inputs.combobulate;
      agenix-el-source = inputs.agenix-el;
      # tree-sitter-grammars =
      #   super.tree-sitter-grammars
      #   // {
      #     tree-sitter-rust = super.tree-sitter-grammars.tree-sitter-rust.overrideAttrs (_: {
      #       nativeBuildInputs = [self.nodejs self.tree-sitter];
      #       configurePhase = ''
      #         tree-sitter generate --abi 13 src/grammar.json
      #       '';
      #     });
      #   };
    });

    packages = {
      treesitGrammars = let
        all-grammars = pkgs.tree-sitter.withPlugins builtins.attrValues;
      in
        pkgs.runCommand "treesit-grammars" {} ''
          mkdir $out
          for f in ${all-grammars}/*
          do
            cp $f $out/"libtree-sitter-$(basename $f)"
          done
        '';
      ccrEmacsWithoutPackages =
        (inputs'.emacs-overlay.packages.emacs-unstable.override {
          withPgtk = true;
          # withNS = false;
          # withX = false;
          # withGTK2 = false;
          # withGTK3 = false;
          # withWebP = false;
        })
        .overrideAttrs (old: {
          name = "ccr-emacs";
          version = "29";
        });
      ccrEmacs = let
        emacs =
          (emacsPackagesFor self'.packages.ccrEmacsWithoutPackages).emacsWithPackages
          (import ./packages.nix pkgs);
      in
        pkgs.runCommand "emacs" {} ''
          cp -r ${emacs} emacs
          chmod 700 -R emacs
          mkdir emacs/share/emacs
          cp -r emacs $out
        '';
      default = self'.packages.ccrEmacs;
    };

    apps = {
      ccrEmacs.program = "${self'.packages.ccrEmacs}/bin/emacs";
      default = config.apps.ccrEmacs;
    };
  };
}
