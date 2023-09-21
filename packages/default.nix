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
    _module.args.pkgs = inputs.nixpkgs.legacyPackages.${system}.extend (self: super: {
      extra-package-inputs = lib.mapAttrs' (inputName: input: {
        name = builtins.head (builtins.match "extra-package-(.*)" inputName);
        value = input;
      }) (lib.filterAttrs (inputName: _: ! builtins.isNull (builtins.match "extra-package-.*" inputName)) inputs);

      # Some tree-sitter grammars in nixpksg are built with a too new ABI
      # https://github.com/NixOS/nixpkgs/issues/209114
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
      ccrEmacsWithoutPackages = inputs'.emacs-overlay.packages.emacs-unstable.override {
        withPgtk = true;
      };
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
