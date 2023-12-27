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
  }: {
    _module.args.pkgs = lib.foldl (lp: lp.extend) inputs.nixpkgs.legacyPackages.${system} [
      (self: super: {
        extra-package-inputs = lib.mapAttrs' (inputName: input: {
          name = builtins.head (builtins.match "extra-package-(.*)" inputName);
          value = input;
        }) (lib.filterAttrs (inputName: _: ! builtins.isNull (builtins.match "extra-package-.*" inputName)) inputs);
      })
      inputs.emacs-overlay.overlays.package
      # Some tree-sitter grammars in nixpksg are built with a too new ABI
      # https://github.com/NixOS/nixpkgs/issues/209114
      # (_: _: {
      #   tree-sitter-grammars =
      #     super.tree-sitter-grammars
      #     // {
      #       tree-sitter-rust = super.tree-sitter-grammars.tree-sitter-rust.overrideAttrs (_: {
      #         nativeBuildInputs = [self.nodejs self.tree-sitter];
      #         configurePhase = ''
      #           tree-sitter generate --abi 13 src/grammar.json
      #         '';
      #       });
      #     };
      #   }
      # )
    ];

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
      ccrEmacsWithoutPackages = pkgs.emacs29.override {
        withSQLite3 = true;
        withWebP = true;
        withPgtk = true;
      };
      ccrEmacs =
        (pkgs.emacsPackagesFor config.packages.ccrEmacsWithoutPackages).emacsWithPackages
        (import ./packages.nix pkgs);
      default = config.packages.ccrEmacs;
    };

    apps = {
      ccrEmacs.program = "${config.packages.ccrEmacs}/bin/emacs";
      default = config.apps.ccrEmacs;
    };
  };
}
