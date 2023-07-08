{
  inputs,
  self,
  ...
}: {
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
      tree-sitter-grammars =
        super.tree-sitter-grammars
        // {
          tree-sitter-rust = super.tree-sitter-grammars.tree-sitter-rust.overrideAttrs (_: {
            nativeBuildInputs = [self.nodejs self.tree-sitter];
            configurePhase = ''
              tree-sitter generate --abi 13 src/grammar.json
            '';
          });
        };
    });

    packages = {
      ccrEmacsWithoutPackages =
        (inputs'.emacs-overlay.packages.emacs-unstable.override {
          # treeSitterPlugins =
          #   builtins.attrValues
          # (builtins.removeAttrs pkgs.tree-sitter-grammars ["recurseForDerivations"]);
          withNS = false;
          withX = false;
          withGTK2 = false;
          withGTK3 = false;
          withWebP = false;
        })
        .overrideAttrs (old: {
          name = "ccr-emacs";
          version = "29";
        });
      ccrEmacs =
        (emacsPackagesFor self'.packages.ccrEmacsWithoutPackages).emacsWithPackages
        (import ./packages.nix pkgs);
      default = self'.packages.ccrEmacs;
    };

    apps = {
      ccrEmacs.program = "${self'.packages.ccrEmacs}/bin/emacs";
      default = self'.apps.ccrEmacs;
    };
  };
}
