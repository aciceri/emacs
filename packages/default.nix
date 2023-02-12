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
    ...
  }: let
    inherit (inputs.emacs-overlay.overlays.default pkgs pkgs) emacsPackagesFor;
  in {
    packages = {
      ccrEmacsWithoutPackages =
        (inputs'.emacs-overlay.packages.emacsPgtk.override {
          treeSitterPlugins =
            builtins.attrValues
            (builtins.removeAttrs pkgs.tree-sitter-grammars ["recurseForDerivations"]);
        })
        .overrideAttrs (_: {
          name = "ccr-emacs-${inputs.emacs-src.rev}";
          src = inputs.emacs-src.outPath;
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
