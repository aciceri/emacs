{inputs, ...}: {
  perSystem = {
    self',
    inputs',
    pkgs,
    ...
  }: {
    packages = {
      ccrEmacs =
        (inputs'.emacs-overlay.packages.emacsPgtk.override {
          treeSitterPlugins = builtins.attrValues (builtins.removeAttrs pkgs.tree-sitter-grammars ["recurseForDerivations"]);
        })
        .overrideAttrs (_: {
          name = "ccr-emacs-${inputs.emacs-src.rev}";
          src = inputs.emacs-src.outPath;
          version = inputs.emacs-src.rev;
        });
      default = self'.packages.ccrEmacs;
    };
  };
}
