{inputs, ...}: {
  perSystem = {
    pkgs,
    self',
    ...
  }: {
    packages.diff-closures = pkgs.writeShellApplication {
      name = "diff-closures";
      runtimeInputs = [pkgs.ansifilter];
      text = ''
        nix store diff-closures --derivation \
          github:aciceri/emacs/master#ccrEmacs \
          "${inputs.self}#ccrEmacs" \
        | ansifilter --text
      '';
    };

    apps.diff-closures.program = "${self'.packages.diff-closures}/bin/diff-closures";
  };
}
