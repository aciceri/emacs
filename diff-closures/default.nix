{inputs, ...}: {
  perSystem = {
    pkgs,
    self',
    ...
  }: {
    packages.diff-closures = let
      diff-closures-script = pkgs.writeText "diff-closures-script" ''
	echo ciao
	export NIX_SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
        # nix --extra-experimental-features "nix-command flakes" \
	#   store diff-closures --derivation \
        #   github:aciceri/emacs/master#ccrEmacs \
        #   "${inputs.self}#ccrEmacs" \
        #   | ${pkgs.ansifilter}/bin/ansifilter --text
      '';
    in pkgs.runCommandNoCC
      "diff-closures"
      {
	__impure = true;
      }
      ''
         ${pkgs.bubblewrap}/bin/bwrap \
	   --dir /run \
	   --dev /dev \
	   --bind /build /build \
	   --bind /proc /proc \
	   --bind /etc /etc \
	   --chdir /build \
	   --setenv PATH "${pkgs.nixVersions.nix_2_14}/bin:${pkgs.busybox}/bin" \
	   --ro-bind /nix/store /nix/store \
	   --share-net \
	   ${pkgs.bash}/bin/bash ${diff-closures-script} > $out  
      '';

    apps.diff-closures.program = "${self'.packages.diff-closures}/bin/diff-closures";
  };
}
