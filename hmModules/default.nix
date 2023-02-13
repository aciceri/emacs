{
  self,
  inputs,
  ...
}: {
  flake.hmModules = {
    default = self.hmModules.ccrEmacs;
    ccrEmacs = {
      lib,
      pkgs,
      config,
      ...
    }: {
      options.ccrEmacs = {
        enable = lib.mkEnableOption "Enable custom Emacs";
        package = lib.mkOption {
          type = lib.types.package;
          description = "Custom Emacs package";
          default = self.packages.${pkgs.system}.ccrEmacs;
        };
      };
      config = let
        ccrEmacsConfig = config.ccrEmacs;
      in
        lib.mkIf ccrEmacsConfig.enable {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlays.default
          ];
          programs.emacs = {
            enable = true;
            package = ccrEmacsConfig.package;
          };
          services.emacs = {
            # TODO Re-enable, this is causing some problems with magit, probably it
            # would be sufficient disabling `socketActivation`
            enable = false;
            client.enable = false;
            defaultEditor = false;
            socketActivation.enable = false;
            startWithUserSession = false;
          };
          home.packages = with pkgs;
            [
              # TODO add epub-thumbnailer to nixpkgs
              nil
              terraform-lsp
              imagemagick
              ffmpegthumbnailer
              mediainfo
              unzipNLS
              binutils
              (ripgrep.override {withPCRE2 = true;})
              gnutls
              fd
              imagemagick
              sqlite
              maim
              jq
              xclip
              hunspell
            ]
            ++ (with hunspellDicts; [
              en_US-large
              it_IT
            ]);
          home.activation = {
            cloneCcrEmacsFlake = lib.hm.dag.entryAfter ["writeBoundary"] ''
              PATH=$PATH:${lib.makeBinPath (with pkgs; [git openssh])}
              if [ ! -d "$HOME/.config/emacs" ]; then
                $DRY_RUN_CMD git clone \
                  https://github.com/aciceri/emacs.git \
                  "$HOME/.config/emacs"
                ln -s "$HOME/.config/emacs" "$HOME/emacs"
              fi
            '';
          };
        };
    };
  };
}
