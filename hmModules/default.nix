{self, inputs, ...}: {
  flake.overlays.default = self: super: {
    ccrEmacs = self.packages.${self.system}.ccrEmacs;
  };
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
          nixpkgs.overlays = [(self: super: {
            ccrEmacs = super.hello;
          })];
          programs.emacs = {
            enable = true;
            package = ccrEmacsConfig.package;
          };
          services.emacs = {
            enable = true;
            client.enable = true;
            defaultEditor = true;
            socketActivation.enable = true;
            startWithUserSession = true;
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
            PATH=$PATH:${lib.makeBinPath (with pkgs; [ git openssh ])}
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
