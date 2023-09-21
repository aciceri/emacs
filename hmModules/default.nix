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
            enable = true;
            client.enable = true;
            defaultEditor = true;
            socketActivation.enable = false;
            startWithUserSession = true;
          };
          systemd.user.sessionVariables = {
            # TODO user `gpgconf --list-dirs agent-ssh-socket`
            # and better manage when gpg is not installed (do not make magit crash)
            SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
          };
          home.packages = with pkgs;
            [
              binutils
              (ripgrep.override {withPCRE2 = true;})
              gnutls
              fd
              hunspell
              python3
              imagemagick
              ffmpegthumbnailer
              mediainfo
              unzipNLS
              pkgs.tree-sitter-grammars.tree-sitter-nix
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
                $DRY_RUN_CMD ln -s "$HOME/.config/emacs" "$HOME/emacs"
              fi
              $DRY_RUN_CMD ln -sfn ${self.packages.${pkgs.system}.treesitGrammars} "$HOME/.config/emacs/tree-sitter"
            '';
          };
        };
    };
  };
}
