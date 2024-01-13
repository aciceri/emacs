{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    extra-package-indent-bars = {
      url = "github:jdtsmith/indent-bars";
      flake = false;
    };
    extra-package-nix-ts-mode = {
      url = "github:aciceri/nix-ts-mode/improved";
      flake = false;
    };
    extra-package-combobulate = {
      url = "github:mickeynp/combobulate";
      flake = false;
    };
    extra-package-agenix-el = {
      url = "github:t4ccer/agenix.el";
      flake = false;
    };
    extra-package-chatgpt = {
      url = "github:joshcho/ChatGPT.el";
      flake = false;
    };
    extra-package-copilot = {
      # TODO remove rev when jsonrpc 1.0.23 hits emacs 29
      # https://github.com/copilot-emacs/copilot.el/issues/232
      url = "github:zerolfx/copilot.el/1542d76909636bf8804dd9c770f28818a897cfdc";
      flake = false;
    };
    extra-package-notmuch-notify = {
      url = "github:firmart/notmuch-notify";
      flake = false;
    };
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux"];
      imports = [
        ./packages
        ./hmModules
        ./formatter
        # ./diff-closures
	./checks
	./ci
      ];
    };

  # Not hitting it
  # nixConfig = {
  #   extra-substituters = ["https://nix-community.cachix.org"];
  #   extra-trusted-public-keys = ["nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="];
  # };
}
