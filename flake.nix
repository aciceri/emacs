{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
      # url = "github:aciceri/combobulate/nix";
      flake = false;
    };
    extra-package-agenix-el = {
      url = "github:t4ccer/agenix.el";
      flake = false;
    };
    extra-package-dracula-theme = {
      url = "github:aciceri/dracula-emacs";
      flake = false;
    };
    extra-package-chatgpt = {
      url = "github:joshcho/ChatGPT.el";
      flake = false;
    };
    extra-package-copilot = {
      url = "github:zerolfx/copilot.el";
      flake = false;
    };
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        ./packages
        ./hmModules
        ./formatter
        ./diff-closures
      ];
    };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
