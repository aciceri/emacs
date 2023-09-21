{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    indent-bars = {
      # TODO remove when it lands on (M)ELPA
      url = "github:jdtsmith/indent-bars";
      flake = false;
    };
    nix-ts-mode = {
      url = "github:aciceri/nix-ts-mode/improved";
      flake = false;
    };
    combobulate = {
      url = "github:mickeynp/combobulate";
      # url = "github:aciceri/combobulate/nix";
      flake = false;
    };
    agenix-el.url = "github:t4ccer/agenix.el";
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
