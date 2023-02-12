{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-src = {
      url = "git://git.savannah.gnu.org/emacs.git?branch=emacs29";
      flake = false;
    };
    nixpkgs.follows = "emacs-overlay/nixpkgs";
  };

  outputs = inputs @ {
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        # flake-parts.flakeModules.easyOverlay
        ./packages
        ./hmModules
        ./formatter
      ];
    };

  nixConfig = {
    extra-substituters = [
      "https://aciceri-fleet.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "aciceri-fleet.cachix.org-1:e1AodrwmzRWy0eQi3lUY71M41fp9Sq+UpuKKv705xsI="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
