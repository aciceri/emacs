{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-src = {
      url = "git://git.savannah.gnu.org/emacs.git?ref=emacs-29";
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
        ./packages
        ./hmModules
        ./formatter
      ];
    };
  
  nixConfig = {
    extra-substituters = [
      "https://aciceri-emacs.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "aciceri-emacs.cachix.org-1:kxDGDFWV6LUj41tb8xmPRBI56UJSZOVveN49LZDUKdA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
