{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.follows = "emacs-overlay/nixpkgs";
  };
  
  outputs = inputs @ {
    flake-parts,
    ...
  } :
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        ./packages
        ./hmModules
        ./formatter
        ./diff-closures
        ./hydra
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

  
 
