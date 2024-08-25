{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.follows = "emacs-overlay/nixpkgs";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    extra-package-indent-bars = {
      url = "github:jdtsmith/indent-bars";
      flake = false;
    };
    extra-package-notmuch-notify = {
      url = "github:firmart/notmuch-notify";
      flake = false;
    };
    extra-package-copilot = {
      url = "github:zerolfx/copilot.el";
      flake = false;
    };
    extra-package-haskell-ts-mode = {
      url = "git+https://codeberg.org/pranshu/haskell-ts-mode.git";
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
}
