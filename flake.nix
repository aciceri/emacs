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
    extra-package-nix-ts-mode = {
      url = "github:antifuchs/nix-ts-mode/improve-indented-string-indentation";
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
      url = "github:zerolfx/copilot.el";
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
}
