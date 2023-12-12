{
  description = "My opinionated Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
      url = "github:zerolfx/copilot.el";
      flake = false;
    };
    extra-package-notmuch-notify = {
      url = "github:firmart/notmuch-notify";
      flake = false;
    };
    # TODO: use from MELPA when v0.7.5 reaches it
    # https://github.com/tumashu/vertico-posframe/issues/36
    extra-package-vertico-posframe = {
      url = "github:tumashu/vertico-posframe";
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
