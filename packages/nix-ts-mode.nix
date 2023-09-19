{
  trivialBuild,
  src,
}:
trivialBuild {
  inherit src;
  pname = "nix-ts-mode";
  version = "git";
}
