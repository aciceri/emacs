{
  trivialBuild,
  src,
  ...
}:
trivialBuild {
  inherit src;
  version = "git";
  pname = "agenix-el";
}
