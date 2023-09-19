{
  trivialBuild,
  src,
  ...
}:
trivialBuild {
  inherit src;
  version = "git";
  pname = "combombulate";
}
