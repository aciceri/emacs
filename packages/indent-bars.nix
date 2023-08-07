{
  trivialBuild,
  compat,
  src,
}: let
  rev = "de347fc7fd2bdb905f95b986460ec485cd047992";
in
  trivialBuild {
    inherit src;
    pname = "indent-bars";
    version = rev;
    propagatedUserEnvPkgs = [
      compat
    ];
    buildInputs = [
      compat
    ];
  }
