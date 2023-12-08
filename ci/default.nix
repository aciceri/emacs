{inputs, ...}: {
  imports = [
    inputs.hercules-ci-effects.flakeModule
  ];
  herculesCI.ciSystems = [
    "x86_64-linux"
    # "aarch64-linux"
  ];
}
