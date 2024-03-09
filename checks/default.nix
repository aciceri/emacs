{
  perSystem = {config, pkgs, ...}: {
    checks = {
      inherit (config.packages) ccrEmacs;
      test = pkgs.hello.overrideDerivation (_: {
	name = "test";
      });
    };
  };
}
