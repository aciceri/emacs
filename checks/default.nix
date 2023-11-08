{
  perSystem = {config, ...}: {
    checks = {
      inherit (config.packages) ccrEmacs;
    };
  };
}
