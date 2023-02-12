{
  perSystem = {config, ...}: {
    overlayAttrs = {
      inherit (config.packages) ccrEmacs;
    };
  };
}
