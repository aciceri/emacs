{self, ...}: {
  flake.hydraJobs = {
    packages = self.packages; 
  };
  perSystem = {pkgs, ...}: {
    packages.hydraHook = pkgs.writeScript "hook" ''
      echo ciao
    ''; 
  };
}
