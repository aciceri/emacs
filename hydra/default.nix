{self, ...}: {
  flake.hydraJobs = {
    packages = self.packages; 
  };
}
