{inputs, config, ...}: {
  imports = [
    inputs.hercules-ci-effects.flakeModule
  ];
  herculesCI.ciSystems = [
    "x86_64-linux"
    # "aarch64-linux"
  ];
  hercules-ci.flake-update = {
    enable = true;
    updateBranch = "updated-flake-lock";
    createPullRequest = true;
    autoMergeMethod = null;
    when = {
      minute = 30;
      hour = 13;
      dayOfWeek = ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"];
    };
    pullRequestBody = ''
      Update `flake.lock`. See the commit message(s) for details.
      
      You may reset this branch by deleting it and re-running the update job. xs

          git push origin :${config.hercules-ci.flake-update.updateBranch}

      ```
      ${builtins.readFile config.packages.x86_64-linux.diff-closures}
      ```
    '';
  };
}
