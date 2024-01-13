{ melpaBuild
, fetchFromGitHub
, writeText
, pkgs

# Elisp dependencies
, ctable
, deferred
, epc
, s

# Native dependencies
, nodejs
, python3
, wmctrl
, xdotool
}:

let
  # TODO: Package nodejs environment

  pythonEnv = ((python3.withPackages(ps: [
    ps.pyqtwebengine
    ps.pyqt5
    ps.qrcode
    ps.qtconsole
    ps.retry
    ps.pymupdf
    # Wrap native dependencies in python env $PATH
    pkgs.aria2
  ])).override { ignoreCollisions = true; });

  node = "${nodejs}/bin/node";

  pname = "eaf";
  version = "20210309.0";

in melpaBuild {

  inherit pname version;

  src = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "emacs-application-framework";
    rev = "d55fef029d9a8fa529d2290f2da178dc8ff3d6f7";
    sha256 = "sha256-0UGeo4I4JB95A8W870x4IS6Syh6roMomjTTNQNGbS3E";
  };

  dontConfigure = true;
  dontBuild = true;

  postPatch = ''
   
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/emacs-$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/emacs-$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (eaf
    :repo "manateelazycat/emacs-application-framework"
    :fetcher github
    :files ("*")
  '';

  packageRequires = [
    ctable
    deferred
    epc
    s
  ];

}
