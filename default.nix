{ mkDerivation, base, optparse-applicative, shelly, stdenv,
  text, extra, ghc, ghc-paths, wget, gnutar }:
mkDerivation {
  pname = "ghc-build-diagnostics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ wget gnutar ];
  executableHaskellDepends = [
    base optparse-applicative shelly text extra ghc ghc-paths
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
