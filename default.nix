{ mkDerivation, base, optparse-applicative, shelly, stdenv,
  text, filepath, extra, ghc, ghc-paths, wget, gnutar, Cabal,
  directory
}:
mkDerivation {
  pname = "ghc-build-diagnostics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ wget gnutar ];
  executableHaskellDepends = [
    base optparse-applicative shelly text extra ghc ghc-paths filepath
    Cabal directory
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
