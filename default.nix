{ mkDerivation, base, optparse-applicative, shelly, stdenv, text, extra }:
mkDerivation {
  pname = "ghc-build-diagnostics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base optparse-applicative shelly text extra
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
