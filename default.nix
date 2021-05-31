{ mkDerivation, base, optparse-applicative, shelly, stdenv, text }:
mkDerivation {
  pname = "ghc-build-diagnostics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base optparse-applicative shelly text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
