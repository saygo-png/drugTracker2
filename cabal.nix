{ mkDerivation, aeson, ansi-terminal, base, bytestring, cassava
, classy-prelude, containers, lib, optparse-applicative, path
, path-io, pretty-relative-time, template-haskell, text, time
, typed-process, vector, vector-algorithms
}:
mkDerivation {
  pname = "drug2";
  version = "0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal base bytestring cassava classy-prelude
    containers optparse-applicative path path-io pretty-relative-time
    template-haskell text time typed-process vector vector-algorithms
  ];
  license = "unknown";
  mainProgram = "drug";
}
