{
  niceHaskell,
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags { doCheck = true; };
  packageRoot = ./.;
  cabalName = "drug2";
}
