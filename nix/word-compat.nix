{ mkDerivation, base, fetchgit, ghc-prim, lib }:
mkDerivation {
  pname = "word-compat";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/j6carey/word-compat";
    sha256 = "03g8lz806h3m15dg6894snyizav76skkfw9bxxa1b5wlyq6aqag1";
    rev = "1d3147cb6789b4ad67285dfbe8b2384c8842b6f1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  description = "Compatibility shim for the Int/Word internal change in GHC 9.2";
  license = lib.licenses.bsd3;
}
