{ mkDerivation, base, fetchgit, ghc-prim, lib }:
mkDerivation {
  pname = "word-compat";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/fumieval/word-compat";
    sha256 = "13370fp55ms3bmyip83vav33jasggz04sksy61ypd0fm5yirpj7x";
    rev = "c02432cea08bc8037fe8f1bbfc7a026bbddff75e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  description = "Compatibility shim for the Int/Word internal change in GHC 9.2";
  license = lib.licenses.bsd3;
}
