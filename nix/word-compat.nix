{ mkDerivation, base, fetchgit, ghc-prim, lib }:
mkDerivation {
  pname = "word-compat";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/fumieval/word-compat";
    sha256 = "15ffpvza5jppcnracqa8shylr8ds6lvb4janry9yyyyw2a3h2cyb";
    rev = "3eb157b229af035c4a35a0524d56acdec50ba6c4";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  description = "Compatibility shim for the Int/Word internal change in GHC 9.2";
  license = lib.licenses.bsd3;
}
