{ mkDerivation, base, base-compat, directory, filepath, hspec
, hspec-discover, lib, mtl, template-haskell
}:
mkDerivation {
  pname = "th-compat";
  version = "0.1.5";
  sha256 = "81f55fafc7afad7763c09cb8b7b4165ca3765edcf70ffa42c7393043a1382a1e";
  revision = "2";
  editedCabalFile = "0bhx6ahf89v7pm3s05b98rm2gbhi4yg8knnn5wchdkfg7jx5k6hj";
  libraryHaskellDepends = [
    base directory filepath template-haskell
  ];
  testHaskellDepends = [
    base base-compat hspec mtl template-haskell
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-compat/th-compat";
  description = "Backward- (and forward-)compatible Quote and Code types";
  license = lib.licenses.bsd3;
}
