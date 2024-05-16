{ mkDerivation, base, containers, lib, microlens, tagged
, template-haskell, th-abstraction, transformers
}:
mkDerivation {
  pname = "microlens-th";
  version = "0.4.3.15";
  sha256 = "0fceb3271297ad29a2bfa3e62953debbb44dba513c439a363acf625f954e45c9";
  libraryHaskellDepends = [
    base containers microlens template-haskell th-abstraction
    transformers
  ];
  testHaskellDepends = [ base microlens tagged ];
  homepage = "http://github.com/stevenfontanella/microlens";
  description = "Automatic generation of record lenses for microlens";
  license = lib.licenses.bsd3;
}
