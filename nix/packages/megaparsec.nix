{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, lib, mtl, parser-combinators, scientific
, text, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "9.2.1";
  sha256 = "6b278397baa4ae66d613330465c919e909ced077ec308b18827b43cf6715e9ff";
  revision = "1";
  editedCabalFile = "04ykwffsscwybjdzq225b3dir1r38xabz2q8aypd7x148dinyxfk";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq text weigh
  ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = lib.licenses.bsd2;
}
