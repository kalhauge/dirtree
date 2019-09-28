{ mkDerivation, base, bytestring, containers, deepseq, directory
, filepath, hpack, hspec, hspec-discover
, hspec-expectations-pretty-diff, lens, stdenv, unix, zip-archive
}:
mkDerivation {
  pname = "dirtree";
  version = "0.1.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers deepseq directory filepath lens unix
    zip-archive
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring containers deepseq directory filepath hspec
    hspec-discover hspec-expectations-pretty-diff lens unix zip-archive
  ];
  testToolDepends = [ hspec-discover ];
  preConfigure = "hpack";
  homepage = "https://github.com/kalhauge/dirtree#readme";
  description = "A small library for working with directories";
  license = stdenv.lib.licenses.mit;
}
