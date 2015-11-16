{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, cryptonite, hspec, HUnit, memory, raw-strings-qq
, stdenv, text, time, utf8-string
}:
mkDerivation {
  pname = "filepicker-policy";
  version = "0.2.5";
  src = ./.;
  buildDepends = [
    aeson base base64-bytestring bytestring containers cryptonite
    memory text time utf8-string
  ];
  testDepends = [
    aeson base bytestring hspec HUnit raw-strings-qq time
  ];
  description = "API for creating filepicker.com policies";
  license = stdenv.lib.licenses.mit;
}
