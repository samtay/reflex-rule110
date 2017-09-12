{ mkDerivation, base, comonad, containers, random, reflex
, reflex-dom, stdenv, text, time
}:
mkDerivation {
  pname = "rule110";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base comonad containers random reflex reflex-dom text time
  ];
  homepage = "https://github.com/SamTay/rule110#readme";
  license = stdenv.lib.licenses.bsd3;
}
