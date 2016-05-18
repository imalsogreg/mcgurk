{ mkDerivation, aeson, base, bytestring, common, configurator
, containers, heist, lens, map-syntax, mtl, servant, servant-snap
, snap, snap-core, snap-loader-dynamic, snap-loader-static
, snap-server, stdenv, stm, text, time, transformers, websockets
, websockets-snap
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring common configurator containers heist lens
    map-syntax mtl servant servant-snap snap snap-core
    snap-loader-dynamic snap-loader-static snap-server stm text time
    transformers websockets websockets-snap
  ];
  license = stdenv.lib.licenses.bsd3;
}
