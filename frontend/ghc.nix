{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});

in
reflex-platform.ghc.override {
  overrides = self: super: { 
     reflex-dom-contrib  = (self.callPackage (reflex-platform.cabal2nixResult ../deps/reflex-dom-contrib) {});
     common      = (self.callPackage ../common {});
     servant             = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});
  };
}
