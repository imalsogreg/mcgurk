{ reflex-platform, ... }:
let

  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;
  nixpkgs = (import <nixpkgs> {});
in
reflex-platform.ghc.override {
  overrides = self: super: { 
     common              = self.callPackage ../common {};
     servant-snap        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap) {});

     clock               = dontCheck (self.callPackage (cabal2nixResult ../deps/clock) {});
     servant             = dontCheck (self.callPackage (cabal2nixResult ../deps/servant/servant) {});
     hspec-snap          = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/hspec-snap) {});
     io-streams          = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/io-streams) {});
     io-streams-haproxy  = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/io-streams-haproxy) {});

     snap                = dontCheck (self.callPackage (cabal2nixResult ../deps/snap) {});
     heist               = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/heist) {});
     xmlhtml             = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/xmlhtml) {});

     snap-core           = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/snap-core) {});
     snap-server         = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/snap-server) {});


     snap-loader-static  = dontCheck (self.callPackage (cabal2nixResult ../deps/snap-loader-static) {});
     snap-loader-dynamic = dontCheck (self.callPackage (cabal2nixResult ../deps/snap-loader-dynamic) {});


     servant-foreign     = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-foreign) {});
     servant-js          = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-js) {});
     servant-docs        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     servant-client      = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-client) {});
     servant-blaze       = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-blaze) {});
     servant-lucid       = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-lucid) {});
     servant-server      = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-server) {});
     websockets-snap     = dontCheck (self.callPackage (cabal2nixResult ../deps/websockets-snap) {});
  };
}
