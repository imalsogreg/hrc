{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     heist    = dontCheck (self.callPackage (cabal2nixResult deps/heist){});
     xmlhtml  = dontCheck (self.callPackage (cabal2nixResult deps/xmlhtml){});
     reflex-dom-contrib  = dontCheck (self.callPackage (cabal2nixResult deps/reflex-dom-contrib){});
  };
}
