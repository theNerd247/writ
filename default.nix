let
  pkgs = import <nixpkgs> {};

  drv = pgks.haskellPackages.callCabal2nix "writ" ./. {};

  shell = drv.env.overrideAttrs (old: with pkgs.haskellPackages; {
    buildInputs = old.buildInputs ++
    [ cabal-install
      ghcid
      hpack
    ];
  });
in
  { inherit shell;
    writ = drv;
  }
