let
  pkgs = import ./pkgs.nix {
    overlays = [
      (self: super:
        {
          haskellPackages = super.haskellPackages.override
            (hOld: {
              overrides = builtins.foldl' super.lib.composeExtensions (hOld.overrides or (_: _: {}))
                [
                  (super.haskell.lib.packageSourceOverrides
                    { fresnel = builtins.fetchGit 
                        {
                          url = "https://github.com/frasertweedale/hs-fresnel.git";
                          rev = "a141d92f9f69bd155060af76d040ac86111ef826";
                        };
                    }
                  )

                  (hself: hsuper: { writ = hself.callCabal2nix "writ" ./. {}; })
                ];
            });
        }
      )
    ];
  };

  writ = pkgs.haskellPackages.writ;

  shell = writ.env.overrideAttrs (old: with pkgs.haskellPackages; {
    buildInputs = old.buildInputs ++
    [ cabal-install
      ghcid
      hpack
    ];
  });

in
  { inherit shell;
    inherit writ;
  }
