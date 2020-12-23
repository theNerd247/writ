let
  pkgs = import ./pkgs.nix {
    overlays = [
      (self: super:
        {
          haskellPackages = super.haskellPackages.override
            (hOld: {
              overrides = builtins.foldl' super.lib.composeExtensions (hOld.overrides or (_: _: {}))
                [
                  (hself: hsuper: { unparse-attoparsec = with self.haskell.lib;
                    (x: doJailbreak (dontCheck x))
                      (hself.callCabal2nix "unparse-attoparsec" 
                        (
                          fetchGit
                          { url = "https://github.com/Lysxia/unparse-attoparsec.git";
                            ref = "master";
                          }
                        )
                        {}
                      );
                  })

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
