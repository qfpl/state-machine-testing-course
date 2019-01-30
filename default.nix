{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
        ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
        ghcWithPackages = hself.ghc.withPackages;
        # Any overrides or snowflake packages can be placed here
      });
    });
  };

  pkgs = import nixpkgs {
    # Yay, overlays!
    overlays = [overlay];
  };

  # The course, yey.
  drv = pkgs.haskellPackages.callPackage ./fp-eedee.nix {};

  drvWithTools = pkgs.haskell.lib.addBuildTools drv
    [ # Include our beloved and standard text editor
      pkgs.ed
      # A development tool for great justice
      (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.ghcid)
      (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.cabal-install)
    ];
in
  # Remove the need for 'shell.nix'.
  pkgs.haskell.lib.shellAware drvWithTools
