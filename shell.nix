{ nixpkgs ? import ./nix/nixpkgs.nix
}:
(import ./default.nix) { inherit nixpkgs; }
