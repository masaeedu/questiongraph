let
 sources = import ./nix/sources.nix;
 pkgs = import sources.nixpkgs {};
 hie = (import sources.all-hies {}).versions.ghc844;
 hpkgs = pkgs.haskellPackages;
 drv = hpkgs.callCabal2nix "questiongraph" ./. {};
in

with pkgs;
rec {
 rebase-tree = drv;

 shell =
   let pkg' =
     haskell.lib.addBuildTools
       rebase-tree
       [
         hpkgs.cabal-install
         hpkgs.ghcid
         hpkgs.hoogle
         hpkgs.stylish-cabal
         hie
       ];
   in
   pkg'.env;
}
