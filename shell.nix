let
  pkgs = import <nixpkgs> { }; 
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv: pkgs.haskell.lib.addBuildTools drv ((with pkgs; [ h3_4 ]) ++ (with pkgs.haskellPackages ; [ cabal-install c2hs ]));
  returnShellEnv = true;
}
