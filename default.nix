let
  pkgs = import <nixpkgs> {
    # overlays = [ 
    #   (final: prev: {
    #     opencv3 = prev.opencv3.override { enableFfmpeg = true; enableGtk2 = true; enableGtk3 = true; };
    #   })
    # ];
  }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  # modifier = drv: pkgs.haskell.lib.addBuildTools drv ((with pkgs; [ h3_4 ]) ++ (with pkgs.haskellPackages ; [ cabal-install c2hs QuickCheck test-framework test-framework-quickcheck ]));
  # modifier = drv: pkgs.haskell.lib.addBuildTools drv ((with pkgs; [ h3_4 ]) ++ (with pkgs.haskellPackages ; [ cabal-install c2hs test-framework test-framework-quickcheck2 ]));
  modifier = drv: pkgs.haskell.lib.addBuildTools drv ((with pkgs; [ h3_4 ]) ++ (with pkgs.haskellPackages ; [ cabal-install c2hs ]));
  # cabal2nixOptions = "--no-check";
  # overrides = self: super: {
  #   # cpython = pkgs.haskell.lib.dontCheck super.cpython;
  #   opencv3 = super.opencv3.override { enableFfmpeg = true; };
  # };
  returnShellEnv = true;
  # overrides = self: super: {
  #   inline-c = pkgs.haskell.lib.dontCheck super.inline-c;
  # };
}
