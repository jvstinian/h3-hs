# We have decided to set 
#   build-tool-depends: c2hs:c2hs
# in the h3-hs.cabal file, though unfortunately this appears 
# to cause an issue in the development shells below.
# We have decided to use this approach as we regard the cabal file as the
# source of truth for the dependencies of the package, and
# will work around the issues in the nix development shells.
# The package successfully builds using the overlay, so the
# issue is specific to the development shells.
# The issue is that, by specifying `c2hs` in `build-tool-depends`,
# we can use `nix develop`, but from within that development shell, 
# we get cabal build errors on account of the handling of c2hs.
# Using `cabal update` seems to address this, but as a result `cabal build`
# will build a local `c2hs` executable, even though a
# suitable `c2hs` is already available on the path.
# This appears to be documented in the references below, though please 
# note that not all of the references might refer to the root issue.
# References:
# * https://github.com/input-output-hk/haskell.nix/issues/760
# * https://github.com/input-output-hk/haskell.nix/issues/231
# * https://github.com/input-output-hk/haskell.nix/issues/839
# * https://github.com/input-output-hk/haskell.nix/issues/1367#issuecomment-1207454622
# * https://github.com/haskell/cabal/issues/8434
{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-24.05";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: 
    let
      h3-hs-source-overlay = final: prev: {
          haskell = prev.haskell // {
              packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
                  finalHaskell: prevHaskell:
                    {
                      h3-hs = prevHaskell.callCabal2nix "h3-hs" ./. { h3 = final.h3_4; };
                    }
              );
          };
      };
      # # Use the following after upgrading nixpkgs
      # h3-hs-hackage-overlay = final: prev: {
      #     haskell = prev.haskell // {
      #         packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      #             finalHaskell: prevHaskell:
      #               {
      #                 h3-hs = prevHaskell.h3-hs.override { h3 = final.h3_4; };
      #               }
      #         );
      #     };
      # };
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ h3-hs-source-overlay ];
          };
          
          # release-pkgs = import nixpkgs {
          #   inherit system;
          #   overlays = [ h3-hs-hackage-overlay ];
          # };
  
          base-packages = [
              pkgs.which
              pkgs.h3_4
              pkgs.pkg-config
          ];

          build-package-map = hs: [hs.cabal-install hs.test-framework-quickcheck2 hs.c2hs ];
          haskell-build-packages-default = base-packages ++ [
              (pkgs.haskellPackages.ghcWithPackages build-package-map)
          ];

          haskell-build-packages-for-version = ghcversion: haskellpackages: (base-packages ++ [
              (pkgs.haskell.packages.${ghcversion}.ghcWithPackages haskellpackages)
          ]);
          
          test-package-map = hs: [hs.cabal-install hs.test-framework-quickcheck2 hs.c2hs hs.h3-hs ];
          haskell-build-packages-test = base-packages ++ [
              (pkgs.haskellPackages.ghcWithPackages test-package-map)
          ];

          # release-haskell-package-map = hs: [hs.h3-hs hs.cabal-install hs.test-framework-quickcheck2];
          # release-haskell-packages = [
          #     pkgs.which
          #     pkgs.h3_4
          #     pkgs.pkg-config
          #     # (pkgs.haskellPackages.ghcWithPackages (hs: [hs.h3-hs hs.cabal-install hs.c2hs hs.test-framework-quickcheck2]))
          #     # NOTE: Removed c2hs from the following
          #     (pkgs.haskellPackages.ghcWithPackages release-haskell-package-map) # (hs: [hs.h3-hs hs.cabal-install hs.test-framework-quickcheck2])
          # ];
      in rec {
        devShells = {
            default = pkgs.mkShell {
              packages = haskell-build-packages-default;
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev > \\[\\e[0m\\]'";
            };
            ghc928shell = pkgs.mkShell {
              # packages = [ pkgs.h3_4 (pkgs.haskell.packages.ghc928.ghcWithPackages build-package-map) ];
              packages = haskell-build-packages-for-version "ghc928" build-package-map;
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev (ghc-9.2.8) > \\[\\e[0m\\]'";
            };
            ghc982shell = pkgs.mkShell {
              # packages = [ pkgs.which pkgs.h3_4 (pkgs.haskell.packages.ghc982.ghcWithPackages build-package-map) ];
              packages = haskell-build-packages-for-version "ghc982" build-package-map;
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev (ghc-9.8.2) > \\[\\e[0m\\]'";
            };
            packageTest = pkgs.mkShell {
              # This is for testing the package build.  Use `ghci` rather than `cabal repl` for manual testing.
              packages = haskell-build-packages-test;
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs (package test) > \\[\\e[0m\\]'";
            };
        };
        packages = {
          default = pkgs.haskellPackages.h3-hs;
          h3-hs = pkgs.haskellPackages.h3-hs;
          # hackage-h3-hs = release-pkgs.haskellPackages.h3-hs;
        };
      }
    ) // {
      overlays.default = h3-hs-source-overlay;
      overlays.h3-hs-source = h3-hs-source-overlay;
    };
}
