# Ideally I'd like to set 
#   build-tool-depends: c2hs:c2hs
# in the h3-hs.cabal file, but unfortunately this appears 
# to cause an issue in the development shells below.
# If we enable `build-tool-depends`, we can use 
# `nix develop`, but then from within that development shell, 
# we get cabal build errors on account of the handling of c2hs.
# This appears to be documented in the references below, though please 
# not that not all of the references might refer to the root issue.
# As a work around, we comment out or disable 
# `build-tool-depends` and instead override 
# the cabal derivation attribute `nativeBuildInputs` to include `c2hs`.
# This appears to allow us both to enter the shell (which itself performs
# a build of `h3-hs`) and use cabal to rebuild and test the package.
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
                    let
                        h3-hs-preliminary = prevHaskell.callCabal2nix "h3-hs" ./. { h3 = final.h3_4; };
                    in
                    {
                      # h3-hs = prevHaskell.callCabal2nix "h3-hs" ./. { h3 = final.h3_4; };
                      # h3-hs = prevHaskell.h3-hs.override { h3 = final.h3_4; };
                      # h3-hs = h3-hs-preliminary.overrideAttrs (prevAttrs: {
                      #     nativeBuildInputs = prevAttrs.nativeBuildInputs ++ [ finalHaskell.c2hs ];
                      # });
                      h3-hs = h3-hs-preliminary;
                    }
              );
          };
      };
      # h3-hs-hackage-overlay = final: prev: {
      #     haskell = prev.haskell // {
      #         packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      #             finalHaskell: prevHaskell:
      #               {
      #                 h3-hs = prevHaskell.callCabal2nix "h3-hs" ./. { h3 = final.h3_4; };
      #                 # Use the following after upgrading nixpkgs
      #                 # h3-hs = prevHaskell.h3-hs.override { h3 = final.h3_4; };
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
   
          build-package-map = hs: [hs.cabal-install hs.test-framework-quickcheck2 hs.c2hs ]; # hs.c2hs
          # build-package-map-with-h3 = hs: [hs.cabal-install hs.test-framework-quickcheck2 hs.c2hs hs.h3-hs];
          haskell-build-packages = [
              pkgs.which
              pkgs.h3_4
              pkgs.pkg-config
              (pkgs.haskellPackages.ghcWithPackages build-package-map)
          ];
          # haskell-build-packages-with-h3 = [
          #     pkgs.which
          #     pkgs.h3_4
          #     pkgs.pkg-config
          #     (pkgs.haskellPackages.ghcWithPackages build-package-map-with-h3)
          # ];

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
        # devShell = devShells.default; 
        # pkgs.mkShell {
        #   packages = dev-haskell-packages;
        #   # inputsFrom = dev-haskell-packages;
        #   nativeBuildInputs = [ pkgs.haskellPackages.c2hs ]; #  dev-haskell-packages;
        #   # buildInputs = dev-haskell-packages;
        #   # propagatedBuildInputs = dev-haskell-packages;
        #   # nativeBuildInputs = dev-haskell-packages;
        #   # packages = [ 
        #   #     pkgs.haskellPackages.cabal-install
        #   #     pkgs.haskellPackages.c2hs
        #   # ];
        #   shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev > \\[\\e[0m\\]'";
        # };
        devShells = {
            default = pkgs.mkShell {
              packages = haskell-build-packages;
              # inputsFrom = dev-haskell-packages;
              nativeBuildInputs = [ pkgs.haskellPackages.c2hs ]; #  dev-haskell-packages;
              # buildInputs = dev-haskell-packages;
              # propagatedBuildInputs = dev-haskell-packages;
              # nativeBuildInputs = dev-haskell-packages;
              # packages = [ 
              #     pkgs.haskellPackages.cabal-install
              #     pkgs.haskellPackages.c2hs
              # ];
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev > \\[\\e[0m\\]'";
            };
            ex-h3-hs = pkgs.mkShell {
              packages = [ pkgs.h3_4 (pkgs.haskellPackages.ghcWithPackages (hs: [hs.cabal-install hs.test-framework-quickcheck2])) ];
              nativeBuildInputs = [ pkgs.haskellPackages.c2hs ];
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev (ex h3-hs) > \\[\\e[0m\\]'";
            };
            legacy = pkgs.haskellPackages.shellFor {
                packages = p: [p.h3-hs];
                withHoogle = true;
                buildInputs =((with pkgs; [ h3_4 ]) ++ (with pkgs.haskellPackages; [ cabal-install c2hs ]));
                shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-shellfor > \\[\\e[0m\\]'";
                # exactDeps = false;
            };
            ghc928shell = pkgs.mkShell {
              packages = [ pkgs.h3_4 (pkgs.haskell.packages.ghc928.ghcWithPackages build-package-map) ];
              # inputsFrom = [ (pkgs.haskell.packages.ghc928.ghcWithPackages (hs: [hs.c2hs])) ];
              # buildInputs = [ (pkgs.haskell.packages.ghc928.ghcWithPackages (hs: [hs.c2hs])) ];
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev (ghc-9.2.8) > \\[\\e[0m\\]'";
            };
            ghc982shell = pkgs.mkShell {
              # buildInputs = [ pkgs.h3_4 (pkgs.haskell.packages.ghc982.ghcWithPackages (hs: [hs.h3-hs hs.cabal-install hs.c2hs hs.test-framework-quickcheck2]))];
              # inputsFrom = [ (pkgs.haskell.packages.ghc982.ghcWithPackages (hs: [hs.c2hs])) ];
              # nativeBuildInputs = [ (pkgs.haskell.packages.ghc982.ghcWithPackages (hs: [hs.c2hs])) ];
              # buildInputs = [ (pkgs.haskell.packages.ghc982.ghcWithPackages (hs: [hs.c2hs])) ];
              packages = [ pkgs.which pkgs.h3_4 (pkgs.haskell.packages.ghc982.ghcWithPackages build-package-map) ];
              shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs-dev (ghc-9.8.2) > \\[\\e[0m\\]'";
            };
            # package-test = pkgs.mkShell {
            #   packages = haskell-build-packages-with-h3;
            #   # nativeBuildInputs = [ pkgs.haskellPackages.c2hs ];
            #   shellHook = "export PS1='\\[\\e[1;34m\\]h3-hs (package test) > \\[\\e[0m\\]'";
            # };
        };
        packages = {
          h3-hs = pkgs.haskellPackages.h3-hs;
          default = pkgs.haskellPackages.h3-hs;
        };
      }
    ) // {
      overlays.default = h3-hs-source-overlay;
    };
}
