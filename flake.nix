{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages.ghc928;
        devTools = [
          hPkgs.ghc
          hPkgs.cabal-install
          hPkgs.haskell-language-server
          hPkgs.hlint
          hPkgs.ghcid
          # hPkgs.fourmolu # Can't install it from Nix rn, don't want to bother with it too much
          # installed it using stack
          hPkgs.implicit-hie
          hPkgs.retrie
          hPkgs.cabal-install
          hPkgs.cabal-fmt
          pkgs.zlib
          pkgs.pkg-config
          stack-wrapped
        ];
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = devTools;
          LD_LIBRARY_PATH = lib.makeLibraryPath devTools;
          shellHook = ''
            export PATH="$HOME/.local/bin:$PATH"
          '';
        };
      }
    );
}
