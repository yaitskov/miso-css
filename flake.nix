{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/bc16855ba53f3cb6851903a393e7073d1b5911e7";
    flake-utils.url = "github:numtide/flake-utils";
    adf = {
        url = "github:yaitskov/add-dependent-file";
        flake = false;
      };
    uphack = {
      url = "github:yaitskov/upload-doc-to-hackage";
      flake = false;
    };
    miso = {
      url = # path:/home/dan/study/haskell/miso/miso;
        "github:dmjio/miso";
      flake = false;
    };
    css-parser = {
      url = # path:/home/dan/study/haskell/miso/miso;
        "github:yaitskov/css-parser";
      flake = false;
    };
    reorder-expression = {
      url = "github:yaitskov/reorder-expression";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, uphack,  ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcName = "ghc9122";

        sourceFilter = root: with nixpkgs.lib.fileset; toSource {
          inherit root;
          fileset = fileFilter
            (file: file.name == "LICENSE" ||
                   # skip cabal.project.local
                   builtins.any file.hasExt [ "cabal" "hs" "md" "css" ])
            root;
        };
        packageName = "miso-css";
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs.haskell.lib) dontCheck; #  enableCabalFlag;
        haskellPackages = pkgs.haskell.packages.${ghcName}.extend(final: prev: {
          add-dependent-file = final.callCabal2nix "add-dependent-file" inputs.adf { };
          reorder-expression = final.callCabal2nix "reorder-expression" inputs.reorder-expression { };
          css-parser = final.callCabal2nix "css-parser" inputs.css-parser { };
          miso =
            dontCheck
              # (enableCabalFlag "template-haskell"
                (final.callCabal2nix "miso" "${inputs.miso}" { });
        });
      in {
        packages.default =
            haskellPackages.callCabal2nix packageName (sourceFilter ./.) {};

        devShells = {
          default = pkgs.mkShell {
            buildInputs = [ haskellPackages.haskell-language-server ] ++ (with pkgs; [
              ghcid
              cabal-install
              pandoc
              (import uphack { inherit pkgs; })
            ]);
            inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
            shellHook = ''
              export PS1='N$ '
              echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
            '';
          };
        };
      });
}
