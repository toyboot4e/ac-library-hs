{
  description = "A basic flake with a shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-for-ghc.url = "github:NixOS/nixpkgs/ebe4301cbd8f81c4f8d3244b3632338bbeb6d49c";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      nixpkgs-for-ghc,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        ghcpkgs = import nixpkgs-for-ghc {
          inherit system;
        };
        oj-verify =
          with pkgs.python3Packages;
          pkgs.python3Packages.buildPythonApplication {
            name = "verification-helper";
            version = "5.6.0";
            pyproject = true;
            src = pkgs.fetchFromGitHub {
              owner = "online-judge-tools";
              repo = "verification-helper";
              rev = "adbff121b1f96de5f34e9f1483eb47d661c54075";
              fetchSubmodules = false;
              sha256 = "sha256-f7Ge8kLRQv9uxdNGtgNsypGVY0XAnKPCg8HYQ5nT6mI=";
            };
            build-system = [ setuptools ];
            dependencies = [
              colorlog
              importlab
              online-judge-tools
              pyyaml
              setuptools
              toml
            ];
            propagatedBuildInputs = [ setuptools ];
          };
        competitive-verifier =
          with pkgs.python3Packages;
          pkgs.python3Packages.buildPythonApplication {
            name = "competitive-verifier";
            version = "3.3.1";
            pyproject = true;
            src = pkgs.fetchFromGitHub {
              owner = "competitive-verifier";
              repo = "competitive-verifier";
              rev = "v3.3.1";
              sha256 = "sha256-eYm70R+XS2qVY6Rk0irdPSnjqd5PSV/e6OZgip54Su4=";
            };
            build-system = [ poetry-core ];
            dependencies = [
              poetry-core
            ];
            propagatedBuildInputs = [
              colorlog
              colorama
              pydantic
              pyyaml
              importlab
              charset-normalizer
              tomli
              requests
              appdirs
              beautifulsoup4
            ];
          };
      in
      {
        formatter = pkgs.nixfmt-tree;

        devShells.default =
          with pkgs;
          mkShell {
            buildInputs = [
              pkg-config
              stack
              cabal-install
              llvmPackages.bintools
            ];

            packages = [
              online-judge-tools
              # competitive-verifier
              oj-verify

              python312Packages.selenium
              python312Packages.pyaml
              python312Packages.importlab
              # python312Packagesz.sxsdiff # TODO: oj side-by-side diff
              nodejs

              # GHC 9.8.4
              (ghcpkgs.haskell.compiler.ghc984.override { useLLVM = true; })
              (ghcpkgs.haskell-language-server.override { supportedGhcVersions = [ "984" ]; })
              ghcpkgs.haskell.packages.ghc984.cabal-fmt
              ghcpkgs.haskell.packages.ghc984.cabal-plan
              ghcpkgs.haskell.packages.ghc984.doctest
              ghcpkgs.haskell.packages.ghc984.implicit-hie

              hlint
              ghcpkgs.haskellPackages.hoogle
              ghcpkgs.haskellPackages.ghcid
              ghcpkgs.haskellPackages.ghcide
              ghcpkgs.haskellPackages.ghci-dap
              ghcpkgs.haskellPackages.haskell-dap

              # CI
              act
              action-validator
              actionlint
            ];
          };
      }
    );
}
