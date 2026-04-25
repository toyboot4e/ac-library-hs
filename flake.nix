{
  description = "ac-library-hs - Haskell port of ac-library for competitive programming n AtCoder";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskellNix,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
          inherit (haskellNix) config;
        };

        competitive-verifier =
          with pkgs.python3Packages;
          pkgs.python3Packages.buildPythonApplication {
            name = "competitive-verifier";
            version = "4.1.1";
            pyproject = true;
            src = pkgs.fetchFromGitHub {
              owner = "competitive-verifier";
              repo = "competitive-verifier";
              rev = "v4.1.1";
              sha256 = "sha256-l6yUAwrth1C38xopn2gWKr73U2nzEaVsSFpNLkGN2uM=";
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

        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs = {
            cabal-gild.enable = true;
            ormolu.enable = true;
          };
        };

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc984";
          # Be sure to install our versions of test/benchmark dependencies
          cabalProjectLocal = ''
            tests: True
            benchmarks: True
          '';
          shell = {
            tools = {
              cabal = "latest";
              haskell-language-server = "latest";
              hlint = "latest";
              ghcid = "latest";
              hoogle = "latest";
              implicit-hie = "latest";
            };
            buildInputs = with pkgs; [
              # HACK: doctest built separately to avoid ghc-library dependency conflict
              (let
                doctestTool = haskell-nix.tool "ghc984" "doctest" "latest";
              in
              writeShellScriptBin "doctest" ''
                exec ${doctestTool}/bin/doctest \
                  "-package-db=$(ghc --print-global-package-db)" \
                  "$@"
              '')
              # Verification
              online-judge-tools
              competitive-verifier

              # Formatting
              treefmtEval.config.build.wrapper
              # CI tools
              act
              action-validator
              actionlint
            ];
          };
        };

        flake = project.flake { };
      in
      {
        packages = (flake.packages or { }) // {
          default = flake.packages."ac-library-hs:lib:ac-library-hs" or null;
          format = treefmtEval.config.build.wrapper;
        };
        checks = (flake.checks or { }) // {
          formatting = treefmtEval.config.build.check self;
          cabal-check = pkgs.runCommand "cabal-check" {
            src = self;
            nativeBuildInputs = [ project.pkg-set.config.ghc.package pkgs.cabal-install ];
          } ''
            cd $src
            cabal check
            touch $out
          '';
        };
        apps = (flake.apps or { }) // {
          verify = {
            type = "app";
            meta = { };
            program = toString (pkgs.writeShellScript "verify" ''
              cd verify
              files="$(ls app/*.hs | ${pkgs.fzf}/bin/fzf -m --history .fzf-history)"
              touch $files
              competitive-verifier oj-resolve --config .verify-helper/config.toml --include $files > /tmp/cv-resolve.json
              competitive-verifier verify --verify-json /tmp/cv-resolve.json --tle 30
            '');
          };
          verify-all = {
            type = "app";
            meta = { };
            program = toString (pkgs.writeShellScript "verify-all" ''
              cd verify
              touch app/*
              competitive-verifier oj-resolve --config .verify-helper/config.toml > /tmp/cv-resolve.json
              competitive-verifier verify --verify-json /tmp/cv-resolve.json --tle 30
            '');
          };
        };
        devShells.default = project.shell;
        formatter = treefmtEval.config.build.wrapper;
      }
    );
}
