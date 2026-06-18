{
  description = "ac-library-hs - Haskell port of ac-library for competitive programming n AtCoder";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskellNix,
      treefmt-nix,
    }:
    let
      inherit (nixpkgs) lib;
      systems = lib.systems.flakeExposed;
      pkgsFor = lib.genAttrs systems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
          inherit (haskellNix) config;
        }
      );
      forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});

      treefmtEval = forEachSystem (
        pkgs:
        treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs = {
            cabal-gild.enable = true;
            ormolu.enable = true;
          };
        }
      );

      projectFor = forEachSystem (
        pkgs:
        pkgs.haskell-nix.cabalProject' {
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
              online-judge-verify-helper

              # Formatting
              treefmtEval.${pkgs.stdenv.hostPlatform.system}.config.build.wrapper
              haskellPackages.cabal-gild

              # CI tools
              pinact
              zizmor
            ];
          };
        }
      );

      flakeFor = forEachSystem (pkgs: projectFor.${pkgs.stdenv.hostPlatform.system}.flake { });
    in
    {
      packages = forEachSystem (
        pkgs:
        let
          flake = flakeFor.${pkgs.stdenv.hostPlatform.system};
        in
        (flake.packages or { })
        // {
          default = flake.packages."ac-library-hs:lib:ac-library-hs" or null;
          format = treefmtEval.${pkgs.stdenv.hostPlatform.system}.config.build.wrapper;
        }
      );

      checks = forEachSystem (
        pkgs:
        let
          flake = flakeFor.${pkgs.stdenv.hostPlatform.system};
          project = projectFor.${pkgs.stdenv.hostPlatform.system};
        in
        (flake.checks or { })
        // {
          formatting = treefmtEval.${pkgs.stdenv.hostPlatform.system}.config.build.check self;
          cabal-check =
            pkgs.runCommand "cabal-check"
              {
                src = self;
                nativeBuildInputs = [
                  project.pkg-set.config.ghc.package
                  pkgs.cabal-install
                ];
              }
              ''
                cd $src
                cabal check
                touch $out
              '';
        }
      );

      apps = forEachSystem (
        pkgs:
        (flakeFor.${pkgs.stdenv.hostPlatform.system}.apps or { })
        // {
          verify = {
            type = "app";
            meta = { };
            program = toString (pkgs.writeShellScript "verify" ''
              cd verify
              files="$(ls app/*.hs | ${pkgs.fzf}/bin/fzf -m --history .fzf-history)"
              touch $files
              oj-verify run $files --tle 30 -j $(nproc)
            '');
          };
          verify-all = {
            type = "app";
            meta = { };
            program = toString (pkgs.writeShellScript "verify-all" ''
              cd verify
              touch app/*
              oj-verify run app/*.hs --tle 30 -j $(nproc)
            '');
          };
        }
      );

      devShells = forEachSystem (pkgs: {
        default = projectFor.${pkgs.stdenv.hostPlatform.system}.shell;
      });

      formatter = forEachSystem (pkgs: treefmtEval.${pkgs.stdenv.hostPlatform.system}.config.build.wrapper);
    };
}
