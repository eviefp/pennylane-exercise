{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  };

  outputs =
    { self, nixpkgs }:
    let
      allSystems = [
        "x86_64-linux" # 64bit AMD/Intel x86
      ];

      forAllSystems = fn:
        nixpkgs.lib.genAttrs allSystems
          (system: fn { pkgs = import nixpkgs { inherit system; }; });
      host = "127.0.0.1";
      port = "5432";
      db = "pennylane";
      user = "exercise";
      password = "exercise";
    in
    {
      packages = forAllSystems ({ pkgs }:
        let
          hlib = pkgs.haskell.lib;
          hp = pkgs.haskell.packages.ghc963.override {
            overrides = hself: hsuper: {
              org-parser = hlib.doJailbreak (hlib.markUnbroken hsuper.org-parser);
              async-timer = hlib.dontCheck (hlib.doJailbreak (hlib.markUnbroken hsuper.async-timer));
            };
          };
        in
        rec
        {
          default = hp.callCabal2nix "pennylane-exercise" ./. { };

          docker = pkgs.dockerTools.buildLayeredImage {
            name = "registry.fly.io/pennylane-exercise";
            tag = "v0";
            contents = with pkgs; [ cacert ];
            config.Cmd = "${default}/bin/pennylane-exercise";
          };
        });
      devShells = forAllSystems ({ pkgs }: {
        default = pkgs.mkShell {
          name = "pennylane-exercise-shell";
          nativeBuildInputs = [
            pkgs.zlib.dev
            pkgs.haskell.compiler.ghc963
            pkgs.haskell.packages.ghc963.cabal-install
            pkgs.haskell.packages.ghc963.cabal2nix
            pkgs.haskell.packages.ghc963.haskell-language-server
            pkgs.postgresql_16
          ];

          PGHOST = host;
          PGPORT = port;
          PGDATABASE = db;
          PGUSER = user;
          PGPASSWORD = password;
          DATABASE_URL = "postgres://${user}:${password}@${host}:${port}/${db}?sslmode=disable";
        };
      });
    };
}
