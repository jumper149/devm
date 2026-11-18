{ self, nixpkgs, weeder-nix }: rec {

  packages.x86_64-linux.default = packages.x86_64-linux.staticExecutables;

  packages.x86_64-linux.staticExecutables =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    haskell.lib.justStaticExecutables packages.x86_64-linux.package;

  packages.x86_64-linux.package =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    let source = nix-gitignore.gitignoreSource [] ./.;
    in haskellPackages.callCabal2nixWithOptions "devm" source "-fcabal2nix" {};

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    haskellPackages.shellFor {
      buildInputs = with haskellPackages; [
        cabal-install
        fourmolu
        ghcid
        haskell-language-server
        hlint
        implicit-hie
        weeder
      ];
      packages = haskellPackages: [
        packages.x86_64-linux.default
      ];
      withHoogle = true;
    };

  checks.x86_64-linux.staticExecutables = packages.x86_64-linux.staticExecutables;

  checks.x86_64-linux.package = packages.x86_64-linux.package;

  checks.x86_64-linux.devShell = devShells.x86_64-linux.default;

  checks.x86_64-linux.fourmolu =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "fourmolu"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        fourmolu --mode check ./source
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        haskellPackages.fourmolu
      ];
    };

  checks.x86_64-linux.hlint =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "hlint"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        hlint ./source
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        haskellPackages.hlint
      ];
    };

  checks.x86_64-linux.hie-yaml =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "hie-yaml"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        diff --report-identical-files ./hie.yaml <(gen-hie)
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        haskellPackages.implicit-hie
      ];
    };

  checks.x86_64-linux.graphmod =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "graphmod"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        graphmod > graphmod.out
        dot -Tdot graphmod.out > graphmod.dot
        dot -Tpdf graphmod.out > graphmod.pdf
      '';
      installPhase = ''
        mkdir $out
        cp graphmod.dot $out
        cp graphmod.pdf $out
      '';
      nativeBuildInputs = [
        haskellPackages.graphmod
        pkgs.graphviz
      ];
    };

  checks.x86_64-linux.weeder =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    weeder-nix.lib.x86_64-linux.makeWeederCheck {
      haskellPackages = haskellPackages.extend (self: super: { devm = packages.x86_64-linux.package; });
      packages = [ "devm" ];
      weederToml = ./weeder.toml;
    };

  overlays.default = final: prev: {
    haskellPackages = prev.haskell.packages.ghc9124.extend (haskellFinal: haskellPrev:
    let
      source = {
        mcp-server = prev.fetchFromGitHub {
          owner  = "jumper149";
          repo   = "haskell-mcp-server";
          rev    = "2143eb677063a5fc38bd478a05779ee6267a8c83";
          sha256 = "sha256-F1ChL7PklK01nAL30RaY55qX5JsR9jHolBlwmntJ9oQ=";
        };
      };
    in {
      mcp-server = haskellPrev.callCabal2nix "mcp-server" source.mcp-server.outPath {};
    });
  };

}
