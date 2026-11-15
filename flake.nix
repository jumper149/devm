{
  description = "devm: disposable Firecracker microVMs for untrusted local repos";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    microvm = {
      type = "github";
      owner = "microvm-nix";
      repo = "microvm.nix";
      ref = "main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, microvm }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      pkgsFor = system: import nixpkgs { inherit system; };
    in
    {
      nixosModules.devm = import ./modules/devm.nix { inherit microvm; };
      nixosModules.default = self.nixosModules.devm;

      packages = forAllSystems (system:
        let pkgs = pkgsFor system; in {
          devm = pkgs.callPackage ./pkgs/devm.nix { };
        });

      # Evaluation-only demo host. Replace the interface/keys before deploying.
      nixosConfigurations.devm-demo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.devm
          {
            system.stateVersion = "24.11";
            networking.hostName = "devm-demo";
            devm = {
              enable = true;
              uplinkInterface = "eth0";
              authorizedKeys = [
                "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDemoOnlyReplaceMe devm-demo"
              ];
            };
          }
        ];
      };
    };
}
