{
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

  outputs = { self, nixpkgs, microvm }@inputs: {

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      pkgs.hello;

    nixosConfigurations.my-microvm = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        microvm.nixosModules.microvm
        {
          networking.hostName = "my-microvm";
          microvm.hypervisor = "cloud-hypervisor";
        }
      ];
    };

  };
}
