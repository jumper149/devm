{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    microvm-nix = {
      type = "github";
      owner = "microvm-nix";
      repo = "microvm.nix";
      ref = "main";
    };
  };

  outputs = { self, nixpkgs, microvm-nix }@inputs: {

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      pkgs.hello;

  };
}
