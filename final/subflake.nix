{ self, nixpkgs }: {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "devm-server-full" ''
      #!${pkgs.bash}/bin/bash
      ${self.subflakes.server.packages.x86_64-linux.default}/bin/devm-server
    '';

  nixosModules.default = import ./module.nix { inherit self; };

}
