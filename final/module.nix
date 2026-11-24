{ self }:
{ config, lib, pkgs, ... }:
let
  cfg = config.services.devm;
in
{
  options.services.devm = {
    enable = lib.mkEnableOption "devm server";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.subflakes.server.packages.${pkgs.system}.default;
      defaultText = lib.literalExpression "self.subflakes.server.packages.\${pkgs.system}.default";
      description = "The devm server package to use.";
    };

    vmExecutables = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Executables to run as VMs. Each entry is passed as a separate --vm
        argument, and VM IDs are assigned in order.
      '';
    };

    readyMarker = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        If non-empty, VM stdout is logged until a line containing this text
        appears, and is discarded afterwards.
      '';
    };

    sshKey = lib.mkOption {
      type = lib.types.str;
      description = ''
        Path to the SSH private key used when sending commands to VMs.

        # Generate a key-pair.
        sudo install -d -m 0700 -o devm -g devm /var/lib/devm/ssh
        sudo ssh-keygen -t ed25519 -N "" -C devm -f /var/lib/devm/ssh/id_ed25519
        sudo chown devm:devm /var/lib/devm/ssh/id_ed25519
        sudo chmod 0600 /var/lib/devm/ssh/id_ed25519

        # And configure devm to use this key.
        services.devm.sshKey = "/var/lib/devm/ssh/id_ed25519";
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.devm = {
      description = "Devm MCP server";
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.openssh ];

      serviceConfig = {
        DynamicUser = true;
        SupplementaryGroups = [ "kvm" ];
        AmbientCapabilities = [ "CAP_NET_ADMIN" ];
        StateDirectory = "devm";
        WorkingDirectory = "%S/devm";
        Restart = "on-failure";
        ExecStart = lib.escapeShellArgs (
          [ "${cfg.package}/bin/devm-server" ]
          ++ lib.concatMap (executable: [ "--vm" executable ]) cfg.vmExecutables
          ++ lib.optionals (cfg.readyMarker != "") [ "--ready" cfg.readyMarker ]
          ++ [ "--ssh-key" cfg.sshKey ]
        );
      };
    };
  };
}
