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

    httpHost = lib.mkOption {
      type = lib.types.str;
      default = "localhost";
      description = ''
        Hostname or IP address for the MCP HTTP server to listen on.
      '';
    };

    httpPort = lib.mkOption {
      type = lib.types.port;
      default = 9141;
      description = ''
        Port for the MCP HTTP server to listen on.
      '';
    };

    logRequests = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Log MCP requests on stdout.
      '';
    };

    logVMs = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Log VM stdout and stderr on stdout.
      '';
    };

    commandTimeoutDefault = lib.mkOption {
      type = lib.types.int;
      default = 300;
      description = ''
        Default wall-clock timeout for commands sent to VMs, in seconds.
      '';
    };

    commandTimeoutMax = lib.mkOption {
      type = lib.types.int;
      default = 3600;
      description = ''
        Maximum wall-clock timeout a command request may select, in seconds.
      '';
    };

    staleVMTimeout = lib.mkOption {
      type = lib.types.int;
      default = 86400;
      description = ''
        Destroy VMs that have not been touched for this many seconds.
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
    assertions = [
      {
        assertion = cfg.commandTimeoutDefault >= 1;
        message = "services.devm.commandTimeoutDefault must be at least 1.";
      }
      {
        assertion = cfg.commandTimeoutMax >= cfg.commandTimeoutDefault;
        message = "services.devm.commandTimeoutMax must be at least services.devm.commandTimeoutDefault.";
      }
      {
        assertion = cfg.staleVMTimeout >= 1;
        message = "services.devm.staleVMTimeout must be at least 1.";
      }
    ];

    systemd.services.devm = {
      description = "Devm MCP server";
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.openssh ];

      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "devm";
        WorkingDirectory = "%S/devm";
        Restart = "on-failure";
        ExecStart = lib.escapeShellArgs (
          [ "${cfg.package}/bin/devm-server" ]
          ++ lib.concatMap (executable: [ "--vm" executable ]) cfg.vmExecutables
          ++ [ "--host" cfg.httpHost ]
          ++ [ "--port" (toString cfg.httpPort) ]
          ++ [ "--ssh-key" cfg.sshKey ]
          ++ [ "--command-timeout-default" (toString cfg.commandTimeoutDefault) ]
          ++ [ "--command-timeout-max" (toString cfg.commandTimeoutMax) ]
          ++ [ "--stale-vm-timeout" (toString cfg.staleVMTimeout) ]
          ++ lib.optionals cfg.logRequests [ "--log-requests" ]
          ++ lib.optionals cfg.logVMs [ "--log-vms" ]
        );
      };
    };
  };
}
