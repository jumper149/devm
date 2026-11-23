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
  };

  config = lib.mkIf cfg.enable {
    systemd.services.devm = {
      description = "Devm MCP server";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        DynamicUser = true;
        SupplementaryGroups = [ "kvm" ];
        StateDirectory = "devm";
        WorkingDirectory = "%S/devm";
        Restart = "on-failure";
        ExecStart = lib.escapeShellArgs (
          [ "${cfg.package}/bin/devm-server" ]
          ++ lib.concatMap (executable: [ "--vm" executable ]) cfg.vmExecutables
          ++ lib.optionals (cfg.readyMarker != "") [ "--ready" cfg.readyMarker ]
        );
      };
    };
  };
}
