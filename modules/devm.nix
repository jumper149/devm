{ microvm }:
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkOption types;

  cfg = config.devm;

  slots = lib.range 0 (cfg.slots - 1);

  slotName = s: "devvm-${toString s}";
  slotTap = s: "devm-tap${toString s}";
  slotHost = slotName;
  slotIp = s: "${cfg.ipPrefix}.${toString (cfg.ipOffset + s)}";

  slotMac = s:
    let
      hex = lib.toHexString (s + 1);
      octet = if lib.stringLength hex == 1 then "0${hex}" else hex;
    in "02:00:00:00:00:${octet}";

  mkGuest = import ./guest.nix { inherit microvm; };

  devmSpawn = pkgs.callPackage ../pkgs/devm-spawn.nix {
    inherit (cfg) slots ipPrefix ipOffset;
  };
  devmClean = pkgs.callPackage ../pkgs/devm-clean.nix { };
  devmCli = pkgs.callPackage ../pkgs/devm.nix { };
in
{
  imports = [ microvm.nixosModules.host ];

  options.devm = {
    enable = mkEnableOption "devm disposable Firecracker microVM service";

    slots = mkOption {
      type = types.ints.positive;
      default = 4;
      description = "Number of prebuilt VM slots (maximum concurrent VMs).";
    };

    mem = mkOption {
      type = types.ints.positive;
      default = 4096;
      description = "Guest RAM in MiB.";
    };

    vcpu = mkOption {
      type = types.ints.positive;
      default = 4;
      description = "Guest vCPUs.";
    };

    workspaceSize = mkOption {
      type = types.ints.positive;
      default = 16384;
      description = "Workspace image size in MiB (sparse).";
    };

    overlaySize = mkOption {
      type = types.ints.positive;
      default = 8192;
      description = "Writable /nix/store overlay size in MiB (sparse).";
    };

    ipPrefix = mkOption {
      type = types.str;
      default = "10.100.0";
      description = "First three octets of the private VM subnet.";
    };

    ipOffset = mkOption {
      type = types.ints.positive;
      default = 10;
      description = "Offset of the first VM address within the subnet.";
    };

    gateway = mkOption {
      type = types.str;
      default = "10.100.0.1";
      description = "Host bridge address, used as the guest default gateway.";
    };

    dns = mkOption {
      type = types.listOf types.str;
      default = [ "1.1.1.1" "9.9.9.9" ];
      description = "DNS servers handed to guests.";
    };

    uplinkInterface = mkOption {
      type = types.str;
      default = "";
      description = "Host interface with internet access, used for NAT.";
    };

    users = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Existing host users to add to the devm group.";
    };

    authorizedKeys = mkOption {
      type = types.listOf types.str;
      default = lib.concatMap
        (u: lib.attrByPath [ u "openssh" "authorizedKeys" "keys" ] [ ] config.users.users)
        cfg.users;
      defaultText = lib.literalExpression "SSH keys of devm.users";
      description = "SSH public keys authorized for the guest dev user.";
    };

    guestPackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = "Extra packages to install in every guest.";
    };

    guestStateVersion = mkOption {
      type = types.str;
      default = "24.11";
      description = "system.stateVersion for the guest.";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.uplinkInterface != "";
        message = "devm.uplinkInterface must be set to the host's internet-facing interface.";
      }
      {
        assertion = cfg.authorizedKeys != [ ];
        message = "devm.authorizedKeys is empty. Set devm.users to users with Nix-defined SSH keys, or set devm.authorizedKeys directly.";
      }
    ] ++ map
      (u: {
        assertion =
          (config.users.users.${u}.isNormalUser or false)
          || (config.users.users.${u}.isSystemUser or false);
        message = "devm.users: host user '${u}' does not exist.";
      })
      cfg.users;

    microvm.vms = lib.listToAttrs (map
      (s: {
        name = slotName s;
        value = {
          autostart = false;
          restartIfChanged = false;
          config = mkGuest {
            hostName = slotHost s;
            tap = slotTap s;
            mac = slotMac s;
            ip = slotIp s;
            gateway = cfg.gateway;
            dns = cfg.dns;
            mem = cfg.mem;
            vcpu = cfg.vcpu;
            workspaceSize = cfg.workspaceSize;
            overlaySize = cfg.overlaySize;
            authorizedKeys = cfg.authorizedKeys;
            packages = cfg.guestPackages;
            stateVersion = cfg.guestStateVersion;
          };
        };
      })
      slots);

    users.groups.devm = { };
    users.groups.kvm = { };
    users.users.devm = {
      isSystemUser = true;
      group = "devm";
      extraGroups = [ "kvm" ];
      description = "devm microVM controller";
    };
    users.users = lib.genAttrs cfg.users (_: {
      extraGroups = [ "devm" ];
    });

    environment.systemPackages = [ devmCli ];

    systemd.tmpfiles.rules = [
      "d /var/lib/devm 0750 devm devm -"
      "d /var/lib/devm/instances 0750 devm devm -"
    ];

    systemd.services."devm@" = {
      description = "devm: allocate a slot for instance %i";
      serviceConfig = {
        Type = "oneshot";
        User = "devm";
        Group = "devm";
        SupplementaryGroups = [ "kvm" ];
        UMask = "0022";
        ExecStart = "${devmSpawn}/bin/devm-spawn %i";
        StateDirectory = "devm";
        StateDirectoryMode = "0750";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ "/var/lib/devm" "/var/lib/microvms" ];
      };
    };

    systemd.services."devm-clean@" = {
      description = "devm: remove instance %i";
      serviceConfig = {
        Type = "oneshot";
        User = "devm";
        Group = "devm";
        SupplementaryGroups = [ "kvm" ];
        ExecStart = "${devmClean}/bin/devm-clean %i";
        StateDirectory = "devm";
        StateDirectoryMode = "0750";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ "/var/lib/devm" "/var/lib/microvms" ];
      };
    };

    systemd.services."microvm@".serviceConfig = {
      UMask = "0002";
      MemoryMax = "${toString (cfg.mem + 1024)}M";
      CPUQuota = "${toString (cfg.vcpu * 100)}%";
      TasksMax = 2048;
      NoNewPrivileges = true;
    };

    systemd.services."microvm-tap-interfaces@".after = [ "systemd-networkd.service" ];

    boot.kernelModules = [ "tun" "tap" ];
    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

    systemd.network = {
      enable = true;
      netdevs."10-devm-br" = {
        netdevConfig = {
          Name = "br-devvm";
          Kind = "bridge";
        };
      };
      networks."10-devm-br" = {
        matchConfig.Name = "br-devvm";
        networkConfig = {
          Address = [ "${cfg.gateway}/24" ];
          ConfigureWithoutCarrier = true;
        };
      };
      networks."10-devm-taps" = {
        matchConfig.Name = "devm-tap*";
        networkConfig.Bridge = "br-devvm";
      };
    };

    networking.nftables.enable = true;
    networking.nftables.ruleset = ''
      table inet devm {
        chain forward {
          type filter hook forward priority -10; policy accept;
          iifname "br-devvm" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop
          iifname "br-devvm" ip6 daddr { fc00::/7, fe80::/10 } drop
          iifname "br-devvm" oifname "${cfg.uplinkInterface}" accept
          iifname "br-devvm" drop
        }
        chain input {
          type filter hook input priority -10; policy accept;
          iifname "br-devvm" ct state new drop
        }
        chain postrouting {
          type nat hook postrouting priority srcnat; policy accept;
          iifname "br-devvm" oifname "${cfg.uplinkInterface}" masquerade
        }
      }
    '';

    networking.firewall = {
      enable = true;
      extraInputRules = ''
        iifname "br-devvm" ct state new drop
      '';
      extraForwardRules = ''
        iifname "br-devvm" oifname "${cfg.uplinkInterface}" accept
      '';
    };

    networking.extraHosts = lib.concatMapStrings
      (s: "${slotIp s} ${slotHost s}\n")
      slots;

    security.polkit.enable = true;
    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (action.id == "org.freedesktop.systemd1.manage-units" &&
            subject.isInGroup("devm")) {
          var unit = action.lookup("unit");
          if (unit && (unit.indexOf("devm@") === 0 ||
                       unit.indexOf("devm-clean@") === 0 ||
                       unit.indexOf("install-microvm-devvm-") === 0 ||
                       unit.indexOf("microvm@devvm-") === 0)) {
            return polkit.Result.YES;
          }
        }
      });
    '';
  };
}
