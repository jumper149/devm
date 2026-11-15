{ microvm }:
{ hostName, tap, mac, ip, gateway, dns, mem, vcpu
, workspaceSize, overlaySize, authorizedKeys, packages, stateVersion
}:
{ lib, pkgs, ... }:
{
  imports = [ microvm.nixosModules.microvm ];

  microvm = {
    hypervisor = "firecracker";
    inherit mem vcpu;

    # Firecracker needs an API socket. Relative to the slot's working directory.
    socket = "firecracker.sock";

    interfaces = [{
      type = "tap";
      id = tap;
      inherit mac;
    }];

    volumes = [
      {
        image = "workspace.img";
        mountPoint = "/home/dev/repo";
        size = workspaceSize;
        label = "workspace";
        fsType = "ext4";
        # Guest dev is uid 1000 / gid 100. Make the mounted root writable by it.
        mkfsExtraArgs = [ "-E" "root_owner=1000:100" ];
      }
      {
        image = "nix-store-overlay.img";
        mountPoint = "/nix/.rw-store";
        size = overlaySize;
        label = "nix-overlay";
        fsType = "ext4";
      }
    ];

    # Writable /nix/store, so builds run inside the guest, never on the host.
    writableStoreOverlay = "/nix/.rw-store";
    storeOnDisk = true;
    vsock.cid = null;
  };

  networking = {
    inherit hostName;
    useDHCP = false;
    useNetworkd = true;
    enableIPv6 = false;
    firewall.allowedTCPPorts = [ 22 ];
  };

  systemd.network.networks."20-devm" = {
    matchConfig.MACAddress = mac;
    networkConfig = {
      Address = [ "${ip}/24" ];
      Gateway = gateway;
      DNS = dns;
      DHCP = "no";
    };
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  users.users.dev = {
    isNormalUser = true;
    uid = 1000;
    group = "users";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = authorizedKeys;
  };

  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    git
    rsync
    neovim
    tmux
    ripgrep
    fd
    jq
    curl
    cacert
  ] ++ packages;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  system.stateVersion = stateVersion;
}
