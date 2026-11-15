{ writeShellApplication
, coreutils
, jq
, systemd
}:
writeShellApplication {
  name = "devm-clean";
  runtimeInputs = [ coreutils jq systemd ];

  text = ''
    set -euo pipefail

    id="''${1:?usage: devm-clean <instance>}"
    state_dir=/var/lib/devm/instances
    microvm_dir=/var/lib/microvms

    if [ ! -e "$state_dir/$id.json" ]; then
      exit 0
    fi

    slot=$(jq -r '.slot' "$state_dir/$id.json")
    host="devvm-$slot"

    if systemctl is-active --quiet "microvm@$host.service"; then
      echo "devm-clean: stop $id before removing it" >&2
      exit 1
    fi

    rm -f "$microvm_dir/$host/workspace.img" \
          "$microvm_dir/$host/nix-store-overlay.img" \
          "$microvm_dir/$host/firecracker.sock" \
          "$microvm_dir/$host/firecracker.sock.old"

    rm -f "$state_dir/$id.json"
    echo "devm: removed $id ($host)"
  '';
}
