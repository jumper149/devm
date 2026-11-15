{ lib
, writeShellApplication
, coreutils
, jq
, systemd
, util-linux
, slots
, ipPrefix
, ipOffset
}:
writeShellApplication {
  name = "devm-spawn";
  runtimeInputs = [ coreutils jq systemd util-linux ];

  text = ''
    set -euo pipefail

    id="''${1:?usage: devm-spawn <instance>}"
    if [[ ! "$id" =~ ^[A-Za-z0-9_.-]+$ ]]; then
      echo "devm-spawn: invalid instance name '$id'" >&2
      exit 1
    fi

    state_dir=/var/lib/devm/instances
    microvm_dir=/var/lib/microvms
    slots=${toString slots}
    ip_prefix=${lib.escapeShellArg ipPrefix}
    ip_offset=${toString ipOffset}

    mkdir -p "$state_dir"
    exec 9>/var/lib/devm/lock
    flock 9

    if [ -e "$state_dir/$id.json" ]; then
      echo "devm-spawn: instance '$id' already exists" >&2
      exit 1
    fi

    shopt -s nullglob
    states=("$state_dir"/*.json)

    slot=
    for s in $(seq 0 $((slots - 1))); do
      if ((${#states[@]})) && jq -s -e --argjson s "$s" 'any(.[]; .slot == $s)' "''${states[@]}" >/dev/null 2>&1; then
        continue
      fi
      if systemctl is-active --quiet "microvm@devvm-$s.service"; then
        continue
      fi
      slot=$s
      break
    done

    if [ -z "$slot" ]; then
      echo "devm-spawn: no free slot (increase devm.slots or remove an instance)" >&2
      exit 1
    fi

    host="devvm-$slot"
    ip="$ip_prefix.$((ip_offset + slot))"

    # A free slot gets a clean workspace and store overlay.
    rm -f "$microvm_dir/$host/workspace.img" \
          "$microvm_dir/$host/nix-store-overlay.img" \
          "$microvm_dir/$host/firecracker.sock" \
          "$microvm_dir/$host/firecracker.sock.old"

    jq -n \
      --arg id "$id" \
      --argjson slot "$slot" \
      --arg ip "$ip" \
      --arg host "$host" \
      --arg created "$(date -u +%FT%TZ)" \
      '{id: $id, slot: $slot, ip: $ip, host: $host, created: $created}' \
      > "$state_dir/$id.json"

    # Make sure the declarative runner is installed for this slot.
    systemctl start "install-microvm-$host.service" || true

    if ! systemctl start "microvm@$host.service"; then
      rm -f "$state_dir/$id.json"
      exit 1
    fi

    echo "devm: $id -> $host ($ip)"
  '';
}
