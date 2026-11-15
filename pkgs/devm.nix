{ writeShellApplication
, coreutils
, jq
, openssh
, rsync
, systemd
}:
writeShellApplication {
  name = "devm";
  runtimeInputs = [ coreutils jq openssh rsync systemd ];

  text = ''
    set -euo pipefail

    svc_state=/var/lib/devm/instances
    cli_state="''${XDG_STATE_HOME:-$HOME/.local/state}/devm"
    mkdir -p "$cli_state"

    ssh_opts=(
      -o StrictHostKeyChecking=no
      -o UserKnownHostsFile=/dev/null
      -o LogLevel=ERROR
      -o ConnectTimeout=5
      -o BatchMode=yes
    )
    if [ -n "''${DEVM_KEY:-}" ]; then
      ssh_opts+=(-i "$DEVM_KEY")
    fi

    die() { echo "devm: $*" >&2; exit 1; }

    usage() {
      cat <<EOF
    Usage: devm <command> [args]

      new <local-dir> [name]   Allocate a VM, seed it from local-dir, print connection info
      ls                       List instances
      ssh <name> [command...]  SSH into an instance
      sync <name>              Copy the guest workspace back to the local directory
      start <name>             Start a stopped instance
      stop <name>              Stop a running instance
      rm <name>                Stop and delete an instance
      logs <name>              Show the hypervisor journal

    Environment:
      DEVM_KEY                 SSH identity file to use (default: your ssh config/agent)
EOF
    }

    svc_json() {
      local id=$1
      [ -e "$svc_state/$id.json" ] || die "unknown instance '$id'"
      cat "$svc_state/$id.json"
    }

    cli_json() {
      local id=$1
      [ -e "$cli_state/$id.json" ] || die "no local state for '$id' (was it created by this user?)"
      cat "$cli_state/$id.json"
    }

    wait_ssh() {
      local ip=$1 i
      for i in $(seq 1 120); do
        if ssh "''${ssh_opts[@]}" "dev@$ip" true >/dev/null 2>&1; then
          return 0
        fi
        sleep 0.5
      done
      die "timed out waiting for SSH on $ip"
    }

    cmd_new() {
      local dir=''${1:-} name=''${2:-}
      [ -n "$dir" ] || die "new: missing local directory"
      [ -d "$dir" ] || die "not a directory: $dir"
      dir=$(realpath "$dir")

      if [ -z "$name" ]; then
        local base
        base=$(basename "$dir" | tr -c 'A-Za-z0-9_.-' '-' | sed 's/^-*//; s/-*$//')
        [ -n "$base" ] || base=repo
        name="$base-$(printf '%04x' $((RANDOM % 65536)))"
      fi

      systemctl start "devm@$name.service" || die "failed to spawn $name"
      for _ in $(seq 1 60); do
        [ -e "$svc_state/$name.json" ] && break
        sleep 0.25
      done
      [ -e "$svc_state/$name.json" ] || die "spawn service did not create state for $name"

      local slot ip host
      slot=$(jq -r .slot "$svc_state/$name.json")
      ip=$(jq -r .ip "$svc_state/$name.json")
      host=$(jq -r .host "$svc_state/$name.json")

      wait_ssh "$ip"
      rsync -a --delete --safe-links --no-owner --no-group \
        -e "ssh ''${ssh_opts[*]}" \
        "$dir/" "dev@$ip:/home/dev/repo/"

      jq -n \
        --arg id "$name" \
        --arg source "$dir" \
        --argjson slot "$slot" \
        --arg ip "$ip" \
        --arg host "$host" \
        '{id: $id, source: $source, slot: $slot, ip: $ip, host: $host}' \
        > "$cli_state/$name.json"

      echo "devm: $name is ready"
      echo "  ssh dev@$ip"
      echo "  devm ssh $name"
    }

    cmd_ls() {
      local f id host ip state
      for f in "$svc_state"/*.json; do
        [ -e "$f" ] || continue
        id=$(jq -r .id "$f")
        host=$(jq -r .host "$f")
        ip=$(jq -r .ip "$f")
        if systemctl is-active --quiet "microvm@$host.service"; then
          state=running
        else
          state=stopped
        fi
        printf '%-28s %-10s %-14s %s\n' "$id" "$host" "$ip" "$state"
      done
    }

    cmd_ssh() {
      local id=''${1:-} ip
      [ -n "$id" ] || die "ssh: missing instance"
      shift || true
      ip=$(svc_json "$id" | jq -r .ip)
      exec ssh "''${ssh_opts[@]}" "dev@$ip" "$@"
    }

    cmd_start() {
      local id=''${1:-} host ip
      [ -n "$id" ] || die "start: missing instance"
      host=$(svc_json "$id" | jq -r .host)
      ip=$(svc_json "$id" | jq -r .ip)
      systemctl start "microvm@$host.service"
      wait_ssh "$ip"
      echo "devm: $id is running at $ip"
    }

    cmd_stop() {
      local id=''${1:-} host
      [ -n "$id" ] || die "stop: missing instance"
      host=$(svc_json "$id" | jq -r .host)
      systemctl stop "microvm@$host.service"
      echo "devm: $id stopped"
    }

    cmd_sync() {
      local id=''${1:-} source ip host
      [ -n "$id" ] || die "sync: missing instance"
      source=$(cli_json "$id" | jq -r .source)
      ip=$(svc_json "$id" | jq -r .ip)
      host=$(svc_json "$id" | jq -r .host)

      if ! systemctl is-active --quiet "microvm@$host.service"; then
        systemctl start "microvm@$host.service"
      fi
      wait_ssh "$ip"

      rsync -a --delete --safe-links --no-owner --no-group \
        -e "ssh ''${ssh_opts[*]}" \
        "dev@$ip:/home/dev/repo/" "$source/"

      echo "devm: synced $id -> $source"
    }

    cmd_rm() {
      local id=''${1:-} host
      [ -n "$id" ] || die "rm: missing instance"
      host=$(svc_json "$id" | jq -r .host)
      systemctl stop "microvm@$host.service" || true
      systemctl start "devm-clean@$id.service"
      rm -f "$cli_state/$id.json"
      echo "devm: removed $id"
    }

    cmd_logs() {
      local id=''${1:-} host
      [ -n "$id" ] || die "logs: missing instance"
      host=$(svc_json "$id" | jq -r .host)
      exec journalctl -u "microvm@$host.service" -f
    }

    case "''${1:-}" in
      new)   shift; cmd_new "$@" ;;
      ls)    cmd_ls ;;
      ssh)   shift; cmd_ssh "$@" ;;
      start) shift; cmd_start "$@" ;;
      stop)  shift; cmd_stop "$@" ;;
      sync)  shift; cmd_sync "$@" ;;
      rm)    shift; cmd_rm "$@" ;;
      logs)  shift; cmd_logs "$@" ;;
      -h|--help|help|"") usage ;;
      *) die "unknown command '$1' (try: devm --help)" ;;
    esac
  '';
}
