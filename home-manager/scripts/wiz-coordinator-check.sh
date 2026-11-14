#!/bin/sh
# Read-only coordinator check, run every 30 minutes by wiz-coordinator-check.timer.
# Declared in home-manager/wiz-coordinator.nix; logs under ~/.local/state/wiz.
# It never merges, pushes, or cancels anything.
set -u
cd /workspace/wiz-lang || exit 1
state=${XDG_STATE_HOME:-"$HOME/.local/state"}/wiz
mkdir -p "$state"
log="$state/coordinator-check.log"
latest="$state/coordinator-check-latest.txt"
tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT

{
  echo "=== $(date -Is) ==="
  echo "master: $(git log -1 --format='%h %s' origin/master 2>/dev/null)"
  echo "open PRs:"
  for p in $(gh pr list -R wiz-pl/wiz --state open --limit 30 --json number --jq '.[].number' 2>/dev/null); do
    s=$(gh pr checks "$p" -R wiz-pl/wiz 2>/dev/null | awk -F'\t' '{print $2}' | sort | uniq -c | tr '\n' ' ')
    b=$(gh pr view "$p" -R wiz-pl/wiz --json headRefName --jq .headRefName 2>/dev/null)
    echo "  #$p ${b:-?}: ${s:-no checks}"
  done
  echo "CI runs:"
  gh run list -R wiz-pl/wiz --limit 12 --json status,headBranch \
    --template '{{range .}}  {{.status}} {{.headBranch}}{{"\n"}}{{end}}' 2>/dev/null |
    sort | uniq -c || echo "  (gh failed)"
  echo "key branches:"
  for b in agent/stack-lang agent/stack-backends agent/nix-cache \
           agent/conv-proof-field agent/tx-accepts agent/rv-adder; do
    printf '  %-22s ' "$b"
    git log -1 --format='%h %s' "$b" 2>/dev/null | cut -c1-70 || echo missing
  done
  echo "heavy locks: $(ls /workspace/.locks 2>/dev/null | tr '\n' ' ')"
  echo
} > "$tmp" 2>&1

cat "$tmp" >> "$log"
cp "$tmp" "$latest"
if [ -d /workspace/wiz-tools ]; then
  cp "$tmp" /workspace/wiz-tools/coordinator-check-latest.txt 2>/dev/null || true
fi
tail -n 6000 "$log" > "$log.tmp" && mv "$log.tmp" "$log"
