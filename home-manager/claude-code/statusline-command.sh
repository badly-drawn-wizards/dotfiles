#!/usr/bin/env bash
# Status line modeled on the user's spaceship zsh prompt: time, dir, git branch/status.
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
model=$(echo "$input" | jq -r '.model.display_name // empty')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# time (yellow)
printf '\033[33m%s\033[0m' "$(date +%H:%M:%S)"

# user@host only over ssh or as root (spaceship default)
if [ -n "$SSH_CONNECTION" ] || [ "$(id -u)" = 0 ]; then
  printf ' \033[33m%s\033[0m' "$(whoami)@$(hostname -s)"
fi

# dir (cyan bold), ~ abbreviated
dir="${cwd/#$HOME/\~}"
printf ' in \033[1;36m%s\033[0m' "$dir"

# git (magenta branch, red status flag)
if git -C "$cwd" --no-optional-locks rev-parse --git-dir >/dev/null 2>&1; then
  branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null || git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  printf ' on \033[1;35m %s\033[0m' "$branch"
  if [ -n "$(git -C "$cwd" --no-optional-locks status --porcelain 2>/dev/null)" ]; then
    printf '\033[31m [!]\033[0m'
  fi
fi

[ -n "$model" ] && printf ' \033[2m| %s\033[0m' "$model"
[ -n "$used" ] && printf ' \033[2m| ctx %.0f%%\033[0m' "$used"

# quota usage (5-hour and 7-day limits, when reported)
five=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
week=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
if [ -n "$five" ] || [ -n "$week" ]; then
  q=""
  [ -n "$five" ] && q="5h:$(printf '%.0f' "$five")%"
  [ -n "$week" ] && q="$q 7d:$(printf '%.0f' "$week")%"
  printf ' \033[2m| %s\033[0m' "${q# }"
fi
echo
