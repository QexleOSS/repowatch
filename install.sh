#!/bin/sh
# Download the latest repowatch binary for this OS.
set -eu
case "$(uname -s)" in
  Linux) target=ubuntu-latest ;;
  Darwin) target=macos-latest ;;
  *) echo "unsupported platform: $(uname -s)" >&2; exit 1 ;;
esac
dir="${BIN:-$HOME/.local/bin}"
url="https://github.com/QexleOSS/repowatch/releases/latest/download/repowatch-$target"
mkdir -p "$dir"
tmp=$(mktemp)
curl -fsSL "$url" -o "$tmp"
chmod +x "$tmp"
mv "$tmp" "$dir/repowatch"
echo "installed $dir/repowatch"
