#!/bin/sh
# One command, no package manager, no network.
set -e
cd "$(dirname "$0")"
ocamlopt -I +unix -I src unix.cmxa \
  src/inflate.ml src/pdf.ml src/json.ml src/lang.ml src/lex.ml src/walk.ml src/graph.ml src/index.ml src/main.ml \
  -o repowatch
echo "built ./repowatch"
