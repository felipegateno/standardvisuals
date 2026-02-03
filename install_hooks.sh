#!/usr/bin/env sh
set -e

ROOT="${1:-$(pwd)}"
HOOK_DIR="$ROOT/.git/hooks"
SOURCE="$ROOT/.git/hooks/pre-commit"
TARGET="$HOOK_DIR/pre-commit"

if [ ! -d "$HOOK_DIR" ]; then
  echo "No se encontró .git/hooks. ¿Estás en la raíz del repo?"
  exit 1
fi

cp "$SOURCE" "$TARGET"
chmod +x "$TARGET"
echo "Hook pre-commit instalado en $TARGET"
