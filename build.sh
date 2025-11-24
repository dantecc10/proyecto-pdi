#!/usr/bin/env bash
set -euo pipefail

# build.sh - Compila el proyecto Lazarus/Free Pascal desde la línea de comandos
# Intentará usar `lazbuild` (sin abrir la IDE). Si no existe, hará un intento
# de fallback usando `fpc` y mostrará instrucciones para instalar Lazarus.

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$PROJECT_DIR"

echo "Proyecto: $PROJECT_DIR"

# Buscar archivos de proyecto
LPI_FILE=$(ls *.lpi 2>/dev/null | head -n1 || true)
LPR_FILE=$(ls *.lpr 2>/dev/null | head -n1 || true)

if command -v lazbuild >/dev/null 2>&1; then
  echo "lazbuild encontrado: usando lazbuild para compilar"
  if [ -n "$LPI_FILE" ]; then
    lazbuild "$LPI_FILE"
  elif [ -n "$LPR_FILE" ]; then
    lazbuild "$LPR_FILE"
  else
    echo "No se encontró .lpi ni .lpr en el directorio del proyecto. Abortando." >&2
    exit 1
  fi
  echo "Compilación finalizada (lazbuild)."
  exit 0
fi

echo "lazbuild no está disponible en PATH. Intentando fallback con fpc (puede fallar)."

if ! command -v fpc >/dev/null 2>&1; then
  cat <<EOF
Error: ni 'lazbuild' ni 'fpc' están disponibles.
Instala Lazarus (incluye lazbuild) o Free Pascal (fpc).
En Debian/Ubuntu: sudo apt install lazarus
En Arch: sudo pacman -S lazarus
EOF
  exit 2
fi

if [ -n "$LPR_FILE" ]; then
  echo "Compilando $LPR_FILE con fpc (fallback)..."
  # Opciones conservadoras: modo ObjFPC, mostrar mensajes, incluir debug
  fpc -MObjFPC -Sc -vewnhi -g "$LPR_FILE"
  rc=$?
  if [ $rc -eq 0 ]; then
    echo "Compilación con fpc finalizada correctamente."
  else
    echo "Compilación con fpc falló (codigo $rc). Intentar instalar Lazarus/lazbuild para builds fiables." >&2
  fi
  exit $rc
fi

echo "No se encontró archivo .lpr para compilar con fpc. Busca un .lpi o abre el proyecto en Lazarus." >&2
exit 3
