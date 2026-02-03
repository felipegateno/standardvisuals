from pathlib import Path
import sys


def _sync_file(src: Path, dst: Path) -> bool:
    dst.parent.mkdir(parents=True, exist_ok=True)
    data = src.read_bytes()
    if not dst.exists() or dst.read_bytes() != data:
        dst.write_bytes(data)
        return True
    return False


def main() -> int:
    repo_root = Path(__file__).resolve().parent
    sources = ["colors.json", "style_tokens.json"]
    targets = [
        repo_root / "pystandarvisuals" / "src" / "pystandarvisuals" / "data",
        repo_root / "rstandarvisuals" / "inst",
    ]

    updated = []
    for name in sources:
        src = repo_root / name
        if not src.exists():
            print(f"[sync_json] No existe el archivo fuente: {src}")
            return 1
        for target_dir in targets:
            dst = target_dir / name
            if _sync_file(src, dst):
                updated.append(dst)

    if updated:
        print("[sync_json] Archivos actualizados:")
        for path in updated:
            print(f"  - {path.relative_to(repo_root)}")
    else:
        print("[sync_json] Sin cambios.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
