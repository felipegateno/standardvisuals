"""
Genera un PNG con la vista previa de las paletas definidas en colors.json.
"""

import json
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.colors import to_rgb


def main():
    repo_root = Path(__file__).resolve().parents[1]
    colors_path = repo_root / "colors.json"
    out_path = repo_root / "assets" / "palette_preview.png"

    with open(colors_path, "r", encoding="utf-8") as f:
        palettes = json.load(f)

    names = list(palettes.keys())
    n_rows = len(names)

    fig_height = max(2.5, n_rows * 0.6)
    fig, axes = plt.subplots(n_rows, 1, figsize=(10, fig_height))
    if n_rows == 1:
        axes = [axes]

    for ax, name in zip(axes, names):
        palette = palettes[name]
        rgb = [to_rgb(c) for c in palette]
        ax.imshow([rgb], aspect="auto")
        ax.set_yticks([])
        ax.set_xticks([])
        ax.set_ylabel(name, rotation=0, ha="right", va="center", fontsize=10)
        for spine in ax.spines.values():
            spine.set_visible(False)

    fig.tight_layout()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=200, bbox_inches="tight")
    plt.close(fig)


if __name__ == "__main__":
    main()

