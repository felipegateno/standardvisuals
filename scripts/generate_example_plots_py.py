import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


def main():
    repo_root = Path(__file__).resolve().parents[1]
    sys.path.insert(0, str(repo_root / "pystandarvisuals" / "src"))

    from pystandarvisuals import create_theme_from_tokens, get_palette

    fechas = pd.date_range(start="2019-01-01", periods=60, freq="MS")
    temp_base = np.array([22, 22, 21, 18, 15, 12, 11, 12, 14, 17, 19, 21])
    temp_est_1 = np.tile(temp_base, 5)

    data = pd.DataFrame(
        {
            "date": fechas,
            "Estacion_1": temp_est_1,
            "Estacion_2": temp_est_1 + 2.0,
            "Estacion_3": temp_est_1 - 2.0,
            "Estacion_4": temp_est_1 + 4.0,
        }
    )

    assets_dir = repo_root / "assets"

    # Figura 1: 2 estaciones
    colors_obs_sim = get_palette("standardcolors_obsvssim")
    fig, ax = plt.subplots(figsize=(12, 4))
    ax.plot(
        data["date"],
        data["Estacion_1"],
        color=colors_obs_sim["black"],
        label="Estacion_1",
    )
    ax.plot(
        data["date"],
        data["Estacion_2"],
        color=colors_obs_sim["pure_blue"],
        label="Estacion_2",
    )
    ax.set_title("Imagen creada en Python: Temperatura mensual sintética – comparación de 2 estaciones")
    ax.set_xlabel("Fecha")
    ax.set_ylabel("Temperatura mensual [°C]")
    ax.legend()
    ax.margins(x=0)
    ax.set_xlim(data["date"].min(), data["date"].max())
    create_theme_from_tokens(ax)
    fig.tight_layout()
    fig.savefig(assets_dir / "p1_py.png", dpi=200, bbox_inches="tight")
    plt.close(fig)

    # Figura 2: 4 estaciones
    colors_div = get_palette("standardcolors_divergent", n=4)
    fig, ax = plt.subplots(figsize=(12, 4))
    station_cols = ["Estacion_1", "Estacion_2", "Estacion_3", "Estacion_4"]
    for idx, (color_name, color_hex) in enumerate(colors_div.items()):
        ax.plot(
            data["date"],
            data[station_cols[idx]],
            color=color_hex,
            label=station_cols[idx],
        )
    ax.set_title("Imagen creada en Python: Temperatura mensual sintética – 4 estaciones")
    ax.set_xlabel("Fecha")
    ax.set_ylabel("Temperatura mensual [°C]")
    ax.legend(ncol=2)
    ax.margins(x=0)
    ax.set_xlim(data["date"].min(), data["date"].max())
    create_theme_from_tokens(ax)
    fig.tight_layout()
    fig.savefig(assets_dir / "p2_py.png", dpi=200, bbox_inches="tight")
    plt.close(fig)


if __name__ == "__main__":
    main()
