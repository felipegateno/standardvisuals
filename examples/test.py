"""
Prueba simple de tema desde style_tokens.json (matplotlib).
"""

import os
import sys
from pathlib import Path

import pandas as pd
import matplotlib.pyplot as plt

script_dir = Path(__file__).resolve().parent
repo_root = script_dir.parent

# Agregar el paquete pystandarvisuals al path
sys.path.insert(0, str(repo_root / "pystandarvisuals" / "src"))

from pystandarvisuals import create_theme_from_tokens


def main():
    print("Leyendo datos de ts_pr_daily.csv...")
    df = pd.read_csv(script_dir / "ts_pr_daily.csv")
    df["Fecha"] = pd.to_datetime(df["Fecha"], format="%d-%m-%Y")
    df = df.rename(columns={"Estacion_mm": "A"})

    # Crear series adicionales para mostrar facetas
    df["B"] = df["A"] * 0.9
    df["C"] = df["A"] * 1.1
    df["D"] = df["A"] * 1.0

    otros = df.set_index("Fecha")[["A", "B", "C", "D"]]

    print("Generando gráfico de prueba...")
    axes = otros.plot(subplots=True, figsize=(20, len(otros.columns) * 2), grid=True)

    fig = axes[0].figure
    fig.suptitle("Otros cauces")

    for ax in axes:
        create_theme_from_tokens(ax, str(repo_root / "style_tokens.json"))

    fig.subplots_adjust(top=0.9)

    os.makedirs(script_dir / "figures", exist_ok=True)
    out_path = script_dir / "figures" / "py_test_tokens_simple.png"
    plt.savefig(out_path, dpi=300, bbox_inches=None, pad_inches=0)
    print(f"Gráfico guardado en: {out_path}")


if __name__ == "__main__":
    main()

