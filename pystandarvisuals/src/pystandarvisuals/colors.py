import json
import os
from pathlib import Path
import importlib.resources as resources


def _resolve_colors_path(colors_path):
    candidates = []
    if colors_path:
        candidates.append(colors_path)
        candidates.append(os.path.join(os.getcwd(), colors_path))
        for parent in Path(__file__).resolve().parents:
            candidates.append(str(parent / colors_path))
    for parent in Path(__file__).resolve().parents:
        candidates.append(str(parent / "colors.json"))

    for path in candidates:
        if os.path.exists(path):
            return path

    try:
        return str(resources.files("pystandarvisuals").joinpath("data/colors.json"))
    except Exception:
        raise FileNotFoundError(
            "No se encontró colors.json. Buscado en: " + ", ".join(candidates)
        )


def load_colors(colors_path=None):
    """
    Carga el archivo colors.json con paletas estandarizadas.

    Parameters
    ----------
    colors_path : str, optional
        Ruta al archivo colors.json.

    Returns
    -------
    dict
        Diccionario con paletas.
    """
    resolved = _resolve_colors_path(colors_path)
    with open(resolved, "r", encoding="utf-8") as f:
        return json.load(f)


def _select_n(palette, n):
    if n is None:
        return palette
    if not isinstance(n, int):
        raise TypeError("n debe ser un entero.")
    if n <= 0:
        raise ValueError("n debe ser mayor que 0.")
    max_n = len(palette)
    if n > max_n:
        raise ValueError(f"n={n} excede el máximo permitido ({max_n}).")
    if n == 1:
        return [palette[0]]
    if n == max_n:
        return palette
    step = (max_n - 1) / (n - 1)
    indices = [round(i * step) for i in range(n)]
    return [palette[i] for i in indices]


def get_palette(name, colors_path=None, n=None):
    """
    Retorna una paleta por nombre.

    Parameters
    ----------
    name : str
        Nombre de la paleta.
    colors_path : str, optional
        Ruta al archivo colors.json.
    n : int, optional
        Número de colores a retornar (máximo = tamaño de la paleta).

    Returns
    -------
    list
        Lista de colores hex.
    """
    colors = load_colors(colors_path)
    if name not in colors:
        raise KeyError(f"Paleta no encontrada: {name}")
    return _select_n(colors[name], n)

