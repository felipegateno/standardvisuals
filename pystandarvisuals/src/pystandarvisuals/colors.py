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
        Ruta al archivo colors.json. Si es None, usa el archivo incluido
        en el paquete.

    Returns
    -------
    dict
        Diccionario con todas las paletas disponibles. Cada paleta contiene
        un array de colores con sus valores hex y nombres.

    Examples
    --------
    >>> palettes = load_colors()
    >>> list(palettes.keys())  # Ver nombres de todas las paletas
    ['standardcolors_obsvssim', 'standardcolors_divergent', ...]

    See Also
    --------
    get_palette : Obtener una paleta específica
    list_palettes : Ver un resumen de paletas disponibles
    """
    resolved = _resolve_colors_path(colors_path)
    with open(resolved, "r", encoding="utf-8") as f:
        return json.load(f)


def _extract_palette_entries(palette):
    """Normaliza los colores de una paleta en una lista de dict con hex y name."""
    if not palette:
        return []
    first = palette[0]
    entries = []
    if isinstance(first, dict):
        for color in palette:
            hex_value = color.get("hex", color)
            name = color.get("name", "")
            entries.append({"hex": hex_value, "name": name})
    else:
        for color in palette:
            entries.append({"hex": color, "name": ""})
    return entries


def _select_n_entries(entries, n):
    if n is None:
        return entries
    if not isinstance(n, int):
        raise TypeError("n debe ser un entero.")
    if n <= 0:
        raise ValueError("n debe ser mayor que 0.")
    max_n = len(entries)
    if n > max_n:
        raise ValueError(f"n={n} excede el máximo permitido ({max_n}).")
    if n == 1:
        return [entries[0]]
    if n == max_n:
        return entries
    step = (max_n - 1) / (n - 1)
    indices = [round(i * step) for i in range(n)]
    return [entries[i] for i in indices]


def get_palette(name, colors_path=None, n=None, dict_output=True):
    """
    Retorna una paleta por nombre.

    Parameters
    ----------
    name : str
        Nombre de la paleta. Debe ser uno de los nombres disponibles.
        Use list_palettes() para ver todas las paletas disponibles.
    colors_path : str, optional
        Ruta al archivo colors.json. Si es None, usa el archivo incluido
        en el paquete.
    n : int, optional
        Número de colores a retornar (máximo = tamaño de la paleta).
        Si es None, retorna todos los colores de la paleta. Si es menor
        al tamaño total, selecciona colores distribuidos uniformemente.

    Notes
    -----
    Paletas disponibles:
    
    - standardcolors_obsvssim: 2 colores (negro y azul) - Para comparaciones
      observado vs simulado
    - standardcolors_divergent: 30 colores - Paleta multicolor diversa
    - standardcolors_terrain: 11 colores - Paleta tipo terreno (marrón a
      verde azulado)
    - standardcolors_rdylbu: 11 colores - Paleta Spectral (Rojo-Amarillo-Azul
      divergente, idéntica a sns.color_palette("Spectral"))
    - standardcolors_rdylgn: 11 colores - Rojo-Amarillo-Verde (divergente)
    - standardcolors_rdbu: 11 colores - Paleta coolwarm (Rojo-Azul divergente,
      idéntica a sns.color_palette("coolwarm"))
    - standardcolors_rdgn: 11 colores - Rojo-Verde (sin pasar por amarillo)
    - standardcolors_magma: 11 colores - Paleta magma (negro a amarillo,
      pasando por púrpura y rojo)
    - standardcolors_viridis: 11 colores - Paleta viridis (púrpura a amarillo,
      pasando por verde)

    Parameters (añadido)
    -------------------
    dict_output : bool, optional
        Si es True (valor por defecto) retorna un dict {name: hex}. Si es False,
        retorna la lista de dicts con campos "hex" y "name".

    Examples
    --------
    >>> # Obtener todos los colores de una paleta con nombres
    >>> pal = get_palette("standardcolors_obsvssim")
    >>> 
    >>> # Solicitar solo los hex, hipotéticamente para compatibilidad
    >>> pal_hex = get_palette("standardcolors_viridis", dict_output=False)
    >>> 
    >>> # Obtener solo 4 colores de una paleta (distribuidos uniformemente)
    >>> pal = get_palette("standardcolors_viridis", n=4)
    >>> 
    >>> # Usar en matplotlib
    >>> import matplotlib.pyplot as plt
    >>> colors = get_palette("standardcolors_rdylbu", n=5)
    >>> for i, color in enumerate(colors):
    ...     plt.plot(x, y[i], color=color)

    See Also
    --------
    load_colors : Cargar todas las paletas
    list_palettes : Ver un resumen de paletas disponibles
    """
    colors = load_colors(colors_path)
    if name not in colors:
        raise KeyError(f"Paleta no encontrada: {name}")
    palette_data = colors[name]
    if isinstance(palette_data, dict) and "colors" in palette_data:
        palette = palette_data["colors"]
    else:
        palette = palette_data

    palette_entries = _extract_palette_entries(palette)
    selected = _select_n_entries(palette_entries, n)

    if not dict_output:
        return selected

    result = {}
    for idx, entry in enumerate(selected):
        key = entry["name"] or f"color_{idx}"
        result[key] = entry["hex"]
    return result


def list_palettes(colors_path=None):
    """
    Lista todas las paletas de colores disponibles en el paquete.

    Parameters
    ----------
    colors_path : str, optional
        Ruta al archivo colors.json. Si es None, usa el archivo incluido
        en el paquete.

    Returns
    -------
    pandas.DataFrame or dict
        Si pandas está disponible, retorna un DataFrame con información sobre
        cada paleta (nombre, número de colores, nombre descriptivo).
        Si pandas no está disponible, retorna un diccionario.

    Examples
    --------
    >>> # Ver todas las paletas disponibles
    >>> palettes_info = list_palettes()
    >>> print(palettes_info)

    See Also
    --------
    get_palette : Obtener una paleta específica
    load_colors : Cargar todas las paletas
    """
    palettes = load_colors(colors_path)
    
    result = []
    for palette_name in palettes.keys():
        palette_data = palettes[palette_name]
        
        # Extraer número de colores
        if isinstance(palette_data, list):
            n_colors = len(palette_data)
            color_name = ""
        elif isinstance(palette_data, dict) and "colors" in palette_data:
            n_colors = len(palette_data["colors"])
            color_name = palette_data.get("color_name", "")
        else:
            n_colors = len(palette_data)
            color_name = ""
        
        result.append({
            "name": palette_name,
            "n_colors": n_colors,
            "color_name": color_name
        })
    
    # Intentar usar pandas si está disponible
    try:
        import pandas as pd
        return pd.DataFrame(result)
    except ImportError:
        return result

