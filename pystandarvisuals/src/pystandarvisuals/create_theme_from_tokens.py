"""
Aplicar tokens de estilo a un eje ya creado (matplotlib).
"""

import json
import os
from pathlib import Path
import importlib.resources as resources

import matplotlib as mpl
from matplotlib import font_manager as fm


def _resolve_tokens_path(tokens_path):
    candidates = []
    if tokens_path:
        candidates.append(tokens_path)
        candidates.append(os.path.join(os.getcwd(), tokens_path))
        for parent in Path(__file__).resolve().parents:
            candidates.append(str(parent / tokens_path))
    for parent in Path(__file__).resolve().parents:
        candidates.append(str(parent / "style_tokens.json"))

    for path in candidates:
        if os.path.exists(path):
            return path

    try:
        return str(resources.files("pystandarvisuals").joinpath("data/style_tokens.json"))
    except Exception:
        raise FileNotFoundError(
            "No se encontró el archivo de tokens. Buscado en: " + ", ".join(candidates)
        )


def _load_tokens(tokens_path):
    resolved = _resolve_tokens_path(tokens_path)
    with open(resolved, "r", encoding="utf-8") as f:
        return json.load(f), resolved


def _register_font_from_repo(font_family, tokens_path):
    """
    Registrar la fuente desde assets/fonts/Inter si existe.
    Retorna el nombre de la fuente disponible en matplotlib.
    """
    possible_dirs = []
    if tokens_path:
        possible_dirs.append(
            os.path.join(os.path.dirname(tokens_path), "assets", "fonts", "Inter")
        )
    possible_dirs.append(os.path.join(os.getcwd(), "assets", "fonts", "Inter"))
    for parent in Path(__file__).resolve().parents:
        possible_dirs.append(str(parent / "assets" / "fonts" / "Inter"))
    try:
        possible_dirs.append(
            str(resources.files("pystandarvisuals").joinpath("assets/fonts/Inter"))
        )
    except Exception:
        pass
    font_dir = next((d for d in possible_dirs if os.path.exists(d)), None)
    if not font_dir:
        return font_family

    regular_candidates = [
        "Inter-Regular.ttf",
        "Inter_18pt-Regular.ttf",
        "Inter_24pt-Regular.ttf",
        "Inter_28pt-Regular.ttf",
    ]
    regular_font = next(
        (os.path.join(font_dir, f) for f in regular_candidates if os.path.exists(os.path.join(font_dir, f))),
        None,
    )
    if not regular_font:
        return font_family

    try:
        fm.fontManager.addfont(regular_font)
        font_name = fm.FontProperties(fname=regular_font).get_name()
        mpl.rcParams["font.family"] = font_name
        return font_name
    except Exception:
        return font_family


def create_theme_from_tokens(ax, tokens_path="style_tokens.json", tokens=None):
    """
    Aplica los tokens de estilo estandarizados a un eje de matplotlib ya creado.
    
    Esta función aplica los estilos definidos en style_tokens.json a un gráfico
    existente, incluyendo tipografía (fuente Inter, tamaños), bordes, y grillas.
    
    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Eje a modificar. Debe ser un objeto Axes de matplotlib.
    tokens_path : str, optional
        Ruta al archivo JSON de tokens. Si no se especifica, busca el archivo
        incluido en el paquete.
    tokens : dict, optional
        Diccionario con tokens ya cargados. Si se proporciona, se usa en lugar
        de cargar desde tokens_path.
    
    Returns
    -------
    matplotlib.axes.Axes
        El mismo eje con estilos aplicados.
    
    Examples
    --------
    >>> import matplotlib.pyplot as plt
    >>> from pystandarvisuals import create_theme_from_tokens
    >>> 
    >>> fig, ax = plt.subplots()
    >>> ax.plot([1, 2, 3], [1, 4, 9])
    >>> ax.set_title("Mi gráfico")
    >>> ax.set_xlabel("Eje X")
    >>> ax.set_ylabel("Eje Y")
    >>> create_theme_from_tokens(ax)
    >>> plt.show()
    
    Notes
    -----
    Los tokens aplicados incluyen:
    - Tipografía: fuente Inter, tamaños estándar para títulos, subtítulos,
      etiquetas de ejes y texto de ejes
    - Bordes: color y grosor para figura y panel
    - Grillas: color y grosor para grillas mayor y menor
    
    See Also
    --------
    get_palette : Obtener paletas de colores estandarizadas
    """
    if tokens is None:
        tokens, tokens_path = _load_tokens(tokens_path)

    typo = tokens["typography"]
    borders = tokens["borders"]
    grid = tokens["grid"]

    fig = ax.figure
    font_family = _register_font_from_repo(typo["axis_text"]["family"], tokens_path)

    # Tipografía de títulos
    if fig._suptitle is not None:
        fig._suptitle.set_fontfamily(font_family)
        fig._suptitle.set_fontsize(typo["title"]["size"])
        fig._suptitle.set_color(typo["title"]["color"])
        fig._suptitle.set_fontweight("normal")
        fig._suptitle.set_ha("center")
    else:
        ax.title.set_fontfamily(font_family)
        ax.title.set_fontsize(typo["title"]["size"])
        ax.title.set_color(typo["title"]["color"])
        ax.title.set_fontweight("normal")
        ax.title.set_ha("center")

    ax.set_xlabel(
        ax.get_xlabel(),
        fontfamily=font_family,
        fontsize=typo["axis_title"]["size"],
        color=typo["axis_title"]["color"],
        fontweight="normal",
    )
    ax.set_ylabel(
        ax.get_ylabel(),
        fontfamily=font_family,
        fontsize=typo["axis_title"]["size"],
        color=typo["axis_title"]["color"],
        fontweight="normal",
    )

    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontfamily(font_family)
        label.set_fontsize(typo["axis_text"]["size"])
        label.set_color(typo["axis_text"]["color"])

    # Título de panel (si hay suptitle, aplica a títulos de ejes)
    if fig._suptitle is not None:
        ax.title.set_fontfamily(font_family)
        ax.title.set_fontsize(typo["panel_title"]["size"])
        ax.title.set_color(typo["panel_title"]["color"])
        ax.title.set_fontweight("normal")
        ax.title.set_ha("center")

    # Bordes (panel)
    for spine in ax.spines.values():
        spine.set_color(borders["panel"]["color"])
        spine.set_linewidth(borders["panel"]["linewidth"])

    # Borde de figura
    fig.patch.set_edgecolor(borders["figure"]["color"])
    fig.patch.set_linewidth(borders["figure"]["linewidth"])

    # Grillas
    ax.grid(
        True,
        which="major",
        color=grid["major"]["color"],
        linewidth=grid["major"]["linewidth"],
    )
    ax.grid(
        False,
        which="minor",
        color=grid["minor"]["color"],
        linewidth=grid["minor"]["linewidth"],
    )

    # Forzar actualización de fuente en ejes
    mpl.rcParams["font.family"] = font_family

    return ax

