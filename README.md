# Standard Visuals - Estándar mínimo de estilo

Este repositorio tiene como objetivo homogenizar las gráficas generadas en R (ggplot2) y Python (matplotlib), estandarizando tipos y tamaño de letras, estilos de grilla, colors de fondo, etc. El repositorio cuenta también con funciones para poder extraer paletas de colores, con el objetivo de trabajar con exactamente los mismos tonos en las gráficas.

## Estructura del Proyecto

```
standardvisuals/
├── style_tokens.json                     # Tokens mínimos de estilo
├── assets/
│   └── fonts/
│       └── Inter/                        # Fuente Inter (ver assets/fonts/Inter/README.md)
│           ├── Inter-Regular.ttf
│           └── Inter-SemiBold.ttf
├── pystandarvisuals/                     # Paquete Python
│   ├── pyproject.toml
│   └── src/pystandarvisuals/
│       └── create_theme_from_tokens.py
├── rstandarvisuals/                      # Paquete R
│   ├── DESCRIPTION
│   ├── NAMESPACE
│   └── R/create_theme_from_tokens.R
└── examples/
    ├── test.py
    ├── test.R
    ├── ts_pr_daily.csv
    └── figures/
```

## Archivo de Tokens

`style_tokens.json` contiene la definición de atributos asociados a:

- **Tipografía**: título, subtítulo, títulos de ejes, texto de ejes y títulos de panel.
- **Bordes**: marco de figura y panel.
- **Grillas**: mayor y menor.

Un ejemplo simplificado de este archivo es:

```json
{
  "typography": {
    "title": { "family": "Inter", "size": 14, "color": "#000000" },
    "subtitle": { "family": "Inter", "size": 12, "color": "#000000" },
    "axis_title": { "family": "Inter", "size": 11, "color": "#000000" },
    "axis_text": { "family": "Inter", "size": 11, "color": "#000000" },
    "panel_title": { "family": "Inter", "size": 11, "color": "#000000" }
  },
  "borders": {
    "figure": { "color": "#333333", "linewidth": 0.9 },
    "panel": { "color": "#333333", "linewidth": 0.9 }
  },
  "grid": {
    "major": { "color": "#E5E5E5", "linewidth": 0.4 },
    "minor": { "color": "#E5E5E5", "linewidth": 0.4 }
  }
}
```

La fuente definida para los gráficos es Inter, cuya previsuzalización se presenta a continuación:

![Inter preview](assets/Inter_preview.png)


## Paletas de color (colors.json)

`colors.json` incluye paletas estándar para uso homogéneo en R y Python.

Paletas disponibles:
- `standarscolors_obsvssim` (2 colores)
- `standarscolors_divergent`(30 colores)
- `standarscolors_terrain` (11 colores)
- `standardscolors_rdylbu` (11 colores)
- `standardscolors_rdylgn` (11 colores)
- `standardscolors_rdbu` (11 colores)

Vista previa:

![Vista previa de paletas](assets/palette_preview.png)

## Instalación desde GitHub

### Dependencias

**R**:

```r
install.packages(c("jsonlite", "ggplot2"))
```

**Python**:

```bash
pip install matplotlib pandas
```


## Uso en R (ggplot2)

### Instalar el paquete desde GitHub y estandarizar gráficas

```r
install.packages("remotes")
remotes::install_github("felipegateno/standardvisuals", subdir = "rstandarvisuals")

p <- ggplot(data, aes(x, y)) +
  geom_line() +
  labs(title = "Título", subtitle = "Subtítulo", x = "Eje X", y = "Eje Y") +
  create_theme_from_tokens()

print(p)
```

### Paletas de colores

```r
library(rstandarvisuals)

pal <- get_palette("standarscolors_obsvssim")
# pal es un vector de colores hex
```

Para obtener solo `n` colores (máximo = tamaño de la paleta):

```r
pal <- get_palette("standarscolors_divergent", n = 7)
```

## Uso en Python (matplotlib)

### Instalar el paquete desde GitHub y estandarizar gráficas

```bash
pip install git+https://github.com/felipegateno/standardvisuals.git#subdirectory=pystandarvisuals
```

```python
import os
import sys
import matplotlib.pyplot as plt
from pystandarvisuals import create_theme_from_tokens, get_palette

fig, ax = plt.subplots()
ax.plot(x, y)
ax.set_title("Título")
ax.set_xlabel("Eje X")
ax.set_ylabel("Eje Y")

create_theme_from_tokens(ax)
plt.show()
```

### Paletas de colores

```python
from pystandarvisuals import get_palette

pal = get_palette("standarscolors_obsvssim")
# pal es una lista de colores hex
```

Para obtener solo `n` colores (máximo = tamaño de la paleta):

```python
pal = get_palette("standarscolors_divergent", n=7)
```

Si usas `subplots=True` en pandas:

```python
axes = df.plot(subplots=True, grid=True)
for ax in axes:
    create_theme_from_tokens(ax)
```

## Scripts de prueba

- R: `Rscript examples/test.R`
- Python: `python examples/test.py`

Ambos scripts usan `examples/ts_pr_daily.csv` y guardan un PNG en `examples/figures/`.

