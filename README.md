# Standard Visuals - Estándar mínimo de estilo

Este repositorio define un estándar **mínimo** de estilo para gráficos en R (ggplot2) y Python (matplotlib). El objetivo es aplicar el mismo look-and-feel desde un único archivo `style_tokens.json`.

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

`style_tokens.json` contiene solo:

- **Tipografía**: título, subtítulo, títulos de ejes, texto de ejes y títulos de panel.
- **Bordes**: marco de figura y panel.
- **Grillas**: mayor y menor.

Ejemplo simplificado:

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

## Instalación desde GitHub

### 1) Clonar el repositorio (opcional)

```bash
git clone https://github.com/felipegatneo/standardvisuals.git
cd standardvisuals
```

### 2) Dependencias

**R**:

```r
install.packages(c("jsonlite", "ggplot2"))
```

**Python**:

```bash
pip install matplotlib pandas
```

### 3) Fuente Inter

Las fuentes deben existir en `assets/fonts/Inter/`. Si no están, descárgalas desde:
https://github.com/rsms/inter/releases

Archivos esperados:
- `Inter-Regular.ttf`
- `Inter-SemiBold.ttf`

Si no están, se usará la fuente por defecto del sistema.

## Uso en R (ggplot2)

### Opción A: instalar el paquete desde GitHub

```r
install.packages("remotes")
remotes::install_github("felipegatneo/standardvisuals", subdir = "rstandarvisuals")
```

### Opción B: usar el script directamente desde el repo

```r
source("rstandarvisuals/R/create_theme_from_tokens.R")

p <- ggplot(data, aes(x, y)) +
  geom_line() +
  labs(title = "Título", subtitle = "Subtítulo", x = "Eje X", y = "Eje Y") +
  create_theme_from_tokens()

print(p)
```

## Uso en Python (matplotlib)

### Opción A: instalar el paquete desde GitHub

```bash
pip install git+https://github.com/felipegatneo/standardvisuals.git#subdirectory=pystandarvisuals
```

### Opción B: usar el código desde el repo

```python
import os
import sys
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.getcwd(), "pystandarvisuals", "src"))
from pystandarvisuals import create_theme_from_tokens

fig, ax = plt.subplots()
ax.plot(x, y)
ax.set_title("Título")
ax.set_xlabel("Eje X")
ax.set_ylabel("Eje Y")

create_theme_from_tokens(ax)
plt.show()
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

