# Fuentes Inter

Este directorio debe contener los archivos de fuente Inter para garantizar consistencia visual entre R y Python.

## Archivos requeridos

- `Inter-Regular.ttf` o `Inter_18pt-Regular.ttf` - Fuente regular
- `Inter-SemiBold.ttf` o `Inter_18pt-SemiBold.ttf` - Fuente en negrita (semi-bold)

**Nota**: El código busca ambos formatos de nombres de archivo para compatibilidad.

## Cómo obtener las fuentes

Las fuentes Inter están disponibles bajo licencia SIL Open Font License 1.1:

1. **Descarga oficial**: https://github.com/rsms/inter/releases
2. **Google Fonts**: https://fonts.google.com/specimen/Inter

### Pasos para instalar:

1. Descarga el archivo ZIP de Inter desde GitHub o Google Fonts
2. Extrae los archivos
3. Copia los siguientes archivos a este directorio (desde la carpeta `static/` del ZIP):
   - `Inter_18pt-Regular.ttf` → `assets/fonts/Inter/Inter-Regular.ttf` (o renómbralo)
   - `Inter_18pt-SemiBold.ttf` → `assets/fonts/Inter/Inter-SemiBold.ttf` (o renómbralo)
   
   **Alternativa**: Puedes copiar los archivos con sus nombres originales y el código los encontrará automáticamente.

## Nota

Si las fuentes no están disponibles, el código usará la fuente por defecto del sistema, pero se mostrará una advertencia.

