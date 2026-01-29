#' @title Crear theme desde style_tokens.json
#' @description
#' Lee un JSON de tokens de estilo y construye un theme de ggplot2
#' aplicable a cualquier gráfico ya creado. Aplica estilos estandarizados
#' incluyendo tipografía (fuente Inter, tamaños), bordes y grillas.
#'
#' @param tokens_path Ruta al archivo JSON de tokens (opcional). Si es NULL,
#'   usa el archivo incluido en el paquete.
#' @return Objeto theme de ggplot2 que puede ser agregado a cualquier gráfico
#'   con el operador \code{+}.
#'
#' @details
#' Los tokens aplicados incluyen:
#' \itemize{
#'   \item Tipografía: fuente Inter, tamaños estándar para títulos, subtítulos,
#'     etiquetas de ejes y texto de ejes
#'   \item Bordes: color y grosor para figura y panel
#'   \item Grillas: color y grosor para grillas mayor y menor
#' }
#'
#' @examples
#' library(ggplot2)
#' library(rstandarvisuals)
#'
#' # Crear un gráfico básico
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   labs(title = "Mi gráfico", x = "Millas por galón", y = "Caballos de fuerza")
#'
#' # Aplicar theme estandarizado
#' p + create_theme_from_tokens()
#'
#' @seealso \code{\link{get_palette}} para obtener paletas de colores estandarizadas
#' @importFrom jsonlite fromJSON
#' @importFrom ggplot2 theme element_text element_line element_rect element_blank
#' @export
create_theme_from_tokens <- function(tokens_path = NULL) {
  if (is.null(tokens_path)) {
    tokens_path <- system.file("style_tokens.json", package = "rstandarvisuals")
  }
  if (!file.exists(tokens_path)) {
    stop("No se encontró el archivo de tokens en: ", tokens_path)
  }

  tokens <- jsonlite::fromJSON(tokens_path, simplifyVector = FALSE)
  typo <- tokens$typography
  borders <- tokens$borders
  grid <- tokens$grid

  font_dir <- system.file("assets", "fonts", "Inter", package = "rstandarvisuals")
  if (font_dir != "" && dir.exists(font_dir)) {
    regular_font <- file.path(font_dir, "Inter-Regular.ttf")
    semibold_font <- file.path(font_dir, "Inter-SemiBold.ttf")
    if (file.exists(regular_font) && file.exists(semibold_font)) {
      try({
        font_add_fn <- utils::getFromNamespace("font_add", "sysfonts")
        showtext_auto_fn <- utils::getFromNamespace("showtext_auto", "showtext")
        showtext_opts_fn <- utils::getFromNamespace("showtext_opts", "showtext")

        font_add_fn(family = "Inter", regular = regular_font, bold = semibold_font)
        showtext_auto_fn()
        showtext_opts_fn(dpi = 300)
      }, silent = TRUE)
    }
  }
  
  ggplot2::theme_bw()+
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = typo$title$family,
      size = typo$title$size,
      color = typo$title$color,
      hjust = 0.5
    ),
    plot.subtitle = ggplot2::element_text(
      family = typo$subtitle$family,
      size = typo$subtitle$size,
      color = typo$subtitle$color,
      hjust = 0.5
    ),
    axis.title = ggplot2::element_text(
      family = typo$axis_title$family,
      size = typo$axis_title$size,
      color = typo$axis_title$color
    ),
    axis.text = ggplot2::element_text(
      family = typo$axis_text$family,
      size = typo$axis_text$size,
      color = typo$axis_text$color
    ),
    strip.text = ggplot2::element_text(
      family = typo$panel_title$family,
      size = typo$panel_title$size,
      color = typo$panel_title$color
    ),
    panel.border = ggplot2::element_rect(
      color = borders$panel$color,
      linewidth = borders$panel$linewidth,
      fill = NA
    ),
    plot.background = ggplot2::element_rect(
      color = borders$figure$color,
      linewidth = borders$figure$linewidth,
      fill = "white"
    ),
    panel.grid.major = ggplot2::element_line(
      color = grid$major$color,
      linewidth = grid$major$linewidth
    ),
    panel.grid.minor = ggplot2::element_line(
      color = grid$minor$color,
      linewidth = grid$minor$linewidth
    ),
    strip.placement = "outside",
    strip.background = element_rect(fill = "white", colour = "white", linewidth = 0.4),
    strip.text.y = element_text(face = "bold", hjust = 1),
    panel.spacing.y = unit(0.6, "lines")
  )
}

