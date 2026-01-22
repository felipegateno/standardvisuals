#' @title Crear theme desde style_tokens.json
#' @description
#' Lee un JSON de tokens de estilo y construye un theme de ggplot2
#' aplicable a cualquier gráfico ya creado.
#'
#' @param tokens_path Ruta al archivo JSON de tokens (opcional).
#' @return Objeto theme de ggplot2.
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

