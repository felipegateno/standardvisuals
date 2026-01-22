#' @title Cargar paletas desde colors.json
#' @description
#' Lee el archivo colors.json incluido en el paquete.
#'
#' @param colors_path Ruta al archivo colors.json (opcional).
#' @return Lista con paletas.
#' @importFrom jsonlite fromJSON
#' @export
load_colors <- function(colors_path = NULL) {
  if (is.null(colors_path)) {
    colors_path <- system.file("colors.json", package = "rstandarvisuals")
  }
  if (!file.exists(colors_path)) {
    stop("No se encontró colors.json en: ", colors_path)
  }
  jsonlite::fromJSON(colors_path, simplifyVector = FALSE)
}

#' @title Obtener una paleta por nombre
#' @param name Nombre de la paleta.
#' @param colors_path Ruta al archivo colors.json (opcional).
#' @param n Número de colores a retornar (máximo = tamaño de la paleta).
#' @return Vector de colores en hex.
#' @export
get_palette <- function(name, colors_path = NULL, n = NULL) {
  palettes <- load_colors(colors_path)
  if (!name %in% names(palettes)) {
    stop("Paleta no encontrada: ", name)
  }
  palette <- unlist(palettes[[name]], use.names = FALSE)
  if (is.null(n)) {
    return(palette)
  }
  if (!is.numeric(n) || length(n) != 1 || is.na(n)) {
    stop("n debe ser un entero válido.")
  }
  n <- as.integer(n)
  if (n <= 0) {
    stop("n debe ser mayor que 0.")
  }
  max_n <- length(palette)
  if (n > max_n) {
    stop("n=", n, " excede el máximo permitido (", max_n, ").")
  }
  if (n == 1) {
    return(palette[1])
  }
  if (n == max_n) {
    return(palette)
  }
  idx <- round(seq(1, max_n, length.out = n))
  palette[idx]
}

