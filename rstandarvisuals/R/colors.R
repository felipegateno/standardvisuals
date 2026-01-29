#' @title Cargar paletas desde colors.json
#' @description
#' Lee el archivo colors.json incluido en el paquete que contiene todas las
#' paletas de colores estandarizadas.
#'
#' @param colors_path Ruta al archivo colors.json (opcional). Si es NULL,
#'   usa el archivo incluido en el paquete.
#' @return Lista con todas las paletas disponibles. Cada paleta contiene
#'   un array de colores con sus valores hex y nombres.
#'
#' @examples
#' # Cargar todas las paletas
#' palettes <- load_colors()
#' names(palettes)  # Ver nombres de todas las paletas disponibles
#'
#' @seealso \code{\link{get_palette}} para obtener una paleta específica,
#'   \code{\link{list_palettes}} para ver un resumen de paletas disponibles.
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


construct_palette_entries <- function(colors_list) {
  entries <- vector("list", length(colors_list))
  for (idx in seq_along(colors_list)) {
    item <- colors_list[[idx]]
    if (is.list(item) && "hex" %in% names(item)) {
      hex <- item[["hex"]]
      name <- if (!is.null(item[["name"]]) && item[["name"]] != "") {
        item[["name"]]
      } else {
        paste0("color_", idx - 1)
      }
    } else {
      hex <- item
      name <- paste0("color_", idx - 1)
    }
    entries[[idx]] <- list(hex = hex, name = name)
  }
  entries
}


select_entries <- function(entries, n) {
  if (is.null(n)) {
    return(entries)
  }
  if (!is.numeric(n) || length(n) != 1 || is.na(n)) {
    stop("n debe ser un entero válido.")
  }
  n <- as.integer(n)
  if (n <= 0) {
    stop("n debe ser mayor que 0.")
  }
  max_n <- length(entries)
  if (n > max_n) {
    stop("n=", n, " excede el máximo permitido (", max_n, ").")
  }
  if (n == 1) {
    return(entries[1])
  }
  if (n == max_n) {
    return(entries)
  }
  idx <- round(seq(1, max_n, length.out = n))
  entries[idx]
}

#' @title Obtener una paleta por nombre
#' @description
#' Extrae una paleta de colores específica por su nombre. Permite obtener
#' todos los colores de la paleta o un subconjunto de ellos.
#'
#' @param name Nombre de la paleta. Debe ser uno de los nombres disponibles.
#'   Use \code{\link{list_palettes}()} para ver todas las paletas disponibles.
#' @param colors_path Ruta al archivo colors.json (opcional). Si es NULL,
#'   usa el archivo incluido en el paquete.
#' @param n Número de colores a retornar (máximo = tamaño de la paleta).
#'   Si es NULL, retorna todos los colores de la paleta. Si es menor al
#'   tamaño total, selecciona colores distribuidos uniformemente.
#' @param dict_output Logical. Si es TRUE (valor por defecto) retorna un vector
#'   nombrado con los hexadecimales. Si es FALSE, retorna una lista de listas
#'   con los campos \code{hex} y \code{name}.
#' @return Vector de colores en formato hexadecimal (ej: "#FF0000").
#'
#' @details
#' Paletas disponibles:
#' \itemize{
#'   \item \code{standardcolors_obsvssim}: 2 colores (negro y azul) - Para comparaciones observado vs simulado
#'   \item \code{standardcolors_divergent}: 30 colores - Paleta multicolor diversa
#'   \item \code{standardcolors_terrain}: 11 colores - Paleta tipo terreno (marrón a verde azulado)
#'   \item \code{standardcolors_rdylbu}: 11 colores - Paleta Spectral (Rojo-Amarillo-Azul divergente)
#'   \item \code{standardcolors_rdylgn}: 11 colores - Rojo-Amarillo-Verde (divergente)
#'   \item \code{standardcolors_rdbu}: 11 colores - Paleta coolwarm (Rojo-Azul divergente)
#'   \item \code{standardcolors_rdgn}: 11 colores - Rojo-Verde (sin pasar por amarillo)
#'   \item \code{standardcolors_magma}: 11 colores - Paleta magma (negro a amarillo, pasando por púrpura y rojo)
#'   \item \code{standardcolors_viridis}: 11 colores - Paleta viridis (púrpura a amarillo, pasando por verde)
#' }
#'
#' @examples
#' # Obtener todos los colores de una paleta
#' pal <- get_palette("standardcolors_obsvssim")
#' 
#' # Obtener solo 4 colores de una paleta (distribuidos uniformemente)
#' pal <- get_palette("standardcolors_viridis", n = 4)
#' 
#' # Usar en ggplot2
#' library(ggplot2)
#' colors <- get_palette("standardcolors_rdylbu", n = 5)
#' ggplot(data, aes(x, y, color = group)) +
#'   geom_line() +
#'   scale_color_manual(values = colors)
#'
#' @seealso \code{\link{load_colors}} para cargar todas las paletas,
#'   \code{\link{list_palettes}} para ver un resumen de paletas disponibles.
#' @export
get_palette <- function(name, colors_path = NULL, n = NULL, dict_output = TRUE) {
  palettes <- load_colors(colors_path)
  if (!name %in% names(palettes)) {
    stop("Paleta no encontrada: ", name)
  }
  palette_data <- palettes[[name]]
  if (is.list(palette_data) && "colors" %in% names(palette_data)) {
    colors_list <- palette_data[["colors"]]
  } else {
    colors_list <- palette_data
  }

  palette_entries <- construct_palette_entries(colors_list)
  selected <- select_entries(palette_entries, n)

  if (!dict_output) {
    return(selected)
  }

  hex_values <- sapply(selected, function(x) x[["hex"]])
  names(hex_values) <- sapply(selected, function(x) x[["name"]])
  hex_values
}


list_palettes <- function(colors_path = NULL) {
  palettes <- load_colors(colors_path)
  
  result <- data.frame(
    name = character(),
    n_colors = integer(),
    color_name = character(),
    stringsAsFactors = FALSE
  )
  
  for (palette_name in names(palettes)) {
    palette_data <- palettes[[palette_name]]
    
    # Extraer número de colores
    if (is.list(palette_data) && "colors" %in% names(palette_data)) {
      colors_list <- palette_data[["colors"]]
      n_colors <- length(colors_list)
      color_name <- if ("color_name" %in% names(palette_data)) {
        palette_data[["color_name"]]
      } else {
        ""
      }
    } else {
      n_colors <- length(palette_data)
      color_name <- ""
    }
    
    result <- rbind(result, data.frame(
      name = palette_name,
      n_colors = n_colors,
      color_name = color_name,
      stringsAsFactors = FALSE
    ))
  }
  
  return(result)
}

