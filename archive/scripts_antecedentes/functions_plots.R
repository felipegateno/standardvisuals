#' @importFrom stringr str_detect
# calc_ts_metrics ----
#' @name calc_ts_metrics
#' @title Calculate performance metrics for two time series
#' @description
#' Internal function to calculate mean, standard deviation, PBIAS, r², and KGE metrics for two time series.
#'
#' @param df1 A data.frame with two columns: Date and value for series 1.
#' @param df2 A data.frame with two columns: Date and value for series 2.
#' @return A data.frame with columns: mean1, sd1, mean2, sd2, pbias, r2, kge
#' @keywords internal
calc_ts_metrics = function(df1, df2) {
  promedio_1 = round(mean(df1[[2]], na.rm = TRUE), 2)
  desvest_1 = round(sd(df1[[2]], na.rm = TRUE), 2)
  promedio_2 = round(mean(df2[[2]], na.rm = TRUE), 2)
  desvest_2 = round(sd(df2[[2]], na.rm = TRUE), 2)
  pbias_val = round(hydroGOF::pbias(sim = df2[[2]], obs = df1[[2]]), 2)
  corr_val = round(hydroGOF::rPearson(sim = df2[[2]], obs = df1[[2]])^2, 2)
  kge_val = round(hydroGOF::KGE(sim = df2[[2]], obs = df1[[2]]), 2)
  data.frame(mean1 = promedio_1,
             sd1 = desvest_1,
             mean2 = promedio_2,
             sd2 = desvest_2,
             pbias = pbias_val,
             r2 = corr_val,
             kge = kge_val)
}

# plot_double_ts ----
#' @name plot_double_ts
#' @title Generates a graphical comparison of two daily, monthly, or annual series.
#' @description
#' Generates a graphical comparison of two daily, monthly, or annual series.
#'
#' @param df1 A data.frame corresponding to series 1. Must have exactly 2 columns: Date (first) and value (second).
#' @param ID1 A string representing the name of series 1 (used for labels and legend).
#' @param df2 A data.frame corresponding to series 2. Must have exactly 2 columns: Date (first) and value (second).
#' @param ID2 A string representing the name of series 2 (used for labels and legend).
#' @param label_y A string representing the y-axis title.
#' @param timestep A string indicating the time aggregation of the data frames. Accepts 'day'/'daily'/'d', 'month'/'monthly'/'m', or 'year'/'yearly'/'y'.
#' @param performance_metrics A logical value. If FALSE, only mean and standard deviation comparisons are added. If TRUE, PBIAS, KGE, and r2 are also added.
#' @param show_legend Logical. If TRUE, displays the legend. Default is FALSE.
#' @param aspect_ratio Numeric. Aspect ratio for the plot. Default is 1/7.
#' @param show_points Logical. If TRUE, adds points to the lines. Default is FALSE.
#' @param show_coverage Logical. If TRUE, adds background shading indicating data availability periods. Default is FALSE.
#' @return A ggplot graph.
#' @examples
#' # example code
#' # read observed data
#' data("data_daily_timeseries_obs")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' # read simulated data (example dataset shipped in the package)
#' data("data_daily_timeseries_cr2metv25")
#' df_sim = data_daily_timeseries_cr2metv25$CR2METv2.5_QuintaNormal[, c("dates", "pr_mmd")]
#' # plot
#' output = plot_double_ts(df1 = df_obs,
#'                      ID1 = 'obs',
#'                      df2 = df_sim,
#'                      ID2 = 'sim',
#'                      label_y = 'precipitation',
#'                      timestep = 'day',
#'                      performance_metrics = TRUE)
#' print(output)
#' @export
plot_double_ts = function(df1,
                       ID1,
                       df2,
                       ID2,
                       label_y,
                       timestep,
                       performance_metrics,
                       show_legend = FALSE,
                       aspect_ratio = 1/7,
                       show_points = FALSE,
                       show_coverage = FALSE) {
  # Validar estructura de los dataframes de entrada
  stopifnot(ncol(df1) == 2, ncol(df2) == 2)
  stopifnot(inherits(df1[[1]], "Date"), inherits(df2[[1]], "Date"))
  
  # Convertir columna de fechas a formato Date para asegurar consistencia
  df1[[1]] = as.Date(df1[[1]])
  df2[[1]] = as.Date(df2[[1]])
  
  # Eliminar períodos sin datos al inicio y final de cada serie
  # Esto evita mostrar ventanas vacías en el gráfico
  # Para serie 1
  df1_valid = df1[!is.na(df1[[2]]), ]
  if (nrow(df1_valid) > 0) {
    min_date_df1 = min(df1_valid[[1]], na.rm = TRUE)
    max_date_df1 = max(df1_valid[[1]], na.rm = TRUE)
    df1 = df1[df1[[1]] >= min_date_df1 & df1[[1]] <= max_date_df1, ]
  }
  
  # Para serie 2
  df2_valid = df2[!is.na(df2[[2]]), ]
  if (nrow(df2_valid) > 0) {
    min_date_df2 = min(df2_valid[[1]], na.rm = TRUE)
    max_date_df2 = max(df2_valid[[1]], na.rm = TRUE)
    df2 = df2[df2[[1]] >= min_date_df2 & df2[[1]] <= max_date_df2, ]
  }
  
  # Normalizar y validar el argumento timestep
  # Acepta variantes como "daily"/"d", "monthly"/"m", "yearly"/"y"
  timestep = tolower(timestep)
  if (timestep %in% c("daily", "d")) {
    timestep = "day"
  }
  if (timestep %in% c("monthly", "m")) {
    timestep = "month"
  }
  if (timestep %in% c("yearly", "y")) {
    timestep = "year"
  }
  timestep = match.arg(timestep, choices = c("day", "month", "year"))
  
  # Definir título descriptivo según la resolución temporal
  if (timestep == "day") {
    titulo = "serie diaria"
  }
  if (timestep == "month") {
    titulo = "serie mensual"
  }
  if (timestep == "year") {
    titulo = "serie anual"
  }
  
  # Estandarizar nombres de columnas internamente para procesamiento
  colnames(df1) = c("Fecha", "serie1")
  colnames(df2) = c("Fecha", "serie2")
  
  # Convertir a data.table para operaciones eficientes
  dt1 = data.table::as.data.table(df1)
  dt2 = data.table::as.data.table(df2)
  
  # Realizar join interno para obtener solo fechas comunes con datos válidos
  # Este dataset se usa para calcular las métricas de desempeño
  dt_stats = dt1[dt2, on = "Fecha", nomatch = 0]
  # Filtrar filas donde ambas series tienen valores no-NA
  dt_stats = dt_stats[!is.na(serie1) & !is.na(serie2)]
  
  # Validar que existan fechas en común; si no, detener ejecución
  if (nrow(dt_stats) == 0) {
    stop("No hay fechas en común entre df1 y df2; no es posible calcular métricas.")
  }
  
  # Separar las series para cálculo de métricas
  df1_stats = data.table::setDF(dt_stats[, c(1, 2)])
  df2_stats = data.table::setDF(dt_stats[, c(1, 3)])
  
  # Crear secuencia completa de fechas para el período de visualización
  # Incluye todas las fechas entre el mínimo y máximo donde exista información en cualquiera de las series
  full_min = min(c(df1[[1]], df2[[1]]), na.rm = TRUE)
  full_max = max(c(df1[[1]], df2[[1]]), na.rm = TRUE)
  df_Fecha = data.frame(Fecha = seq.Date(from = full_min,
                                         to = full_max,
                                         by = timestep))
  dt_Fecha = data.table::as.data.table(df_Fecha)
  
  # Hacer join izquierdo para incluir todas las fechas en el rango
  # Esto permite mostrar períodos con NA explícitamente en el gráfico
  dt_full = merge(dt_Fecha, dt1, by = "Fecha", all.x = TRUE, sort = FALSE)
  dt_full = merge(dt_full, dt2, by = "Fecha", all.x = TRUE, sort = FALSE)
  dt_full = data.table::as.data.table(dt_full)
  
  # Seleccionar solo las columnas necesarias para el gráfico
  dt_full = dt_full[, .(Fecha, serie1, serie2)]
  
  # Crear copia con nombres de columnas usando IDs del usuario
  # Esto es necesario para el cálculo del estado de cobertura
  dt_plot = data.table::copy(dt_full)
  data.table::setnames(dt_plot, c("Fecha", ID1, ID2))
  
  # Determinar rango de fechas válidas para los límites del eje x
  # Considera fechas donde al menos una serie tiene datos
  dt_valid = dt_full[!(is.na(serie1) & is.na(serie2))]
  min_date = min(dt_valid$Fecha, na.rm = TRUE)
  max_date = max(dt_valid$Fecha, na.rm = TRUE)
  
  # Transformar datos a formato largo (melt) para facilitar el gráfico
  # na.rm = FALSE preserva los NA para que ggplot no dibuje líneas conectando períodos vacíos
  df_plot = data.table::melt(dt_full, id.vars = 'Fecha', variable.name = 'variable', value.name = 'value', na.rm = FALSE)
  df_plot = data.table::setDF(df_plot)
  
  # Reemplazar nombres internos (serie1, serie2) con IDs del usuario para la leyenda
  df_plot$variable = ifelse(df_plot$variable == "serie1", ID1, ID2)
  df_plot$variable = factor(df_plot$variable, levels = c(ID1, ID2))
  
  # Preparar datos para visualización de cobertura de datos (show_coverage)
  if (show_coverage) {
    # Crear tabla de estados de disponibilidad de datos por fecha
    # Estados: "none" (ninguna serie), "no_blue" (falta serie 1), "no_red" (falta serie 2), "both" (ambas presentes)
    dt_status = data.table::copy(dt_plot)
    dt_status[, status := data.table::fifelse(
      is.na(get(ID1)) & is.na(get(ID2)), "none",
      data.table::fifelse(
        is.na(get(ID1)) & !is.na(get(ID2)), "no_blue",
        data.table::fifelse(
          !is.na(get(ID1)) & is.na(get(ID2)), "no_red",
          "both"
        )
      )
    )]
    
    # Agrupar fechas consecutivas con el mismo estado para crear rectángulos de fondo
    dt_bg = dt_status[order(Fecha)][
      , .(xmin = data.table::first(Fecha),
          xmax = data.table::last(Fecha)),
      by = .(grp = data.table::rleid(status), status)
    ]
    # Extender el borde derecho del rectángulo un día para mejor visualización
    # Limitar al max_date para evitar warnings de ggplot2
    dt_bg[, xmax := pmin(xmax + 1L, as.Date(max_date))]
    # Filtrar rectángulos que intersectan con el rango válido del gráfico
    dt_bg = dt_bg[xmin <= max_date & xmax >= min_date]
    dt_bg = data.table::setDF(dt_bg)
  }
  
  # Calcular métricas de desempeño y generar subtítulo del gráfico
  if (performance_metrics) {
    # Calcular métricas completas (media, desv. estándar, PBIAS, r², KGE)
    metrics = calc_ts_metrics(df1_stats, df2_stats)
    promedio_1 = metrics$mean1
    desvest_1 = metrics$sd1
    promedio_2 = metrics$mean2
    desvest_2 = metrics$sd2
    pbias_str = toString(metrics$pbias)
    corr_str = toString(metrics$r2)
    kge_str = toString(metrics$kge)
    subtitulo = bquote(bold(.(ID1)) ~ bold("v/s") ~ bold(.(ID2)) ~ bold(.(titulo)) ~
                         bar(x) == .(promedio_1) ~ "v/s" ~ .(promedio_2) ~
                         "," ~
                         S[x] == .(desvest_1) ~ "v/s" ~ .(desvest_2) ~
                         ", PBIAS" == .(pbias_str) ~ "," ~
                         r^2 == .(corr_str) ~
                         ", KGE" == .(kge_str))
  } else {
    # Calcular solo estadísticas básicas (media y desviación estándar)
    promedio_1 = round(mean(df1_stats[[2]], na.rm = TRUE), 2)
    desvest_1 = round(sd(df1_stats[[2]], na.rm = TRUE), 2)
    promedio_2 = round(mean(df2_stats[[2]], na.rm = TRUE), 2)
    desvest_2 = round(sd(df2_stats[[2]], na.rm = TRUE), 2)
    subtitulo = bquote(bold(.(ID1)) ~ bold("v/s") ~ bold(.(ID2)) ~ bold(.(titulo)) ~
                         bar(x) == .(promedio_1) ~ "v/s" ~ .(promedio_2) ~
                         "," ~
                         S[x] == .(desvest_1) ~ "v/s" ~ .(desvest_2))
  }
  
  # Generar breaks automáticos para el eje x basados en el rango de fechas válidas
  x_breaks = scales::pretty_breaks(n = 8)(c(min_date, max_date))
  
  # Construir el gráfico
  if (show_coverage) {
    # Filtrar rectángulos de fondo para mostrar solo períodos con datos faltantes
    # No se muestran rectángulos cuando ambas series tienen datos ("both")
    dt_bg_plot = dt_bg[dt_bg$status != "both", ]
    
    # Inicializar gráfico con capas de fondo y líneas
    fig = ggplot() +
      # Agregar rectángulos de fondo que indican períodos sin datos
      geom_rect(
        data = dt_bg_plot,
        aes(xmin = xmin, xmax = xmax,
            ymin = -Inf, ymax = Inf,
            fill = status),
        inherit.aes = FALSE,
        alpha = 0.2
      ) +
      # Dibujar líneas de las series temporales
      geom_line(
        data = df_plot,
        aes(x = Fecha, y = value, color = variable, linetype = variable),
        linewidth = 0.5,
        na.rm = TRUE
      )
    
    # Agregar puntos sobre las líneas si el usuario lo solicita
    if (show_points) {
      fig = fig + geom_point(
        data = df_plot,
        aes(x = Fecha, y = value, color = variable),
        inherit.aes = FALSE,
        na.rm = TRUE
      )
    }
    
    fig = fig +
      scale_color_manual(values = c("blue","red"),
                         breaks = c(ID1, ID2),
                         name = if(show_legend) "Serie" else NULL)+
      scale_linetype_manual(values = c("solid", "twodash"),
                            breaks = c(ID1, ID2),
                            name = if(show_legend) "Serie" else NULL)+
      scale_fill_manual(
        values = c( "black","blue","red"),
        breaks = c("none", "no_blue", "no_red")
      )+
      guides(fill = "none")+
      scale_x_date(limits = c(min_date, max_date),
                   breaks = x_breaks,
                   date_labels = "%m/%y")+
      labs(x = "Fecha",
           y = label_y,
           title = subtitulo)+
      theme_bw()+
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA))+
      theme(legend.position = ifelse(show_legend, "right", "none"),
            legend.background = element_rect(fill = NA),
            aspect.ratio = aspect_ratio)
  } else {
    # Construir gráfico sin achurado de fondo (comportamiento por defecto)
    fig = ggplot(df_plot,
                 aes(x = Fecha,
                     y = value,
                     color = variable,
                     linetype = variable))+
      geom_line(linewidth = 0.5)
    
    # Agregar puntos sobre las líneas si el usuario lo solicita
    if (show_points) {
      fig = fig + geom_point(na.rm = TRUE)
    }
    
    fig = fig +
      scale_color_manual(values = c("blue","red"),
                         breaks = c(ID1, ID2),
                         name = if(show_legend) "Serie" else NULL)+
      scale_linetype_manual(values = c("solid", "twodash"),
                            breaks = c(ID1, ID2),
                            name = if(show_legend) "Serie" else NULL)+
      scale_x_date(limits = c(min_date, max_date),
                   breaks = x_breaks,
                   date_labels = "%m/%y")+
      labs(x = "Fecha",
           y = label_y,
           title = subtitulo)+
      theme_bw()+
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA))+
      theme(legend.position = ifelse(show_legend, "right", "none"),
            legend.background = element_rect(fill = NA),
            aspect.ratio = aspect_ratio)
  }
  
  # return
  return(fig)
}
# ----

# plot_double_durationcurves ----
#' @name plot_double_durationcurves
#' @title Generates a graphical comparison of two duration curves.
#' @description
#' Generates a graphical comparison of two duration curves. Wrapping of calc_durationcurve function.
#'
#' @param df1 A data.frame corresponding to series 1.
#' @param ID1 A string representing the name of series 1.
#' @param df2 A data.frame corresponding to series 2.
#' @param ID2 A string representing the name of series 2.
#' @param is_masic Logical. If `TRUE`, calculates the probability of non-zero values and filters values greater than zero before calculating the curve. If `FALSE`, includes all values (including zeros).
#' @param label_y A string representing the y-axis title.
#' @param timestep A string indicating the time aggregation of the data frames, 'day', 'month', or 'year'.
#' @param logscale A logical value indicating whether to plot on a logarithmic scale or not.
#' @param aspect_ratio Numeric. Aspect ratio for the plot. Default is 1/7.
#' @param show_legend Logical. If TRUE, displays the legend. Default is FALSE.
#' @return A ggplot graph.
#' @examples
#' # example code
#' # read observed data
#' data("data_daily_timeseries_obs")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' # read simulated data (example dataset shipped in the package)
#' data("data_daily_timeseries_cr2metv25")
#' df_sim = data_daily_timeseries_cr2metv25$CR2METv2.5_QuintaNormal[, c("dates", "pr_mmd")]
#' # plot
#' output = plot_double_durationcurves(df1 = df_obs,
#'                                  ID1 = 'obs',
#'                                  df2 = df_sim,
#'                                  ID2 = 'sim',
#'                                  is_masic = TRUE,
#'                                  label_y = 'precipitation',
#'                                  timestep = 'day',
#'                                  logscale = FALSE)
#' print(output)
#' @export
plot_double_durationcurves = function(df1,
                                   ID1,
                                   df2,
                                   ID2,
                                   is_masic,
                                   label_y,
                                   timestep,
                                   logscale = FALSE,
                                   aspect_ratio = 1/7,
                                   show_legend = FALSE){
  # Normalizar y validar el argumento timestep
  # Acepta variantes como "daily", "monthly", "yearly"
  if (timestep == "daily") {
    timestep = "day"
  }
  if (timestep == "monthly") {
    timestep = "month"
  }
  if (timestep == "yearly") {
    timestep = "year"
  }
  timestep = match.arg(timestep, choices = c("day", "month", "year"))
  # Definir título descriptivo según la resolución temporal
  if (timestep == "day") {
    titulo = "Curva de duración de registros diarios"
  }
  if (timestep == "month") {
    titulo = "Curva de duración de registros mensuales"
  }
  if (timestep == "year") {
    titulo = "Curva de duración de registros anuales"
  }
  # Estandarizar nombres de columnas y convertir a data.table
  colnames(df1) = c("Fecha", ID1)
  colnames(df2) = c("Fecha", ID2)
  dt1 = data.table::as.data.table(df1)
  dt2 = data.table::as.data.table(df2)
  # Realizar join interno para obtener solo fechas comunes
  dt_plot = dt1[dt2, on = "Fecha", nomatch = 0]
  df_plot = data.table::setDF(dt_plot)
  # Separar las series para cálculo de curvas de duración
  df1 = df_plot[,c(1,2)]
  df2 = df_plot[,c(1,3)]
  df1_cd_raw = calc_durationcurve(data = df1,
                                  is_masic = is_masic)
  df1_prob = attr(df1_cd_raw, "prob_nozero")
  if (all(c("Pexc", "Valor") %in% names(df1_cd_raw))) {
    df1_cd = df1_cd_raw
    colnames(df1_cd)[1:2] = c("Pexc", "Serie")
    df1_cd$ID = ID1
    if (is_masic) {
      Pnozero1 = round(100 * df1_prob,1)
    }
  } else {
    df1_cd = df1_cd_raw[df1_cd_raw$variable == ID1, c("Pexc", "value"), drop = FALSE]
    colnames(df1_cd) = c("Pexc", "Serie")
    df1_cd$ID = ID1
    if (is_masic && length(df1_prob)) {
      Pnozero1 = 100 * round(as.numeric(df1_prob[ID1]), 2)
    }
  }
  
  df2_cd_raw = calc_durationcurve(data = df2,
                                  is_masic = is_masic)
  df2_prob = attr(df2_cd_raw, "prob_nozero")
  if (all(c("Pexc", "Valor") %in% names(df2_cd_raw))) {
    df2_cd = df2_cd_raw
    colnames(df2_cd)[1:2] = c("Pexc", "Serie")
    df2_cd$ID = ID2
    if (is_masic) {
      Pnozero2 = round(100 * df2_prob,1)
    }
  } else {
    df2_cd = df2_cd_raw[df2_cd_raw$variable == ID2, c("Pexc", "value"), drop = FALSE]
    colnames(df2_cd) = c("Pexc", "Serie")
    df2_cd$ID = ID2
    if (is_masic && length(df2_prob)) {
      Pnozero2 = 100 * round(as.numeric(df2_prob[ID2]), 2)
    }
  }
  df_cd = rbind(df1_cd,
                df2_cd)
  # Definir límites y breaks para el eje x (probabilidad de excedencia)
  limits_CD = c(0,100)
  breaks_CD = c(0,20,40,60,80,100)
  if (is_masic) {
    # Inicializar variables de probabilidad si no fueron calculadas
    if (!exists("Pnozero1")) Pnozero1 = NA_real_
    if (!exists("Pnozero2")) Pnozero2 = NA_real_
    
    fig = ggplot(df_cd) +
      geom_line(aes(x = Pexc,
                    y = Serie,
                    color = ID,
                    linetype = ID),
                linewidth = 0.5) +
      scale_x_continuous(breaks = breaks_CD,
                         limits = limits_CD)+
      scale_color_manual(values = c("blue","red"),
                         breaks = c(ID1, ID2))+
      scale_linetype_manual(values = c("solid", "twodash"),
                            breaks = c(ID1, ID2))+
      annotate("text",
               x = 90,
               y = diff(range(df_cd$Serie))*0.65,
               label = paste0("Prob. no cero: ",
                              ifelse(is.na(Pnozero1), "N/A", Pnozero1),
                              "%"),
               color = "blue")+
      annotate("text",
               x = 60,
               y = diff(range(df_cd$Serie))*0.65,
               label = paste0("Prob. no cero: ",
                              ifelse(is.na(Pnozero2), "N/A", Pnozero2),
                              "%"),
               color = "red")+
      labs(title = bquote(bold(.(titulo))),
           x = "Probabilidad de excedencia [%]",
           y = label_y)+
      theme_bw()+
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA))+
      theme(legend.position = "none",
            legend.background = element_rect(fill = "white"),
            aspect.ratio = 1/7)
  } else {
    fig = ggplot(df_cd) +
      geom_line(aes(x = Pexc,
                    y = Serie,
                    color = ID,
                    linetype = ID),
                linewidth = 0.5) +
      scale_x_continuous(breaks = breaks_CD,
                         limits = limits_CD)+
      scale_color_manual(values = c("blue","red"),
                         breaks = c(ID1, ID2))+
      scale_linetype_manual(values = c("solid", "twodash"),
                            breaks = c(ID1, ID2))+
      labs(title = bquote(bold(.(titulo))),
           x = "Probabilidad de excedencia [%]",
           y = label_y)+
      theme_bw()+
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA))+
      theme(legend.position = "none",
            legend.background = element_rect(fill = "white"),
            aspect.ratio = 1/7)
  }
  if (isTRUE(logscale)) {
    fig = fig +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))+
      annotation_logticks(sides = "l")
  }
  return(fig)
}
# ----

# plot_double_cves ----
#' @name plot_double_cves
#' @title Generates a graphical comparison of two seasonal variation curves.
#' @description
#' Generates a graphical comparison of two seasonal variation curves. Wrapping of calc_cve function.
#'
#' @param df1 A data.frame corresponding to monthly series 1.
#' @param ID1 A string representing the name of series 1.
#' @param df2 A data.frame corresponding to monthly series 2.
#' @param ID2 A string representing the name of series 2.
#' @param label_y A string representing the y-axis title.
#' @param probs Numeric vector with exceedance probabilities to display (default c(10, 50, 85)). Values should be between 0 and 100.
#' @param aspect_ratio Numeric. Aspect ratio for the plot. Default is 1/4.
#' @param show_legend Logical. If TRUE, displays the legend. Default is FALSE.
#' @return A ggplot graph.
#' @examples
#' # example code
#' # read observed data
#' data("data_daily_timeseries_obs")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' df_obs = agg_day2month(df_obs, agg_function = sum)
#' # read simulated data (example dataset shipped in the package)
#' data("data_daily_timeseries_cr2metv25")
#' df_sim = data_daily_timeseries_cr2metv25$CR2METv2.5_QuintaNormal[, c("dates", "pr_mmd")]
#' df_sim = agg_day2month(df_sim, agg_function = sum)
#' # plot
#' output = plot_double_cves(df1 = df_obs,
#'                        ID1 = 'obs',
#'                        df2 = df_sim,
#'                        ID2 = 'sim',
#'                        label_y = 'precipitation')
#' print(output)
#' @export
plot_double_cves = function(df1,
                         ID1,
                         df2,
                         ID2,
                         label_y,
                         probs = c(10, 50, 85),
                         aspect_ratio = 1/4,
                         show_legend = TRUE){
  # Validar que probs solo contenga valores permitidos
  valid_probs = c(5, 10, 20, 50, 85)
  if (!all(probs %in% valid_probs)) {
    invalid_probs = probs[!probs %in% valid_probs]
    stop("El argumento 'probs' solo puede contener valores de c(5, 10, 20, 50, 85). ",
         "Valores inválidos encontrados: ", paste(invalid_probs, collapse = ", "))
  }
  
  # Estandarizar nombres de columnas y convertir a data.table
  colnames(df1) = c("Fecha", ID1)
  colnames(df2) = c("Fecha", ID2)
  dt1 = data.table::as.data.table(df1)
  dt2 = data.table::as.data.table(df2)
  # Realizar join interno para obtener solo fechas comunes
  dt_plot = dt1[dt2, on = "Fecha", nomatch = 0]
  df_plot = data.table::setDF(dt_plot)
  # Separar las series para cálculo de curvas de variación estacional
  df1 = df_plot[,c(1,2)]
  df2 = df_plot[,c(1,3)]
  # Calcular curvas de variación estacional para cada serie
  df1_cve = calc_cve(df1)
  df1_cve$ID = ID1
  df2_cve = calc_cve(df2)
  df2_cve$ID = ID2
  # Combinar resultados de ambas series
  df_CVE = rbind(df1_cve,
                 df2_cve)
  # Filtrar solo las probabilidades de excedencia seleccionadas por el usuario
  # Convertir valores numéricos (10, 50, 85) a nombres de probabilidad (Pexc10, Pexc50, Pexc85)
  # calc_cve ya devuelve los datos en formato largo con columna 'probability'
  prob_names = paste0("Pexc", as.integer(probs))
  df_CVE = df_CVE[df_CVE$probability %in% prob_names, ]
  fig = ggplot(data = df_CVE,
               aes(x = Mes_n,
                   y = value,
                   color = ID,
                   linetype = ID))+
    geom_line(linewidth = 0.5)+
    geom_point(size = 2)+
    scale_color_manual(breaks = c(ID1, ID2),
                       values = c("blue", "red"),
                       name = if(show_legend) "Serie" else NULL)+
    scale_linetype_manual(breaks = c(ID1, ID2),
                          values = c("solid", "twodash"),
                          name = if(show_legend) "Serie" else NULL)+
    scale_x_continuous("Mes",
                       breaks = 1:12,
                       labels = unique(df_CVE$Mes))+
    scale_y_continuous(name = label_y)+
    facet_wrap(. ~ probability,
               scales = "free")+
    theme_bw()+
    theme(strip.background = element_rect(color = "black", fill = "white"),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
          strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
          strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
          axis.title = element_text(color = "black", size = 10, hjust = 0.5),
          legend.title = element_text(color = "black", size = 10, hjust = 0.5),
          legend.text = element_text(color = "black", size = 10, hjust = 0),
          plot.title = element_text(size = 11, hjust = 0),
          legend.background = element_rect(color = "black", fill = NA))+
    theme(panel.grid.minor.x = element_blank(),
          legend.position = ifelse(show_legend, "top", "none"),
          plot.title = element_blank(),
          legend.background = element_rect(fill = "white"),
          aspect.ratio = aspect_ratio)
  return(fig)
}
# ----

# plot_double_dailycycles ----
#' @name plot_double_dailycycles
#' @title Generates a graphical comparison of two daily mean cycles.
#' @description
#' Generates a graphical comparison of two daily mean cycles.
#'
#' @param df1 A data.frame corresponding to daily series 1.
#' @param ID1 A string representing the name of series 1.
#' @param df2 A data.frame corresponding to daily series 2.
#' @param ID2 A string representing the name of series 2.
#' @param label_y A string representing the y-axis title.
#' @param aspect_ratio Numeric. Aspect ratio for the plot. Default is `1/7`.
#' @param show_legend Logical. If `TRUE`, displays the legend. Default is `FALSE`.
#' @return A ggplot graph.
#' @examples
#' # example code
#' # read observed data
#' data("data_daily_timeseries_obs")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' # read simulated data (example dataset shipped in the package)
#' data("data_daily_timeseries_cr2metv25")
#' df_sim = data_daily_timeseries_cr2metv25$CR2METv2.5_QuintaNormal[, c("dates", "pr_mmd")]
#' # plot
#' output = plot_double_dailycycles(df1 = df_obs,
#'                               ID1 = 'obs',
#'                               df2 = df_sim,
#'                               ID2 = 'sim',
#'                               label_y = 'precipitation')
#' print(output)
#' @export
plot_double_dailycycles = function(df1,
                                ID1,
                                df2,
                                ID2,
                                label_y,
                                aspect_ratio = 1/7,
                                show_legend = FALSE){
  # Estandarizar nombres de columnas y convertir a data.table
  colnames(df1) = c("Fechas", ID1)
  colnames(df2) = c("Fechas", ID2)
  dt1 = data.table::as.data.table(df1)
  dt2 = data.table::as.data.table(df2)
  # Realizar join interno para obtener solo fechas comunes con datos válidos
  dt_dailycycle = dt1[dt2, on = "Fechas", nomatch = 0]
  # Filtrar filas donde ambas series tienen valores no-NA
  dt_dailycycle = dt_dailycycle[!is.na(get(ID1)) & !is.na(get(ID2))]
  # Transformar a formato largo para facilitar el cálculo de promedios
  dt_dailycycle = data.table::melt(dt_dailycycle, id.vars = "Fechas", variable.name = "variable", value.name = "value")
  # Calcular día del año (1-366) para cada fecha
  dt_dailycycle[, iday := lubridate::yday(Fechas)]
  # Calcular promedio por día del año para cada serie
  dt_dailycycle = dt_dailycycle[, .(mean_iday = mean(value, na.rm = TRUE)), by = .(iday, variable)]
  # Ajustar día del año para año hidrológico (Abril = día 1)
  # Días >= 91 (1 abril) se restan 90; días < 91 se suman 276
  dt_dailycycle[, iday := data.table::fifelse(iday >= 91, iday - 90, iday + 276)]
  df_dailycycle = data.table::setDF(dt_dailycycle)
  # Construir el gráfico
  fig = ggplot(df_dailycycle,
               aes(x = iday,
                   y = mean_iday,
                   color = variable,
                   linetype = variable))+
    geom_line(linewidth = 0.5)+
    scale_x_continuous(breaks = c(1,31,62,92,123,154,184,215,245,277,308,336),
                       labels = c("Abr-01","May-01","Jun-01",
                                  "Jul-01","Ago-01","Sep-01",
                                  "Oct-01","Nov-01","Dic-01",
                                  "Ene-01","Feb-01","Mar-01"))+
    scale_color_manual(values = c("blue","red"),
                       breaks = c(ID1, ID2),
                       name = if(show_legend) "Serie" else NULL)+
    scale_linetype_manual(values = c("solid", "twodash"),
                          breaks = c(ID1, ID2),
                          name = if(show_legend) "Serie" else NULL)+
    labs(x = "Fecha",
         y = label_y,
         title = bquote(bold("Año hidrológico promedio")))+
    theme_bw()+
    theme(strip.background = element_rect(color = "black", fill = "white"),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
          strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
          strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
          axis.title = element_text(color = "black", size = 10, hjust = 0.5),
          legend.title = element_text(color = "black", size = 10, hjust = 0.5),
          legend.text = element_text(color = "black", size = 10, hjust = 0),
          plot.title = element_text(size = 11, hjust = 0),
          legend.background = element_rect(color = "black", fill = NA))+
    theme(legend.position = ifelse(show_legend, "right", "none"),
          legend.background = element_rect(fill = NA),
          aspect.ratio = aspect_ratio)
  return(fig)
}
# ----

# diagnostic_2ts ----
#' @name diagnostic_2ts
#' @title Conducts a diagnostic of two daily, monthly, or annual series.
#' @description
#' Conducts a diagnostic of two daily or monthly series, returning a composite
#' figure with multiple panels:
#' \describe{
#' \item{Seasonal variation curves}{For 10%, 50% and 85% exceedance probability.}
#' \item{Time series panels}{Daily (if applicable), monthly and yearly comparisons.}
#' \item{Duration curves}{Resolution depends on `diagnostic_resolution`.}
#' \item{Daily mean cycle}{Only when `diagnostic_resolution = "day"`.}
#' }
#'
#' @param df1 A data.frame corresponding to series 1.
#' @param ID1 A string representing the name of series 1.
#' @param df2 A data.frame corresponding to series 2.
#' @param ID2 A string representing the name of series 2.
#' @param var_id A string representing the variable name, one of "pr", "tas", "vel", "HR", "Qmm", or "Qm3s".
#' @param timestep A character vector of length 2, indicating the timestep of df1 and df2 respectively. Each element must be either "day" or "month".
#' @param diagnostic_resolution A string, either 'day' or 'month', indicating the resolution at which the comparison should be performed.
#' @param performance_metrics A logical value. If FALSE, only mean and standard deviation comparisons are added. If TRUE, PBIAS, KGE, and r2 are also added.
#' @param logscale A logical value indicating whether to plot the duration curve on a logarithmic scale or not.
#' @param filename_png Optional. If not `NULL`, writes the composite plot to this
#'   PNG filename (e.g. `"diagnostic.png"`). Defaults to `NULL` (no file output).
#' @return A ggplot graph.
#' @examples
#' # example code
#' data("data_daily_timeseries_obs")
#' data("data_daily_timeseries_cr2metv25")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' df_sim = data_daily_timeseries_cr2metv25$CR2METv2.5_QuintaNormal[, c("dates", "pr_mmd")]
#' output = diagnostic_2ts(df1 = df_obs,
#'                         ID1 = "obs",
#'                         df2 = df_sim,
#'                         ID2 = "sim",
#'                         var_id = "pr",
#'                         timestep = c("day", "day"),
#'                         diagnostic_resolution = "day",
#'                         performance_metrics = TRUE,
#'                         logscale = FALSE)
#' output
#' @export
diagnostic_2ts = function(df1,
                          ID1,
                          df2,
                          ID2,
                          var_id,
                          timestep,
                          diagnostic_resolution,
                          performance_metrics = TRUE,
                          logscale = FALSE,
                          filename_png = NULL){
  # Validar argumentos y determinar función de agregación según tipo de variable
  diagnostic_resolution = match.arg(diagnostic_resolution,
                                    c("day","month"))
  var_id = match.arg(var_id,
                     c("pr","tas","vel","hr","qmm","qm3s"))
  
  # Validar timestep
  if (missing(timestep) || is.null(timestep)) {
    stop("El argumento 'timestep' es obligatorio. Debe ser un vector de longitud 2 con valores 'day' o 'month'.")
  }
  if (length(timestep) != 2) {
    stop("El argumento 'timestep' debe tener longitud 2 (timestep de df1 y df2 respectivamente).")
  }
  timestep = match.arg(timestep, choices = c("day", "month"), several.ok = TRUE)
  timestep_1 = timestep[1]
  timestep_2 = timestep[2]
  
  # Validar compatibilidad de resoluciones temporales
  if ((timestep_2 == "month" || timestep_1 == "month") &&
      diagnostic_resolution == "day") {
    stop("No se puede diagnosticar a una resolución diaria porque una de las series de tiempo tiene una resolución mensual")
  }
  
  if (var_id %in% c("pr","qmm")) {
    is_masic = TRUE
  } else {
    is_masic = FALSE
  }
  # Determinar función de agregación: suma para precipitación y escorrentía, media para el resto
  idx_var_id = which(str_detect(c("pr","tas","vel","hr","qmm","qm3s"), var_id))
  if (length(idx_var_id) != 1) {
    stop("var_id debe coincidir exactamente con uno de: pr, tas, vel, hr, qmm, qm3s")
  }
  if (idx_var_id == 1) {
    agg_fun = sum
  } else {
    agg_fun = mean
  }
  # Definir unidades y etiquetas según var_id
  units_day = list(
    bquote(mm%.%d^-1), "ºC", bquote(m%.%s^-1), "%", 
    bquote(mm%.%d^-1), bquote(m^3%.%s^-1)
  )
  labels_day = list(
    expression(atop("Precipitación diaria", "["~mm%.%d^-1~"]")),
    expression(atop("Temperatura diaria", "[°C]")),
    expression(atop("Velocidad del viento", "["~m%.%s^-1~"]")),
    expression(atop("Humedad relativa", "[%]")),
    expression(atop("Escorrentía diaria", "["~mm%.%d^-1~"]")),
    expression(atop("Caudal medio diario", "["~m^3%.%s^-1~"]"))
  )
  units_month = list(
    bquote(mm%.%mes^-1), "ºC", bquote(m%.%s^-1), "%",
    bquote(mm%.%mes^-1), bquote(m^3%.%s^-1)
  )
  labels_month = list(
    expression(atop("Precipitación mensual", "["~mm%.%mes^-1~"]")),
    expression(atop("Temperatura media mensual", "[°C]")),
    expression(atop("Velocidad del viento", "["~m%.%s^-1~"]")),
    expression(atop("Humedad relativa", "[%]")),
    expression(atop("Escorrentía mensual", "["~mm%.%mes^-1~"]")),
    expression(atop("Caudal medio mensual", "["~m^3%.%s^-1~"]"))
  )
  units_year = list(
    bquote(mm%.%anho^-1), "ºC", bquote(m%.%s^-1), "%",
    bquote(mm%.%anho^-1), bquote(m^3%.%s^-1)
  )
  labels_year = list(
    expression(atop("Precipitación anual", "["~mm%.%anho^-1~"]")),
    expression(atop("Temperatura media anual", "[°C]")),
    expression(atop("Velocidad del viento", "["~m%.%s^-1~"]")),
    expression(atop("Humedad relativa", "[%]")),
    expression(atop("Escorrentía anual", "["~mm%.%anho^-1~"]")),
    expression(atop("Caudal medio anual", "["~m^3%.%s^-1~"]"))
  )
  
  unit_day = units_day[[idx_var_id]]
  label_day = labels_day[[idx_var_id]]
  unit_month = units_month[[idx_var_id]]
  label_month = labels_month[[idx_var_id]]
  unit_year = units_year[[idx_var_id]]
  label_year = labels_year[[idx_var_id]]
  # Almacenar etiquetas y unidades para cada resolución temporal
  labels_plots = list(unit_day = unit_day,
                      label_day = label_day,
                      unit_month = unit_month,
                      label_month = label_month,
                      unit_year = unit_year,
                      label_year = label_year)
  
  # Preparar series usando helpers reutilizables
  series_prep = prepare_series_for_diagnostic(
    df1 = df1,
    df2 = df2,
    ID1 = ID1,
    ID2 = ID2,
    timestep1 = timestep_1,
    timestep2 = timestep_2,
    agg_function = agg_fun
  )
  
  # Generar lista completa de series (daily, monthly, yearly, CVE)
  list_dfs = generate_diagnostic_series_list(
    df1_daily = series_prep$df1_daily,
    df2_daily = series_prep$df2_daily,
    df1_monthly = series_prep$df1_monthly,
    df2_monthly = series_prep$df2_monthly,
    ID1 = ID1,
    ID2 = ID2,
    agg_function = agg_fun
  )
  # Construir gráficos de diagnóstico según la resolución solicitada
  if (diagnostic_resolution == "day") {
    # Extraer datos y etiquetas para resolución diaria
    df1_daily = list_dfs$df1_daily
    df2_daily = list_dfs$df2_daily
    label_day = labels_plots$label_day
    if (is.list(label_day) && length(label_day) == 1) {
      label_day = label_day[[1]]
    }
    df1_monthly = list_dfs$df1_monthly
    df2_monthly = list_dfs$df2_monthly
    label_month = labels_plots$label_month
    if (is.list(label_month) && length(label_month) == 1) {
      label_month = label_month[[1]]
    }
    df1_yearly = list_dfs$df1_yearly
    df2_yearly = list_dfs$df2_yearly
    label_year = labels_plots$label_year
    if (is.list(label_year) && length(label_year) == 1) {
      label_year = label_year[[1]]
    }
    # Generar string con el período de análisis
    fecha_min = min(df1_daily[[1]], na.rm = TRUE)
    fecha_max = max(df1_daily[[1]], na.rm = TRUE)
    periodo_str = paste0("Periodo: ", fecha_min, " a ", fecha_max)
    # Crear gráficos de series temporales en diferentes resoluciones
    {
      fig_daily = plot_double_ts(df1 = df1_daily,
                              ID1 = ID1,
                              df2 = df2_daily,
                              ID2 = ID2,
                              label_y = label_day,
                              timestep = "day",
                              performance_metrics = performance_metrics)
      fig_monthly = plot_double_ts(df1 = df1_monthly,
                                ID1 = ID1,
                                df2 = df2_monthly,
                                ID2 = ID2,
                                label_y = label_month,
                                timestep = "month",
                                performance_metrics = performance_metrics)
      fig_yearly = plot_double_ts(df1 = df1_yearly,
                               ID1 = ID1,
                               df2 = df2_yearly,
                               ID2 = ID2,
                               label_y = label_year,
                               timestep = "year",
                               performance_metrics = performance_metrics,
                               show_points = TRUE)
    }
    # Crear gráfico de ciclo diario promedio (año hidrológico)
    {
      fig_dailycycle = plot_double_dailycycles(df1 = df1_daily,
                                             ID1 = ID1,
                                             df2 = df2_daily,
                                             ID2 = ID2,
                                             label_y = label_day)
    }
    # Crear gráfico de curva de duración
    {
      fig_CD = plot_double_durationcurves(df1 = df1_daily,
                                        ID1 = ID1,
                                        df2 = df2_daily,
                                        ID2 = ID2,
                                        is_masic = is_masic,
                                        label_y = label_day,
                                        timestep = "day",
                                        logscale = logscale)
    }
    # Crear gráfico de curva de variación estacional (CVE)
    {
      fig_cve = plot_double_cves(df1 = df1_monthly,
                              ID1 = ID1,
                              df2 = df2_monthly,
                              ID2 = ID2,
                              label_y = label_month,
                              probs = c(10, 50, 85),
                              aspect_ratio = 1/4,
                              show_legend = TRUE)
    }
    plot_list_day = list(fig_cve,
                         fig_daily,
                         fig_monthly,
                         fig_yearly,
                         fig_CD,
                         fig_dailycycle)
    plot_list_day = lapply(plot_list_day,
                           function(p) p + theme(aspect.ratio = NULL))
    fig_out = patchwork::wrap_plots(plotlist = plot_list_day,
                                    ncol = 1,
                                    heights = rep(1, length(plot_list_day))) +
      patchwork::plot_annotation(
        title = paste0(paste("Diagnóstico",
                             ID1,
                             "vs",
                             ID2),
                       "\n ",
                       periodo_str),
        theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
      )
    if (!is.null(filename_png)) {
      ggsave(filename_png, plot = fig_out, width = 12, height = 14, dpi = 300)
    }
  }
  if (diagnostic_resolution == "month") {
    # Extraer datos y etiquetas para resolución mensual
    df1_monthly = list_dfs$df1_monthly
    df2_monthly = list_dfs$df2_monthly
    label_month = labels_plots$label_month
    if (is.list(label_month) && length(label_month) == 1) {
      label_month = label_month[[1]]
    }
    df1_yearly = list_dfs$df1_yearly
    df2_yearly = list_dfs$df2_yearly
    label_year = labels_plots$label_year
    if (is.list(label_year) && length(label_year) == 1) {
      label_year = label_year[[1]]
    }
    # Generar string con el período de análisis
    fecha_min = min(df1_monthly[[1]], na.rm = TRUE)
    fecha_max = max(df2_monthly[[1]], na.rm = TRUE)
    periodo_str = paste0("Periodo: ", fecha_min, " a ", fecha_max)
    # Crear gráficos de series temporales mensuales y anuales
    {
      fig_monthly = plot_double_ts(df1 = df1_monthly,
                                ID1 = ID1,
                                df2 = df2_monthly,
                                ID2 = ID2,
                                label_y = label_month,
                                timestep = "month",
                                performance_metrics = performance_metrics,
                                show_points = TRUE)
      fig_yearly = plot_double_ts(df1 = df1_yearly,
                               ID1 = ID1,
                               df2 = df2_yearly,
                               ID2 = ID2,
                               label_y = label_year,
                               timestep = "year",
                               performance_metrics = FALSE,
                               show_points = TRUE)
    }
    # Crear gráfico de curva de duración
    {
      fig_CD = plot_double_durationcurves(df1 = df1_monthly,
                                        ID1 = ID1,
                                        df2 = df2_monthly,
                                        ID2 = ID2,
                                        is_masic = is_masic,
                                        label_y = label_month,
                                        timestep = "month",
                                        logscale = logscale)
    }
    # Crear gráfico de curva de variación estacional (CVE)
    {
      fig_cve = plot_double_cves(df1 = df1_monthly,
                              ID1 = ID1,
                              df2 = df2_monthly,
                              ID2 = ID2,
                              label_y = label_month,
                              probs = c(10, 50, 85),
                              aspect_ratio = 1/4,
                              show_legend = TRUE)
    }
    plot_list_month = list(fig_cve,
                           fig_monthly,
                           fig_yearly,
                           fig_CD)
    plot_list_month = lapply(plot_list_month,
                             function(p) p + theme(aspect.ratio = NULL))
    fig_out = patchwork::wrap_plots(plotlist = plot_list_month,
                                    ncol = 1,
                                    heights = rep(1, length(plot_list_month))) +
      patchwork::plot_annotation(
        title = paste0(paste("Diagnóstico",
                             ID1,
                             "vs",
                             ID2),
                       "\n ",
                       periodo_str),
        theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
                      plot.background = element_rect(fill = "white"))
      )
    if (!is.null(filename_png)) {
      ggsave(filename_png, plot = fig_out, width = 12, height = 10, dpi = 300)
    }
  }
  # Retornar gráfico combinado
  return(fig_out)
}
# ----

# diagnostic_1ts ----
#' @name diagnostic_1ts
#' @title Conducts a diagnostic of a single daily or monthly series.
#' @description
#' Conducts a diagnostic of a single daily or monthly series, returning a composite
#' figure with multiple panels:
#' \describe{
#' \item{Seasonal variation curve}{For 10%, 50% and 85% exceedance probability.}
#' \item{Time series panels}{Daily (if applicable), monthly and yearly.}
#' \item{Duration curve}{Resolution depends on `diagnostic_resolution`.}
#' \item{Daily mean cycle}{Only when `diagnostic_resolution = "day"`.}
#' \item{Descriptive statistics}{Summary statistics panel.}
#' \item{QA/QC panel}{Optional quality assessment visualization.}
#' }
#'
#' @param df A data.frame corresponding to the time series.
#' @param ID A string representing the name of the series.
#' @param var_id A string representing the variable name, one of "pr", "tas", "vel", "HR", "Qmm", or "Qm3s".
#' @param timestep A character string indicating the timestep of the input data, either "day" or "month".
#' @param diagnostic_resolution A string, either 'day' or 'month', indicating the resolution at which the comparison should be performed.
#' @param logscale A logical value indicating whether to plot the duration curve on a logarithmic scale or not.
#' @param include_qa Logical. If TRUE, includes a QA/QC panel. Default is FALSE.
#' @param filename_png Optional. If not `NULL`, writes the composite plot to this
#'   PNG filename (e.g. `"diagnostic.png"`). Defaults to `NULL` (no file output).
#' @return A ggplot graph.
#' @examples
#' # example code
#' data("data_daily_timeseries_obs")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' output = diagnostic_1ts(df = df_obs,
#'                         ID = "obs",
#'                         var_id = "pr",
#'                         timestep = "day",
#'                         diagnostic_resolution = "day",
#'                         logscale = FALSE,
#'                         include_qa = FALSE)
#' output
#' @export
diagnostic_1ts = function(df,
                          ID,
                          var_id,
                          timestep,
                          diagnostic_resolution,
                          logscale = FALSE,
                          include_qa = FALSE,
                          filename_png = NULL){
  # Validar argumentos y determinar función de agregación según tipo de variable
  diagnostic_resolution = match.arg(diagnostic_resolution,
                                    c("day","month"))
  var_id = match.arg(var_id,
                     c("pr","tas","vel","hr","qmm","qm3s"))
  
  # Validar timestep
  timestep = match.arg(timestep, choices = c("day", "month"))
  
  # Validar compatibilidad de resoluciones temporales
  if (timestep == "month" && diagnostic_resolution == "day") {
    stop("No se puede diagnosticar a una resolución diaria porque la serie de tiempo tiene una resolución mensual")
  }
  
  if (var_id %in% c("pr","qmm")) {
    is_masic = TRUE
  } else {
    is_masic = FALSE
  }
  
  # Determinar función de agregación: suma para precipitación y escorrentía, media para el resto
  idx_var_id = which(str_detect(c("pr","tas","vel","hr","qmm","qm3s"), var_id))
  if (length(idx_var_id) != 1) {
    stop("var_id debe coincidir exactamente con uno de: pr, tas, vel, hr, qmm, qm3s")
  }
  if (idx_var_id == 1) {
    agg_fun = sum
  } else {
    agg_fun = mean
  }
  
  # Definir etiquetas y unidades (reutilizar lógica de diagnostic_2ts)
  units_day = list(
    bquote(mm%.%d^-1), "ºC", bquote(m%.%s^-1), "%", 
    bquote(mm%.%d^-1), bquote(m^3%.%s^-1)
  )
  labels_day = list(
    expression(atop("Precipitación diaria", "["~mm%.%d^-1~"]")),
    expression(atop("Temperatura diaria", "[°C]")),
    expression(atop("Velocidad del viento", "["~m%.%s^-1~"]")),
    expression(atop("Humedad relativa", "[%]")),
    expression(atop("Escorrentía diaria", "["~mm%.%d^-1~"]")),
    expression(atop("Caudal medio diario", "["~m^3%.%s^-1~"]"))
  )
  units_month = list(
    bquote(mm%.%mes^-1), "ºC", bquote(m%.%s^-1), "%",
    bquote(mm%.%mes^-1), bquote(m^3%.%s^-1)
  )
  labels_month = list(
    expression(atop("Precipitación mensual", "["~mm%.%mes^-1~"]")),
    expression(atop("Temperatura media mensual", "[°C]")),
    expression(atop("Velocidad del viento", "["~m%.%s^-1~"]")),
    expression(atop("Humedad relativa", "[%]")),
    expression(atop("Escorrentía mensual", "["~mm%.%mes^-1~"]")),
    expression(atop("Caudal medio mensual", "["~m^3%.%s^-1~"]"))
  )
  units_year = list(
    bquote(mm%.%anho^-1), "ºC", bquote(m%.%s^-1), "%",
    bquote(mm%.%anho^-1), bquote(m^3%.%s^-1)
  )
  labels_year = list(
    expression(atop("Precipitación anual", "["~mm%.%anho^-1~"]")),
    expression(atop("Temperatura media anual", "[°C]")),
    expression(atop("Velocidad del viento", "["~m%.%s^-1~"]")),
    expression(atop("Humedad relativa", "[%]")),
    expression(atop("Escorrentía anual", "["~mm%.%anho^-1~"]")),
    expression(atop("Caudal medio anual", "["~m^3%.%s^-1~"]"))
  )
  
  unit_day = units_day[[idx_var_id]]
  label_day = labels_day[[idx_var_id]]
  unit_month = units_month[[idx_var_id]]
  label_month = labels_month[[idx_var_id]]
  unit_year = units_year[[idx_var_id]]
  label_year = labels_year[[idx_var_id]]
  
  # Almacenar etiquetas y unidades para cada resolución temporal
  labels_plots = list(unit_day = unit_day,
                      label_day = label_day,
                      unit_month = unit_month,
                      label_month = label_month,
                      unit_year = unit_year,
                      label_year = label_year)
  
  # Preparar series usando helpers reutilizables
  series_prep = prepare_single_series_for_diagnostic(
    df = df,
    ID = ID,
    timestep = timestep,
    agg_function = agg_fun
  )
  
  # Generar lista completa de series (daily, monthly, yearly, CVE)
  list_dfs = generate_single_diagnostic_series_list(
    df_daily = series_prep$df_daily,
    df_monthly = series_prep$df_monthly,
    ID = ID,
    agg_function = agg_fun
  )
  
  # Construir gráficos de diagnóstico según la resolución solicitada
  if (diagnostic_resolution == "day") {
    # Extraer datos y etiquetas para resolución diaria
    df_daily = list_dfs$df_daily
    label_day = labels_plots$label_day
    # Asegurar que label_day es una expresión, no una lista
    if (is.list(label_day) && length(label_day) == 1) {
      label_day = label_day[[1]]
    }
    df_monthly = list_dfs$df_monthly
    label_month = labels_plots$label_month
    if (is.list(label_month) && length(label_month) == 1) {
      label_month = label_month[[1]]
    }
    df_yearly = list_dfs$df_yearly
    label_year = labels_plots$label_year
    if (is.list(label_year) && length(label_year) == 1) {
      label_year = label_year[[1]]
    }
    
    # Generar string con el período de análisis
    fecha_min = min(df_daily[[1]], na.rm = TRUE)
    fecha_max = max(df_daily[[1]], na.rm = TRUE)
    periodo_str = paste0("Periodo: ", fecha_min, " a ", fecha_max)
    
    # Crear gráficos de series temporales en diferentes resoluciones
    fig_daily = plot_single_ts(df_daily, ID = ID, label_y = label_day, 
                               timestep = "day", show_stats = TRUE)
    fig_monthly = plot_single_ts(df_monthly, ID = ID, label_y = label_month, 
                                 timestep = "month", show_stats = TRUE, aspect_ratio = 1/4)
    fig_yearly = plot_single_ts(df_yearly, ID = ID, label_y = label_year, 
                                timestep = "year", show_stats = TRUE, aspect_ratio = 1/2)
    
    # Crear gráfico de ciclo diario promedio (año hidrológico)
    fig_dailycycle = plot_single_dailycycle(df_daily, ID = ID, label_y = label_day)
    
    # Crear gráfico de curva de duración
    df_cd_raw = calc_durationcurve(df_daily, is_masic = is_masic, skip_validation = TRUE)
    prob_nozero = attr(df_cd_raw, "prob_nozero")
    if (all(c("Pexc", "Valor") %in% names(df_cd_raw))) {
      df_cd = df_cd_raw
      if (is_masic && !is.null(prob_nozero) && length(prob_nozero) > 0) {
        Pnozero = round(100 * prob_nozero[1], 1)
      } else {
        Pnozero = NA_real_
      }
    } else {
      df_cd = df_cd_raw[df_cd_raw$variable == ID, c("Pexc", "value"), drop = FALSE]
      colnames(df_cd) = c("Pexc", "Valor")
      if (is_masic && !is.null(prob_nozero) && ID %in% names(prob_nozero)) {
        Pnozero = round(100 * prob_nozero[ID], 1)
      } else {
        Pnozero = NA_real_
      }
    }
    
    fig_CD = ggplot(df_cd, aes(x = Pexc, y = Valor)) +
      geom_line(linewidth = 0.5, color = "black") +
      scale_x_continuous(breaks = c(0,20,40,60,80,100),
                         limits = c(0,100)) +
      labs(title = bquote(bold("Curva de duración de registros diarios")),
           x = "Probabilidad de excedencia [%]",
           y = label_day) +
      theme_bw() +
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA)) +
      theme(legend.position = "none",
            aspect.ratio = 1/7)
    
    # Agregar texto de probabilidad de no cero si is_masic = TRUE
    if (is_masic && !is.na(Pnozero)) {
      fig_CD = fig_CD +
        annotate("text",
                 x = 90,
                 y = diff(range(df_cd$Valor)) * 0.65,
                 label = paste0("Prob. no cero: ", Pnozero, "%"),
                 color = "black")
    }
    
    if (isTRUE(logscale)) {
      fig_CD = fig_CD +
        scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
        scales::annotation_logticks(sides = "l")
    }
    
    # Crear gráfico de curva de variación estacional (CVE)
    df_cve = list_dfs$df_cve
    df_cve_filtered = df_cve[df_cve$probability %in% c("Pexc10", "Pexc50", "Pexc85"), ]
    
    fig_cve = ggplot(df_cve_filtered, aes(x = Mes_n, y = value, linetype = probability)) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 2) +
      scale_linetype_manual(breaks = c("Pexc10", "Pexc50", "Pexc85"),
                            values = c("dashed", "solid", "dashed"),
                            labels = c("Pexc10", "Pexc50", "Pexc85"),
                            name = "Probabilidad") +
      scale_x_continuous("Mes",
                         breaks = 1:12,
                         labels = unique(df_cve_filtered$Mes)) +
      scale_y_continuous(label_month) +
      facet_wrap(. ~ probability, scales = "free") +
      labs(title = bquote(bold("Curva de variación estacional"))) +
      theme_bw() +
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA)) +
      theme(panel.grid.minor.x = element_blank(),
            legend.position = "none",
            plot.title = element_blank(),
            aspect.ratio = 1/4)
    
    # Crear panel de QA/QC si se solicita
    plot_list_day = list(fig_cve, fig_daily, fig_monthly, fig_yearly, fig_CD, fig_dailycycle)
    
    if (include_qa) {
      qa_result = qa_ts(df, var_id = var_id, timestep = "day", 
                        title_y = as.character(label_day), skip_validation = TRUE)
      fig_qa = qa_result$ggplot_qaqc
      plot_list_day = c(plot_list_day, list(fig_qa))
    }
    
    plot_list_day = lapply(plot_list_day,
                           function(p) p + theme(aspect.ratio = NULL))
    fig_out = patchwork::wrap_plots(plotlist = plot_list_day,
                                    ncol = 1,
                                    heights = rep(1, length(plot_list_day))) +
      patchwork::plot_annotation(
        title = paste0(paste("Diagnóstico", ID),
                       "\n ",
                       periodo_str),
        theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
      )
    if (!is.null(filename_png)) {
      ggsave(filename_png, plot = fig_out, width = 12, height = 14, dpi = 300)
    }
  }
  
  if (diagnostic_resolution == "month") {
    # Extraer datos y etiquetas para resolución mensual
    df_monthly = list_dfs$df_monthly
    label_month = labels_plots$label_month
    if (is.list(label_month) && length(label_month) == 1) {
      label_month = label_month[[1]]
    }
    df_yearly = list_dfs$df_yearly
    label_year = labels_plots$label_year
    if (is.list(label_year) && length(label_year) == 1) {
      label_year = label_year[[1]]
    }
    
    # Generar string con el período de análisis
    fecha_min = min(df_monthly[[1]], na.rm = TRUE)
    fecha_max = max(df_monthly[[1]], na.rm = TRUE)
    periodo_str = paste0("Periodo: ", fecha_min, " a ", fecha_max)
    
    # Crear gráficos de series temporales mensuales y anuales
    fig_monthly = plot_single_ts(df_monthly, ID = ID, label_y = label_month, 
                                 timestep = "month", show_stats = TRUE, aspect_ratio = 1/4)
    fig_yearly = plot_single_ts(df_yearly, ID = ID, label_y = label_year, 
                                timestep = "year", show_stats = TRUE, aspect_ratio = 1/2)
    
    # Crear gráfico de curva de duración
    df_cd_raw = calc_durationcurve(df_monthly, is_masic = is_masic, skip_validation = TRUE)
    prob_nozero = attr(df_cd_raw, "prob_nozero")
    if (all(c("Pexc", "Valor") %in% names(df_cd_raw))) {
      df_cd = df_cd_raw
      if (is_masic && !is.null(prob_nozero) && length(prob_nozero) > 0) {
        Pnozero = round(100 * prob_nozero[1], 1)
      } else {
        Pnozero = NA_real_
      }
    } else {
      df_cd = df_cd_raw[df_cd_raw$variable == ID, c("Pexc", "value"), drop = FALSE]
      colnames(df_cd) = c("Pexc", "Valor")
      if (is_masic && !is.null(prob_nozero) && ID %in% names(prob_nozero)) {
        Pnozero = round(100 * prob_nozero[ID], 1)
      } else {
        Pnozero = NA_real_
      }
    }
    
    fig_CD = ggplot(df_cd, aes(x = Pexc, y = Valor)) +
      geom_line(linewidth = 0.5, color = "black") +
      scale_x_continuous(breaks = c(0,20,40,60,80,100),
                         limits = c(0,100)) +
      labs(title = bquote(bold("Curva de duración de registros mensuales")),
           x = "Probabilidad de excedencia [%]",
           y = label_month) +
      theme_bw() +
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA)) +
      theme(legend.position = "none",
            aspect.ratio = 1/7)
    
    # Agregar texto de probabilidad de no cero si is_masic = TRUE
    if (is_masic && !is.na(Pnozero)) {
      fig_CD = fig_CD +
        annotate("text",
                 x = 90,
                 y = diff(range(df_cd$Valor)) * 0.65,
                 label = paste0("Prob. no cero: ", Pnozero, "%"),
                 color = "black")
    }
    
    if (isTRUE(logscale)) {
      fig_CD = fig_CD +
        scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
        scales::annotation_logticks(sides = "l")
    }
    
    # Crear gráfico de curva de variación estacional (CVE)
    df_cve = list_dfs$df_cve
    df_cve_filtered = df_cve[df_cve$probability %in% c("Pexc10", "Pexc50", "Pexc85"), ]
    
    fig_cve = ggplot(df_cve_filtered, aes(x = Mes_n, y = value, color = probability, linetype = probability)) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 2) +
      scale_color_manual(breaks = c("Pexc10", "Pexc50", "Pexc85"),
                         values = c("red", "blue", "green"),
                         labels = c("Pexc10", "Pexc50", "Pexc85"),
                         name = "Probabilidad") +
      scale_linetype_manual(breaks = c("Pexc10", "Pexc50", "Pexc85"),
                            values = c("dashed", "solid", "dashed"),
                            labels = c("Pexc10", "Pexc50", "Pexc85"),
                            name = "Probabilidad") +
      scale_x_continuous("Mes",
                         breaks = 1:12,
                         labels = unique(df_cve_filtered$Mes)) +
      scale_y_continuous(label_month) +
      facet_wrap(. ~ probability, scales = "free") +
      labs(title = bquote(bold("Curva de variación estacional"))) +
      theme_bw() +
      theme(strip.background = element_rect(color = "black", fill = "white"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            strip.text.x = element_text(color = "black", size = 10, hjust = 0.5, face = "bold"),
            strip.text.y = element_text(color = "black", size = 10, hjust = 0.5),
            axis.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.title = element_text(color = "black", size = 10, hjust = 0.5),
            legend.text = element_text(color = "black", size = 10, hjust = 0),
            plot.title = element_text(size = 11, hjust = 0),
            legend.background = element_rect(color = "black", fill = NA)) +
      theme(panel.grid.minor.x = element_blank(),
            legend.position = "none",
            plot.title = element_blank(),
            aspect.ratio = 1/4)
    
    # Crear panel de QA/QC si se solicita
    plot_list_month = list(fig_cve, fig_monthly, fig_yearly, fig_CD)
    
    if (include_qa) {
      qa_result = qa_ts(df, var_id = var_id, timestep = "month", 
                        title_y = as.character(label_month), skip_validation = TRUE)
      fig_qa = qa_result$ggplot_qaqc
      plot_list_month = c(plot_list_month, list(fig_qa))
    }
    
    plot_list_month = lapply(plot_list_month,
                             function(p) p + theme(aspect.ratio = NULL))
    fig_out = patchwork::wrap_plots(plotlist = plot_list_month,
                                    ncol = 1,
                                    heights = rep(1, length(plot_list_month))) +
      patchwork::plot_annotation(
        title = paste0(paste("Diagnóstico", ID),
                       "\n ",
                       periodo_str),
        theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
                      plot.background = element_rect(fill = "white"))
      )
    if (!is.null(filename_png)) {
      ggsave(filename_png, plot = fig_out, width = 12, height = 10, dpi = 300)
    }
  }
  
  # Retornar gráfico combinado
  return(fig_out)
}
# ----

# plot_climogram ----
#' @name plot_climogram
#' @title Generates a graphical representation of climogram.
#' @description
#' Generates a graphical representation of climogram.
#'
#' @param data_pr A data.frame (Dates, numeric) containing dates and monthly precipitation values.
#' @param data_tmean A data.frame (Dates, numeric) containing dates and monthly mean temperature values.
#' @param title_plot A string representing the title of the plot.
#' @param frac_req A numeric value between 0 and 1, representing the fraction of months required to calculate the monthly mean, see agg_month2seas function.
#' @param filename_fig A string. If defined, the figure is exported as a PNG.
#' @return A ggplot graph.
#' @examples
#' # example code
#' data("data_daily_timeseries_obs")
#' df_obs_pr = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' df_obs_pr = agg_day2month(df_obs_pr, agg_function = sum)
#' df_obs_tasmean = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "tasmean_celsius")]
#' df_obs_tasmean = agg_day2month(df_obs_tasmean, agg_function = mean)
#' # plot
#' output = plot_climogram(data_pr = df_obs_pr,
#'                         data_tmean = df_obs_tasmean,
#'                         title_plot = 'Climograma en estación Lagunitas',
#'                         frac_req = 0.8,
#'                         filename_fig = NULL)
#' print(output)
#' @export
plot_climogram = function(data_pr,
                          data_tmean,
                          title_plot,
                          frac_req = 0.8,
                          filename_fig = NULL){
  # Estandarizar nombres de columnas y convertir a data.table
  colnames(data_pr) = c('dates', 'pr')
  colnames(data_tmean) = c('dates', 'tmean')
  dt_pr = data.table::as.data.table(data_pr)
  dt_tmean = data.table::as.data.table(data_tmean)
  # Realizar join interno para obtener solo fechas comunes con datos válidos
  dt_monthly = dt_pr[dt_tmean, on = "dates", nomatch = 0]
  # Filtrar filas donde ambas variables tienen valores no-NA
  dt_monthly = dt_monthly[!is.na(pr) & !is.na(tmean)]
  data_monthly = data.table::setDF(dt_monthly)
  
  
  # Identificar rango de fechas para el subtítulo
  dates_period = paste0('Periodo: ',
                        min(data_monthly$dates),
                        '/',
                        max(data_monthly$dates))
  
  
  # Calcular datos estacionales (promedios mensuales)
  data_seasonly = agg_month2season(data_monthly,
                                   frac_req = frac_req)
  dt_seasonly = data.table::as.data.table(data_seasonly)
  # Ajustar orden de meses para año hidrológico (Abril = mes 1)
  dt_seasonly[, months := data.table::fifelse(months < 4, months + 9, months - 3)]
  data_seasonly = data.table::setDF(dt_seasonly)
  # Calcular coeficiente de escala para el eje secundario de temperatura
  # Esto permite mostrar precipitación y temperatura en la misma escala visual
  ratio_pr_tmean = round(max(data_seasonly$pr) / max(data_seasonly$tmean))
  coef_plot = ifelse(ratio_pr_tmean %% 5 > 3,
                     ratio_pr_tmean + 5,
                     ratio_pr_tmean - ratio_pr_tmean %% 5 )
  # Determinar límites del eje y considerando ambas variables escaladas
  max_y = max(max(data_seasonly$pr),
              max(data_seasonly$tmean*coef_plot))
  min_pr = 0
  min_tmean = min(data_seasonly$tmean*coef_plot)
  min_y = ifelse(min_tmean < min_pr,
                 min_tmean,
                 min_pr)
  # Construir el gráfico
  fig = ggplot(data_seasonly,
               aes(x = months))+
    geom_line(aes(y = tmean*coef_plot), color = "red", linewidth = 0.5)+
    geom_line(aes(y = pr), color = "blue", linewidth = 0.5)+
    geom_point(aes(y = tmean*coef_plot), color = "red", size = 1.5)+
    geom_point(aes(y = pr), color = "blue", size = 1.5)+
    scale_y_continuous(name = "Precipitación [mm/mes]",
                       sec.axis = sec_axis(~./coef_plot,
                                           name = "Temperatura media [°C]"),
                       limits = c(min_y,max_y))+
    scale_x_continuous(name = 'Mes',
                       breaks = 1:12,
                       labels = c('ABR','MAY','JUN',
                                  'JUL','AGO','SEP',
                                  'OCT','NOV','DIC',
                                  'ENE','FEB','MAR'))+
    labs(title = title_plot,
         subtitle = dates_period)+
    theme_bw()+
    theme(axis.text.y.left = element_text(color = "blue"),
          axis.title.y.left = element_text(color = "blue"),
          axis.ticks.y.left = element_line(color = "blue"),
          axis.text.y.right = element_text(color = "red"),
          axis.title.y.right = element_text(color = "red"),
          axis.ticks.y.right = element_line(color = "red"),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  # Exportar gráfico a archivo PNG si se especifica nombre de archivo
  if (!is.null(filename_fig)) {
    ggsave(filename = filename_fig,
           dpi = 300,
           plot = fig,
           width = 8,
           height = 4)
  }
  # Retornar objeto ggplot
  return(fig)
}
# ----