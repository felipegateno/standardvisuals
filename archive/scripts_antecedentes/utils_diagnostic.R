# Utility functions for diagnostic workflows
# These helpers are used by diagnostic_2ts and shinyapp_compare2timeseries

# complete_date_sequence ----
#' Complete a date sequence with missing dates filled as NA
#'
#' @param data A data.frame with Date in first column and numeric in second
#' @param by Character string: "day", "month", or "year"
#' @param ID Character string: name for the numeric column
#' @return A data.frame with complete date sequence and original data merged
#' @keywords internal
complete_date_sequence = function(data, by = c("day", "month", "year"), ID) {
  by = match.arg(by)
  stopifnot(is.data.frame(data), ncol(data) >= 2)
  stopifnot(inherits(data[[1]], "Date"))
  
  # Asegurar que data es un data.frame estándar (no tibble ni data.table)
  # y que todas las columnas son vectores atómicos
  data = as.data.frame(data, stringsAsFactors = FALSE)
  
  # Verificar y convertir columnas que sean listas a vectores
  for (i in seq_len(ncol(data))) {
    if (is.list(data[[i]]) && !inherits(data[[i]], "Date")) {
      # Intentar convertir lista a vector
      tryCatch({
        data[[i]] = unlist(data[[i]], recursive = FALSE)
      }, error = function(e) {
        stop("La columna ", i, " ('", names(data)[i], "') es una lista y no puede ser convertida a vector")
      })
    }
  }
  
  colnames(data) = c("Fecha", ID)
  
  # Extraer columnas como vectores atómicos para asegurar que no sean listas
  fecha_col = as.Date(data[[1]])
  valor_col = as.numeric(data[[2]])
  
  # Crear nuevo data.frame limpio
  data_clean = data.frame(Fecha = fecha_col, Valor = valor_col, stringsAsFactors = FALSE)
  colnames(data_clean) = c("Fecha", ID)
  
  # Convertir a data.table de forma segura
  dt = data.table::as.data.table(data_clean)
  
  # Verificar que dt es un data.table válido
  if (!data.table::is.data.table(dt)) {
    stop("Error al convertir data a data.table")
  }
  
  # Verificar que las columnas son del tipo correcto
  if (!inherits(dt$Fecha, "Date")) {
    dt$Fecha = as.Date(dt$Fecha)
  }
  
  # Create complete sequence
  full_seq = data.frame(Fecha = seq.Date(
    from = min(dt$Fecha, na.rm = TRUE),
    to = max(dt$Fecha, na.rm = TRUE),
    by = by
  ), stringsAsFactors = FALSE)
  dt_full = data.table::as.data.table(full_seq)
  
  # Verificar que dt_full es un data.table válido
  if (!data.table::is.data.table(dt_full)) {
    stop("Error al crear dt_full")
  }
  
  # Merge with original data - usar merge explícito en lugar de join sintáctico
  dt_complete = merge(dt_full, dt, by = "Fecha", all.x = TRUE, sort = FALSE)
  dt_complete = data.table::as.data.table(dt_complete)
  data.table::setDF(dt_complete)
  dt_complete
}

# merge_common_dates ----
#' Merge two data.frames on common dates, filtering for non-NA values in both
#'
#' @param df1 data.frame with Date and numeric column
#' @param df2 data.frame with Date and numeric column
#' @param ID1 Character: name for df1 numeric column
#' @param ID2 Character: name for df2 numeric column
#' @return A data.frame with merged data, only rows where both have non-NA values
#' @keywords internal
merge_common_dates = function(df1, df2, ID1, ID2, agg_function = mean) {
  stopifnot(is.data.frame(df1), is.data.frame(df2))
  stopifnot(ncol(df1) >= 2, ncol(df2) >= 2)
  stopifnot(inherits(df1[[1]], "Date"), inherits(df2[[1]], "Date"))
  
  colnames(df1) = c("Fecha", ID1)
  colnames(df2) = c("Fecha", ID2)
  
  dt1 = data.table::as.data.table(df1)
  dt2 = data.table::as.data.table(df2)

   # Resolver duplicados por fecha aplicando la función de agregación
   if (any(duplicated(dt1$Fecha))) {
     dt1 = dt1[, .(tmp_val = agg_function(get(ID1), na.rm = TRUE)), by = Fecha]
     data.table::setnames(dt1, "tmp_val", ID1)
   }
   if (any(duplicated(dt2$Fecha))) {
     dt2 = dt2[, .(tmp_val = agg_function(get(ID2), na.rm = TRUE)), by = Fecha]
     data.table::setnames(dt2, "tmp_val", ID2)
   }
  
  # Inner join on common dates
  dt_merge = dt1[dt2, on = "Fecha", nomatch = 0]
  
  # Filter rows where both series have non-NA values
  colnames_merge = names(dt_merge)
  num_cols = colnames_merge[colnames_merge != "Fecha"]
  if (length(num_cols) >= 2) {
    dt_merge = dt_merge[!is.na(get(num_cols[1])) & !is.na(get(num_cols[2]))]
  }
  
  data.table::setDF(dt_merge)
  dt_merge
}

# prepare_series_for_diagnostic ----
#' Prepare daily and monthly series from two input data.frames
#'
#' Handles different input timesteps (day/day, day/month, month/day, month/month)
#' and returns standardized daily and monthly series ready for diagnostics.
#'
#' @param df1 data.frame with Date and numeric column (series 1)
#' @param df2 data.frame with Date and numeric column (series 2)
#' @param ID1 Character: name for series 1
#' @param ID2 Character: name for series 2
#' @param timestep1 Character: "day" or "month" - timestep of df1
#' @param timestep2 Character: "day" or "month" - timestep of df2
#' @param agg_function Function: aggregation function (sum or mean)
#' @return A list with df1_daily, df2_daily, df1_monthly, df2_monthly (daily may be NULL)
#' @keywords internal
prepare_series_for_diagnostic = function(df1, df2, ID1, ID2, 
                                         timestep1, timestep2, agg_function) {
  stopifnot(timestep1 %in% c("day", "month"), timestep2 %in% c("day", "month"))
  
  # Parse dates and standardize column names
  df1 = parsedates(df1)
  colnames(df1) = c("Fecha", ID1)
  df2 = parsedates(df2)
  colnames(df2) = c("Fecha", ID2)
  
  df1_daily = NULL
  df2_daily = NULL
  
  # Case 1: Both are daily
  if (timestep1 == "day" && timestep2 == "day") {
    # Merge on common dates
    df_merge = merge_common_dates(df1, df2, ID1, ID2, agg_function = agg_function)
    df1_daily = df_merge[, c(1, 2), drop = FALSE]
    df2_daily = df_merge[, c(1, 3), drop = FALSE]
    
    # Complete daily sequences
    df1_daily = complete_date_sequence(df1_daily, by = "day", ID = ID1)
    df2_daily = complete_date_sequence(df2_daily, by = "day", ID = ID2)
    
    # Aggregate to monthly
    df1_monthly = agg_day2month(df1_daily, agg_function = agg_function, skip_validation = TRUE)
    colnames(df1_monthly)[1] = "Fecha"
    df2_monthly = agg_day2month(df2_daily, agg_function = agg_function, skip_validation = TRUE)
    colnames(df2_monthly)[1] = "Fecha"
  }
  
  # Case 2: df1 daily, df2 monthly
  if (timestep1 == "day" && timestep2 == "month") {
    # Aggregate df1 to monthly
    df1 = agg_day2month(df1, agg_function = agg_function)
    colnames(df1)[1] = "Fecha"
    
    # Normalize df2 dates to first of month
    dt2 = data.table::as.data.table(df2)
    dt2[, Fecha := lubridate::floor_date(Fecha, unit = "month")]
    df2 = data.table::setDF(dt2)
    
    # Complete df1 monthly sequence
    df1 = complete_date_sequence(df1, by = "month", ID = ID1)
    
    # Merge on common dates
    df_merge = merge_common_dates(df1, df2, ID1, ID2, agg_function = agg_function)
    df1_monthly = df_merge[, c(1, 2), drop = FALSE]
    df2_monthly = df_merge[, c(1, 3), drop = FALSE]
  }
  
  # Case 3: df1 monthly, df2 daily
  if (timestep1 == "month" && timestep2 == "day") {
    # Aggregate df2 to monthly
    df2 = agg_day2month(df2, agg_function = agg_function)
    colnames(df2)[1] = "Fecha"
    
    # Normalize df1 dates to first of month
    dt1 = data.table::as.data.table(df1)
    dt1[, Fecha := lubridate::floor_date(Fecha, unit = "month")]
    df1 = data.table::setDF(dt1)
    
    # Complete df2 monthly sequence
    df2 = complete_date_sequence(df2, by = "month", ID = ID2)
    
    # Merge on common dates
    df_merge = merge_common_dates(df1, df2, ID1, ID2, agg_function = agg_function)
    df1_monthly = df_merge[, c(1, 2), drop = FALSE]
    df2_monthly = df_merge[, c(1, 3), drop = FALSE]
  }
  
  # Case 4: Both are monthly
  if (timestep1 == "month" && timestep2 == "month") {
    # Normalize both to first of month
    dt1 = data.table::as.data.table(df1)
    dt1[, Fecha := lubridate::floor_date(Fecha, unit = "month")]
    df1 = data.table::setDF(dt1)
    
    dt2 = data.table::as.data.table(df2)
    dt2[, Fecha := lubridate::floor_date(Fecha, unit = "month")]
    df2 = data.table::setDF(dt2)
    
    # Complete both sequences
    df1 = complete_date_sequence(df1, by = "month", ID = ID1)
    df2 = complete_date_sequence(df2, by = "month", ID = ID2)
    
    # Merge on common dates
    df_merge = merge_common_dates(df1, df2, ID1, ID2, agg_function = agg_function)
    df1_monthly = df_merge[, c(1, 2), drop = FALSE]
    df2_monthly = df_merge[, c(1, 3), drop = FALSE]
  }
  
  # Ensure column names are correct
  colnames(df1_monthly) = c("Fecha", ID1)
  colnames(df2_monthly) = c("Fecha", ID2)
  
  # Complete monthly sequences
  df1_monthly = complete_date_sequence(df1_monthly, by = "month", ID = ID1)
  df2_monthly = complete_date_sequence(df2_monthly, by = "month", ID = ID2)
  
  list(df1_daily = df1_daily,
       df2_daily = df2_daily,
       df1_monthly = df1_monthly,
       df2_monthly = df2_monthly)
}

# generate_diagnostic_series_list ----
#' Generate complete list of series (daily, monthly, yearly, CVE) for diagnostics
#'
#' @param df1_daily data.frame or NULL: daily series 1
#' @param df2_daily data.frame or NULL: daily series 2
#' @param df1_monthly data.frame: monthly series 1
#' @param df2_monthly data.frame: monthly series 2
#' @param ID1 Character: name for series 1
#' @param ID2 Character: name for series 2
#' @param agg_function Function: aggregation function for yearly
#' @return A list with df1_daily, df2_daily, df1_monthly, df2_monthly, df1_yearly, df2_yearly, df1_cve, df2_cve
#' @keywords internal
generate_diagnostic_series_list = function(df1_daily, df2_daily, df1_monthly, df2_monthly,
                                           ID1, ID2, agg_function) {
  # Calculate CVE for each monthly series
  df1_cve = calc_cve(df1_monthly, skip_validation = TRUE)
  df2_cve = calc_cve(df2_monthly, skip_validation = TRUE)
  
  # Calculate yearly series
  df1_yearly = agg_month2year(df1_monthly, agg_function = agg_function, skip_validation = TRUE)
  df1_yearly = df1_yearly[stats::complete.cases(df1_yearly), ]
  df2_yearly = agg_month2year(df2_monthly, agg_function = agg_function, skip_validation = TRUE)
  df2_yearly = df2_yearly[stats::complete.cases(df2_yearly), ]
  
  colnames(df1_yearly) = c("Fecha", ID1)
  colnames(df2_yearly) = c("Fecha", ID2)
  
  # Complete yearly sequences
  if (nrow(df1_yearly) > 0 && nrow(df2_yearly) > 0) {
    df1_yearly = complete_date_sequence(df1_yearly, by = "year", ID = ID1)
    df2_yearly = complete_date_sequence(df2_yearly, by = "year", ID = ID2)
  }
  
  # Build output list
  if (!is.null(df1_daily) && !is.null(df2_daily)) {
    list(df1_daily = df1_daily,
         df2_daily = df2_daily,
         df1_monthly = df1_monthly,
         df2_monthly = df2_monthly,
         df1_yearly = df1_yearly,
         df2_yearly = df2_yearly,
         df1_cve = df1_cve,
         df2_cve = df2_cve)
  } else {
    list(df1_monthly = df1_monthly,
         df2_monthly = df2_monthly,
         df1_yearly = df1_yearly,
         df2_yearly = df2_yearly,
         df1_cve = df1_cve,
         df2_cve = df2_cve)
  }
}

# prepare_single_series_for_diagnostic ----
#' Prepare daily and monthly series from a single input data.frame
#'
#' Handles different input timesteps (day or month) and returns standardized
#' daily and monthly series ready for diagnostics.
#'
#' @param df data.frame with Date and numeric column
#' @param ID Character: name for the series
#' @param timestep Character: "day" or "month" - timestep of df
#' @param agg_function Function: aggregation function (sum or mean)
#' @return A list with df_daily, df_monthly (daily may be NULL)
#' @keywords internal
prepare_single_series_for_diagnostic = function(df, ID, timestep, agg_function) {
  stopifnot(timestep %in% c("day", "month"))
  
  # Parse dates and standardize column names
  df = parsedates(df)
  colnames(df) = c("Fecha", ID)
  
  df_daily = NULL
  
  # Case 1: Input is daily
  if (timestep == "day") {
    # Complete daily sequence
    df_daily = complete_date_sequence(df, by = "day", ID = ID)
    
    # Aggregate to monthly
    df_monthly = agg_day2month(df_daily, agg_function = agg_function, skip_validation = TRUE)
    colnames(df_monthly)[1] = "Fecha"
  }
  
  # Case 2: Input is monthly
  if (timestep == "month") {
    # Normalize dates to first of month
    dt = data.table::as.data.table(df)
    dt[, Fecha := lubridate::floor_date(Fecha, unit = "month")]
    df = data.table::setDF(dt)
    
    # Complete monthly sequence
    df_monthly = complete_date_sequence(df, by = "month", ID = ID)
  }
  
  # Ensure column names are correct
  colnames(df_monthly) = c("Fecha", ID)
  
  # Complete monthly sequences
  df_monthly = complete_date_sequence(df_monthly, by = "month", ID = ID)
  
  list(df_daily = df_daily,
       df_monthly = df_monthly)
}

# generate_single_diagnostic_series_list ----
#' Generate complete list of series (daily, monthly, yearly, CVE) for single series diagnostics
#'
#' @param df_daily data.frame or NULL: daily series
#' @param df_monthly data.frame: monthly series
#' @param ID Character: name for the series
#' @param agg_function Function: aggregation function for yearly
#' @return A list with df_daily, df_monthly, df_yearly, df_cve
#' @keywords internal
generate_single_diagnostic_series_list = function(df_daily, df_monthly, ID, agg_function) {
  # Calculate CVE for monthly series
  df_cve = calc_cve(df_monthly, skip_validation = TRUE)
  
  # Calculate yearly series
  df_yearly = agg_month2year(df_monthly, agg_function = agg_function, skip_validation = TRUE)
  df_yearly = df_yearly[stats::complete.cases(df_yearly), ]
  
  colnames(df_yearly) = c("Fecha", ID)
  
  # Complete yearly sequences
  if (nrow(df_yearly) > 0) {
    df_yearly = complete_date_sequence(df_yearly, by = "year", ID = ID)
  }
  
  # Build output list
  if (!is.null(df_daily)) {
    list(df_daily = df_daily,
         df_monthly = df_monthly,
         df_yearly = df_yearly,
         df_cve = df_cve)
  } else {
    list(df_monthly = df_monthly,
         df_yearly = df_yearly,
         df_cve = df_cve)
  }
}

# plot_single_ts ----
#' @name plot_single_ts
#' @title Plot a single time series
#' @description
#' Generates a graphical representation of a single daily, monthly, or annual time series.
#'
#' @param df A data.frame corresponding to the time series. Must have exactly 2 columns: Date (first) and value (second).
#' @param ID A string representing the name of the series (used for labels and title).
#' @param label_y A string representing the y-axis title.
#' @param timestep A string indicating the time aggregation of the data frame. Accepts 'day'/'daily'/'d', 'month'/'monthly'/'m', or 'year'/'yearly'/'y'.
#' @param show_stats Logical. If TRUE, displays statistics (mean, standard deviation, min, max) in the title. Default is TRUE.
#' @param aspect_ratio Numeric. Aspect ratio for the plot. Default is 1/7.
#' @return A ggplot graph.
#' @examples
#' # example code
#' data("data_daily_timeseries_obs")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' # plot
#' output = plot_single_ts(df = df_obs,
#'                         ID = 'obs',
#'                         label_y = 'Precipitación diaria (mm · d⁻¹)',
#'                         timestep = 'day',
#'                         show_stats = TRUE)
#' print(output)
#' @export
plot_single_ts = function(df, ID, label_y, timestep, show_stats = TRUE, aspect_ratio = 1/7) {
  stopifnot(is.data.frame(df), ncol(df) >= 2)
  stopifnot(inherits(df[[1]], "Date"))
  
  colnames(df) = c("Fecha", "Valor")
  
  # Remove leading/trailing NA periods
  df_valid = df[!is.na(df$Valor), ]
  if (nrow(df_valid) > 0) {
    min_date = min(df_valid$Fecha, na.rm = TRUE)
    max_date = max(df_valid$Fecha, na.rm = TRUE)
    df = df[df$Fecha >= min_date & df$Fecha <= max_date, ]
  }
  
  # Calculate statistics if requested
  if (show_stats) {
    mean_val = round(mean(df$Valor, na.rm = TRUE), 2)
    sd_val = round(stats::sd(df$Valor, na.rm = TRUE), 2)
    min_val = round(min(df$Valor, na.rm = TRUE), 2)
    max_val = round(max(df$Valor, na.rm = TRUE), 2)
    
    # Determine title based on timestep
    if (timestep == "day") {
      titulo = "serie diaria"
    } else if (timestep == "month") {
      titulo = "serie mensual"
    } else {
      titulo = "serie anual"
    }
    
    subtitulo = bquote(bold(.(ID)) ~ bold(.(titulo)) ~
                       bar(x) == .(mean_val) ~
                       "," ~ S[x] == .(sd_val) ~
                       ", min" == .(min_val) ~
                       ", max" == .(max_val))
  } else {
    subtitulo = ID
  }
  
  # Generate breaks for x-axis
  x_breaks = scales::pretty_breaks(n = 8)(c(min(df$Fecha, na.rm = TRUE), max(df$Fecha, na.rm = TRUE)))
  
  # Build plot
  fig = ggplot(df, aes(x = Fecha, y = Valor)) +
    geom_line(linewidth = 0.5, color = "black", na.rm = TRUE) +
    scale_x_date(limits = c(min(df$Fecha, na.rm = TRUE), max(df$Fecha, na.rm = TRUE)),
                 breaks = x_breaks,
                 date_labels = "%m/%y") +
    labs(x = "Fecha",
         y = label_y,
         title = subtitulo) +
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
          aspect.ratio = aspect_ratio)
  
  return(fig)
}

# plot_single_dailycycle ----
#' @name plot_single_dailycycle
#' @title Plot daily mean cycle for a single series
#' @description
#' Generates a graphical representation of the daily mean cycle (hydrological year average) for a single daily time series.
#'
#' @param df A data.frame corresponding to the daily time series. Must have exactly 2 columns: Date (first) and value (second).
#' @param ID A string representing the name of the series (used for labels and title).
#' @param label_y A string representing the y-axis title.
#' @param aspect_ratio Numeric. Aspect ratio for the plot. Default is 1/7.
#' @return A ggplot graph.
#' @examples
#' # example code
#' data("data_daily_timeseries_obs")
#' df_obs = data_daily_timeseries_obs$obs_Lagunitas[, c("dates", "pr_mmd")]
#' # plot
#' output = plot_single_dailycycle(df = df_obs,
#'                                 ID = 'obs',
#'                                 label_y = 'Precipitación diaria (mm · d⁻¹)')
#' print(output)
#' @export
plot_single_dailycycle = function(df, ID, label_y, aspect_ratio = 1/7) {
  stopifnot(is.data.frame(df), ncol(df) >= 2)
  stopifnot(inherits(df[[1]], "Date"))
  
  colnames(df) = c("Fechas", ID)
  dt = data.table::as.data.table(df)
  
  # Filter non-NA values
  dt = dt[!is.na(get(ID))]
  
  # Calculate day of year (1-366)
  dt[, iday := lubridate::yday(Fechas)]
  
  # Calculate mean by day of year
  dt_dailycycle = dt[, .(mean_iday = mean(get(ID), na.rm = TRUE)), by = iday]
  
  # Adjust for hydrological year (April = day 1)
  dt_dailycycle[, iday := data.table::fifelse(iday >= 91, iday - 90, iday + 276)]
  
  df_dailycycle = data.table::setDF(dt_dailycycle)
  
  # Build plot
  fig = ggplot(df_dailycycle, aes(x = iday, y = mean_iday)) +
    geom_line(linewidth = 0.5, color = "black") +
    scale_x_continuous(breaks = c(1,31,62,92,123,154,184,215,245,277,308,336),
                       labels = c("Abr-01","May-01","Jun-01",
                                  "Jul-01","Ago-01","Sep-01",
                                  "Oct-01","Nov-01","Dic-01",
                                  "Ene-01","Feb-01","Mar-01")) +
    labs(x = "Fecha",
         y = label_y,
         title = bquote(bold("Año hidrológico promedio"))) +
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
          aspect.ratio = aspect_ratio)
  
  return(fig)
}

# plot_stats_panel ----
#' Create a text panel with descriptive statistics
#'
#' @param df data.frame with Date and numeric column
#' @param ID Character: name for the series
#' @param timestep Character: "day", "month", or "year"
#' @return A ggplot object with text table
#' @keywords internal
plot_stats_panel = function(df, ID, timestep) {
  stopifnot(is.data.frame(df), ncol(df) >= 2)
  
  colnames(df) = c("Fecha", "Valor")
  df = df[!is.na(df$Valor), ]
  
  if (nrow(df) == 0) {
    # Return empty plot if no data
    return(ggplot() + theme_void() + labs(title = "Sin datos"))
  }
  
  # Calculate statistics
  stats_df = data.frame(
    Estadistica = c("Media", "Desv. Estándar", "Mínimo", "Máximo", "Mediana", "Q25", "Q75", "N"),
    Valor = c(
      round(mean(df$Valor, na.rm = TRUE), 2),
      round(stats::sd(df$Valor, na.rm = TRUE), 2),
      round(min(df$Valor, na.rm = TRUE), 2),
      round(max(df$Valor, na.rm = TRUE), 2),
      round(stats::median(df$Valor, na.rm = TRUE), 2),
      round(stats::quantile(df$Valor, 0.25, na.rm = TRUE), 2),
      round(stats::quantile(df$Valor, 0.75, na.rm = TRUE), 2),
      nrow(df)
    )
  )
  
  # Determine title
  if (timestep == "day") {
    titulo = "Estadísticas descriptivas (serie diaria)"
  } else if (timestep == "month") {
    titulo = "Estadísticas descriptivas (serie mensual)"
  } else {
    titulo = "Estadísticas descriptivas (serie anual)"
  }
  
  # Create table plot
  fig = ggplot(stats_df, aes(x = 1, y = rev(seq_along(Estadistica)))) +
    geom_text(aes(label = paste0(Estadistica, ": ", Valor)), 
              hjust = 0, size = 3.5, color = "black") +
    xlim(0.5, 2) +
    ylim(0, nrow(stats_df) + 1) +
    labs(title = titulo) +
    theme_void() +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2),
          plot.margin = margin(20, 20, 20, 20))
  
  return(fig)
}

