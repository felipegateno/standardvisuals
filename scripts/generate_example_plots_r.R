library(ggplot2)
library(dplyr)
library(tidyr)

repo_root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
assets_dir <- file.path(repo_root, "assets")

# Cargar funciones directamente desde el código fuente local
source(file.path(repo_root, "rstandarvisuals", "R", "colors.R"))
source(file.path(repo_root, "rstandarvisuals", "R", "create_theme_from_tokens.R"))

colors_path <- file.path(repo_root, "colors.json")
tokens_path <- file.path(repo_root, "style_tokens.json")

# ============================================================
# 1) Generar serie mensual de temperatura (5 años)
# ============================================================

fechas <- seq(
  as.Date("2019-01-01"),
  by = "month",
  length.out = 60
)

temp_base <- c(
  22, 22, 21, 18, 15, 12,
  11, 12, 14, 17, 19, 21
)

temp_est_1 <- rep(temp_base, 5)

data <- data.frame(
  date = fechas,
  Estacion_1 = temp_est_1,
  Estacion_2 = temp_est_1 + 2,
  Estacion_3 = temp_est_1 - 2,
  Estacion_4 = temp_est_1 + 4
)

# ============================================================
# 2) Gráfico de 2 estaciones
# ============================================================

pal_obs_sim <- get_palette("standardcolors_obsvssim", colors_path = colors_path)

data_2 <- data %>%
  select(date, Estacion_1, Estacion_2) %>%
  pivot_longer(
    -date,
    names_to = "station",
    values_to = "temperature"
  )

p1 <- ggplot(data_2, aes(date, temperature, color = station)) +
  geom_line() +
  scale_color_manual(values = unname(pal_obs_sim)) +
  scale_x_date(breaks = "1 year",
               expand = c(0,0)) +
  labs(
    title = "Imagen creada en R: Temperatura mensual sintética – comparación de 2 estaciones",
    x = "Fecha",
    y = "Temperatura mensual [°C]"
  ) +
  create_theme_from_tokens(tokens_path = tokens_path) +
  theme(
    legend.position = c(0.93, 0.88),
    legend.title = element_blank(),
    legend.background = element_rect(color = "black")
  )

ggsave(
  filename = file.path(assets_dir, "p1_R.png"),
  plot = p1,
  width = 12,
  height = 4,
  dpi = 200,
  units = "in"
)

# ============================================================
# 3) Gráfico de las 4 estaciones
# ============================================================

pal_div <- get_palette("standardcolors_divergent", n = 4, colors_path = colors_path)

data_4 <- data %>%
  pivot_longer(
    -date,
    names_to = "station",
    values_to = "temperature"
  )

p2 <- ggplot(data_4, aes(date, temperature, color = station)) +
  geom_line() +
  scale_color_manual(values = unname(pal_div)) +
  labs(
    title = "Imagen creada en R: Temperatura mensual sintética – 4 estaciones",
    x = "Fecha",
    y = "Temperatura mensual [°C]"
  ) +
  scale_x_date(expand = c(0,0)) +
  create_theme_from_tokens(tokens_path = tokens_path) +
  theme(
    legend.position = c(0.05, 0.2),
    legend.title = element_blank(),
    legend.background = element_rect(color = "black")
  )

ggsave(
  filename = file.path(assets_dir, "p2_R.png"),
  plot = p2,
  width = 12,
  height = 4,
  dpi = 200,
  units = "in"
)
