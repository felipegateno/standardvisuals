#!/usr/bin/env Rscript

# Prueba simple de tema desde style_tokens.json (ggplot2)

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) ""
)
if (is.na(script_dir) || script_dir == "") {
  script_dir <- getwd()
}

repo_root <- normalizePath(file.path(script_dir, ".."))
source(file.path(repo_root, "rstandarvisuals", "R", "create_theme_from_tokens.R"))

cat("Leyendo datos de ts_pr_daily.csv...\n")
df <- read.csv(file.path(script_dir, "ts_pr_daily.csv"), stringsAsFactors = FALSE)
df$Fecha <- as.Date(df$Fecha, format = "%d-%m-%Y")

df <- df %>% rename(A = Estacion_mm)
df$B <- df$A * 0.9
df$C <- df$A * 1.1
df$D <- df$A * 1.0

otros <- df %>%
  select(Fecha, A, B, C, D) %>%
  pivot_longer(cols = -Fecha, names_to = "Serie", values_to = "Valor")

cat("Generando gráfico de prueba...\n")
p <- ggplot(otros, aes(x = Fecha, y = Valor)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~Serie, ncol = 1, scales = "free_y") +
  labs(
    title = "Otros cauces",
    x = NULL,
    y = "Caudal (m3/s)"
  ) +
  create_theme_from_tokens(file.path(repo_root, "style_tokens.json"))

dir.create(file.path(script_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(script_dir, "figures", "r_test_tokens_simple.png")

ggsave(
  filename = out_path,
  plot = p,
  width = 20,
  height = length(unique(otros$Serie)) * 2,
  units = "in",
  dpi = 300
)

cat("Gráfico guardado en:", out_path, "\n")

