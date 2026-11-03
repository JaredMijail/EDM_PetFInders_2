# ===========================================================================
# Configuración común del sitio
# ---------------------------------------------------------------------------
# Este archivo se carga desde el primer bloque de cada página con:
#
#   source(here::here("_setup.R"))
#
# Aquí viven la carga de datos, los ayudantes de formateo y los gráficos que
# comparten todas las secciones. Lo específico de cada análisis va en su .qmd.
# ===========================================================================

library(here)
library(tidyverse)

datos <- read.csv(here("data", "petfinder_enriquecido_final.csv"))

# --- Formateo de cifras en español ---

ent <- function(x) {
  format(as.integer(x), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

# Función fmt(): Formatea números decimales (por defecto a 2 decimales)
fmt <- function(x, d = 2) {
  format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = ".", scientific = FALSE, trim = TRUE)
}

# Función pct(): Formatea valores como porcentaje
pct <- function(x, d = 1) {
  paste0(format(round(x, d), nsmall = d, decimal.mark = ",", scientific = FALSE, trim = TRUE), "%")
}

# --- Estadísticos ---
moda      <- function(x) { x <- x[!is.na(x)]; ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] }
asimetria <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x); n/((n-1)*(n-2))*sum(((x-m)/s)^3) }
curtosis  <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x)
n*(n+1)/((n-1)*(n-2)*(n-3))*sum(((x-m)/s)^4) - 3*(n-1)^2/((n-2)*(n-3)) }

esc_num <- function(x) {
  x <- as.numeric(x); v <- x[!is.na(x)]; q <- quantile(v, c(.25, .75)); iqr <- diff(q)
  z <- (v - mean(v)) / sd(v); li <- q[1] - 1.5*iqr; ls <- q[2] + 1.5*iqr
  list(n = length(v), na = sum(is.na(x)), media = mean(v), mediana = median(v), moda = as.numeric(moda(v)),
       min = min(v), max = max(v), q1 = q[1], q3 = q[2], iqr = iqr, rango = max(v) - min(v),
       var = var(v), sd = sd(v), cv = 100*sd(v)/mean(v), asim = asimetria(v), curt = curtosis(v),
       out_iqr = sum(v < li | v > ls), pct_out_iqr = 100*mean(v < li | v > ls),
       out_z = sum(abs(z) > 3), pct_out_z = 100*mean(abs(z) > 3), pct_ceros = 100*mean(v == 0),
       unicos = length(unique(v)))
}

# --- Tablas ---

tabla_estadisticos <- function(x, caption = NULL) {
  s <- esc_num(x)

  tab <- tibble::tibble(
    `Estadístico` = c("Entradas válidas", "Valores faltantes", "Media", "Mediana", "Moda",
                      "Mínimo", "Máximo", "Q1", "Q3",
                      "IQR", "Rango", "Varianza", "Desviación estándar",
                      "Coeficiente de variación", "Asimetría", "Curtosis"),
    Valor = c(ent(s$n), ent(s$na), fmt(s$media), fmt(s$mediana), fmt(s$moda), fmt(s$min),
              fmt(s$max), fmt(s$q1), fmt(s$q3), fmt(s$iqr), fmt(s$rango), fmt(s$var),
              fmt(s$sd), pct(s$cv), fmt(s$asim, 3), fmt(s$curt, 3))
  )

  izq <- tab[1:8, ]
  der <- tab[9:16, ]

  knitr::kables(list(
    knitr::kable(izq, align = c("l", "r")),
    knitr::kable(der, align = c("l", "r"))
  ),
  caption = caption)
}

tabla_atipicos <- function(x, caption = NULL) {
  v <- as.numeric(x)
  v <- v[!is.na(v)]
  n <- length(v)

  q   <- unname(quantile(v, c(.25, .75)))
  riq <- q[2] - q[1]

  # Multiplicadores de las cercas de Tukey
  c_tukey <- c("Atípicos (1,5 × RIQ)" = 1.5,
               "Extremos (3 × RIQ)"   = 3)

  li   <- unname(q[1] - c_tukey * riq)
  ls   <- unname(q[2] + c_tukey * riq)
  n_at <- vapply(seq_along(c_tukey),
                 function(i) sum(v < li[i] | v > ls[i]),
                 numeric(1))

  tab <- tibble::tibble(
    Criterio          = names(c_tukey),
    `Límite inferior` = fmt(li),
    `Límite superior` = fmt(ls),
    Atípicos          = ent(n_at),
    `% del total`     = pct(100 * n_at / n)
  )

  knitr::kable(
    tab,
    format   = if (knitr::is_latex_output()) "latex" else "pipe",
    booktabs = TRUE,
    linesep  = "",
    align    = c("l", "r", "r", "r", "r"),
    caption  = caption,
    escape   = FALSE
  )
}

# --- Gráficos ---

tema_u <- function() {
  theme_minimal(base_size = 11.5) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(colour = "#1a3a5c", face = "bold", size = 11.5),
          axis.title = element_text(colour = "#4b5563", size = 10),
          panel.grid.minor = element_blank())
}

graficos_cuanti <- function(x, etiqueta, tipo = c("continuo", "conteo")) {
  tipo <- match.arg(tipo)
  d <- tibble::tibble(v = as.numeric(x))
  p1 <- ggplot(d, aes(v)) +
    geom_histogram(bins = 30, fill = "#2e7d9a", colour = "white", linewidth = 0.2) +
    labs(title = "Histograma", x = etiqueta, y = "Frecuencia") + tema_u()
  p2 <- ggplot(d, aes(y = v)) +
    stat_boxplot(geom = "errorbar", width = 0.15) +
    geom_boxplot(
      fill = "#d97706",
      width = 0.32,
      outlier.shape = 1,
      outlier.colour = "black",
      outlier.alpha = 1,
      outlier.size = 1.5
    ) +
    labs(title = "Diagrama de caja", x = NULL, y = etiqueta) +
    tema_u() +
    theme(
      axis.text.x = element_blank(),
      aspect.ratio = 1.8
    )
  p3 <- ggplot(d, aes(v)) +
    geom_density(fill = "#0f766e", alpha = 0.35, colour = "#0f766e") +
    labs(title = "Densidad", x = etiqueta, y = "Densidad") + tema_u()
  p4 <- if (tipo == "continuo") {
    ggplot(d, aes(sample = v)) +
      stat_qq(colour = "#1a3a5c", alpha = 0.4, size = 0.8) +
      stat_qq_line(colour = "#d97706") +
      labs(title = "QQ-plot (referencia normal)", x = "Cuantiles teóricos", y = "Cuantiles muestrales") + tema_u()
  } else {
    d |> count(v) |> slice_max(n, n = 8) |> mutate(v = factor(v)) |>
      ggplot(aes(x = v, y = n)) + geom_col(fill = "#1a3a5c") +
      labs(title = "Valores más frecuentes", x = etiqueta, y = "Frecuencia") + tema_u()
  }
  for (p in list(p1, p2, p3, p4)) print(p)
  invisible(NULL)
}

# --- Tablas y gráficos para variables cualitativas ---

tabla_frecuencias <- function(x, etiqueta = "Categoría", n_top = 10) {
  v <- as.character(x); tb <- sort(table(v), decreasing = TRUE); n <- length(tb); tot <- sum(tb)
  if (n > n_top) {
    tab <- tibble::tibble(
      Categoría = c(names(tb)[1:(n_top - 1)], sprintf("Otras (%d categorías)", n - n_top + 1)),
      Frecuencia = c(as.integer(tb[1:(n_top - 1)]), tot - sum(tb[1:(n_top - 1)])))
  } else {
    tab <- tibble::tibble(Categoría = names(tb), Frecuencia = as.integer(tb))
  }
  tab |>
    mutate(`Frecuencia relativa` = 100*Frecuencia/sum(Frecuencia),
           `Frecuencia acumulada` = cumsum(`Frecuencia relativa`)) |>
    mutate(Frecuencia = ent(Frecuencia),
           `Frecuencia relativa` = pct(`Frecuencia relativa`),
           `Frecuencia acumulada` = pct(`Frecuencia acumulada`)) |>
    knitr::kable(col.names = c(etiqueta, "Frecuencia absoluta", "Frecuencia relativa",
                               "Frecuencia acumulada"), align = c("l", "r", "r", "r"))
}

graficos_cuali <- function(x, etiqueta, pastel = FALSE, n_top = 10) {
  v <- as.character(x); tb <- sort(table(v), decreasing = TRUE); n <- length(tb); tot <- sum(tb)
  d <- if (n > n_top) {
    tibble::tibble(cat = c(names(tb)[1:(n_top - 1)], sprintf("Otras (%d)", n - n_top + 1)),
                   n = c(as.integer(tb[1:(n_top - 1)]), tot - sum(tb[1:(n_top - 1)])))
  } else tibble::tibble(cat = names(tb), n = as.integer(tb))
  d <- d |> mutate(cat = factor(cat, levels = rev(cat)),
                   pct = 100*n/sum(n))
  p1 <- ggplot(d, aes(x = pct, y = cat)) +
    geom_col(fill = "#2e7d9a", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f %%", pct)), hjust = -0.15, size = 3, colour = "#1f2937") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
    labs(title = "Distribución de frecuencias", x = "Porcentaje de anuncios", y = etiqueta) + tema_u()
  if (!pastel) { print(p1); return(invisible(NULL)) }
  p2 <- ggplot(d, aes(x = "", y = n, fill = cat)) +
    geom_col(width = 1, colour = "white") + coord_polar("y") +
    labs(title = "Composición porcentual", x = NULL, y = NULL) +
    theme_minimal(base_size = 11.5) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(colour = "#1a3a5c", face = "bold", size = 11.5),
          legend.title = element_blank(), legend.text = element_text(size = 9),
          legend.position = "right", axis.text = element_blank(), panel.grid = element_blank())
  print(p1); print(p2)
  invisible(NULL)
}
