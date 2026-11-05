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

datos <- read.csv(here("data", "petfinder_enriquecido_final.csv")) |>
  dplyr::mutate(
    AdoptionSpeed = factor(
      AdoptionSpeed,
      levels = c("SameDay", "1-7d", "8-30d", "31-90d", "NoAdoption"),
      labels = c("Mismo día", "1–7 días", "8–30 días", "31–90 días", "Sin adopción")
    ),
    Gender = factor(
      Gender,
      levels = c("Male", "Female", "Mixed"),
      labels = c("Macho", "Hembra", "Mixto")
    ),
    Vaccinated = factor(
      Vaccinated,
      levels = c("Yes", "No", "NotSure"),
      labels = c("Sí", "No", "No está seguro")
    )
  )

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

# Función pval(): Formatea valores p con el umbral habitual
pval <- function(p, d = 3) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0,001")
  fmt(p, d)
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

# ===========================================================================
# Análisis de correspondencias simple (ACS)
# ===========================================================================
# `acs()` recibe una tabla de contingencia y devuelve las coordenadas,
# contribuciones e inercia de filas y columnas. El eje 1 se orienta para que
# la categoría de fila con mayor coordenada absoluta quede del lado positivo;
# así los planos de distintas variables se pueden comparar entre sí.
acs <- function(tabla) {
  tabla <- as.matrix(tabla)
  n <- sum(tabla)
  P <- tabla / n
  r <- rowSums(P)
  c <- colSums(P)

  S <- diag(1 / sqrt(r)) %*% (P - outer(r, c)) %*% diag(1 / sqrt(c))
  des <- svd(S)
  k <- sum(des$d > 1e-10)
  d <- des$d[seq_len(k)]
  U <- des$u[, seq_len(k), drop = FALSE]
  V <- des$v[, seq_len(k), drop = FALSE]

  F <- diag(1 / sqrt(r)) %*% U %*% diag(d, nrow = k)
  G <- diag(1 / sqrt(c)) %*% V %*% diag(d, nrow = k)

  # Orientación estable de los ejes
  if (F[which.max(abs(F[, 1])), 1] < 0) {
    F[, 1] <- -F[, 1]; G[, 1] <- -G[, 1]
  }
  if (k >= 2 && F[which.max(abs(F[, 2])), 2] < 0) {
    F[, 2] <- -F[, 2]; G[, 2] <- -G[, 2]
  }

  eig <- d^2
  pct <- 100 * eig / sum(eig)

  filas <- tibble::tibble(
    categoria = rownames(tabla),
    masa = r,
    dim1 = F[, 1],
    dim2 = if (k >= 2) F[, 2] else 0,
    contrib1 = 100 * r * F[, 1]^2 / eig[1],
    contrib2 = if (k >= 2) 100 * r * F[, 2]^2 / eig[2] else 0
  )

  columnas <- tibble::tibble(
    categoria = colnames(tabla),
    masa = c,
    dim1 = G[, 1],
    dim2 = if (k >= 2) G[, 2] else 0,
    contrib1 = 100 * c * G[, 1]^2 / eig[1],
    contrib2 = if (k >= 2) 100 * c * G[, 2]^2 / eig[2] else 0
  )

  chi <- suppressWarnings(chisq.test(tabla))

  list(
    tabla = tabla,
    n = n,
    chi = chi,
    cramer = unname(sqrt(chi$statistic / (n * (min(dim(tabla)) - 1)))),
    inercia = sum(eig),
    eig = eig,
    pct = pct,
    filas = filas,
    columnas = columnas
  )
}

# Tabla con la inercia explicada por cada dimensión del ACS
tabla_acs <- function(res, caption = NULL) {
  tibble::tibble(
    Dimensión = seq_along(res$eig),
    `Valor propio` = fmt(res$eig, 4),
    `% de inercia` = pct(res$pct, 1),
    `% acumulado` = pct(cumsum(res$pct), 1)
  ) |>
    knitr::kable(align = c("c", "r", "r", "r"), caption = caption)
}

# Biplot del ACS: filas y columnas en el mismo plano
grafico_acs <- function(res, titulo, etiqueta_filas = "AdoptionSpeed",
                        etiqueta_columnas = "Variable") {
  d <- dplyr::bind_rows(
    dplyr::mutate(res$filas, grupo = etiqueta_filas),
    dplyr::mutate(res$columnas, grupo = etiqueta_columnas)
  ) |>
    dplyr::mutate(grupo = factor(grupo, levels = c(etiqueta_filas, etiqueta_columnas)))

  ggplot(d, aes(x = dim1, y = dim2, colour = grupo, shape = grupo)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey75") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey75") +
    geom_segment(aes(xend = dim1, yend = dim2), x = 0, y = 0,
                 alpha = 0.45,
                 arrow = grid::arrow(length = grid::unit(0.15, "cm"))) +
    geom_point(size = 3) +
    ggrepel::geom_text_repel(aes(label = categoria), size = 3,
                             max.overlaps = Inf, seed = 1, show.legend = FALSE) +
    scale_colour_manual(values = c("#1a3a5c", "#d97706")) +
    scale_shape_manual(values = c(16, 17)) +
    scale_x_continuous(labels = function(x) format(x, decimal.mark = ",", trim = TRUE)) +
    scale_y_continuous(labels = function(x) format(x, decimal.mark = ",", trim = TRUE)) +
    labs(
      title = titulo,
      x = paste0("Dimensión 1 (", pct(res$pct[1]), ")"),
      y = paste0("Dimensión 2 (", pct(res$pct[2]), ")"),
      colour = NULL, shape = NULL
    ) +
    tema_u() +
    theme(legend.position = "bottom")
}
