# =============================================================================
# Colores y formato de números (en español: coma decimal, punto de miles)
# =============================================================================

# Paleta común. Los sufijos _SU son las versiones suaves, para fondos.
COL_J1    <- "#1D4E89"   # Jugador 1 / MAX (azul)
COL_J1_SU <- "#E7EDF5"
COL_J2    <- "#A6192E"   # Jugador 2 / MIN (granate UGR)
COL_J2_SU <- "#F5E6E8"
COL_EQ    <- "#C98A00"   # equilibrio / decisión elegida (ocre)
COL_EQ_SU <- "#FAF0D4"
COL_OK    <- "#2D6A4F"   # bien / gana (verde botella)
COL_OK_SU <- "#E3EFE8"
COL_MAL   <- "#B42318"   # mal / pierde
COL_GRIS  <- "#A0A6AF"
COL_TINTA <- "#16213A"
COL_CANAL_A <- "#127475"
COL_CANAL_B <- "#6C4AA0"

# Sustituye el guion por el signo menos tipográfico (−).
menos <- function(txt) gsub("-", "\u2212", txt)

# Número con coma decimal (máximo d decimales, sin ceros sobrantes).
fmt <- function(x, d = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    if (is.infinite(v)) return(if (v > 0) "+\u221e" else "\u2212\u221e")
    txt <- formatC(round(v, d), format = "f", digits = d, decimal.mark = ",")
    if (grepl(",", txt)) txt <- sub(",$", "", sub("0+$", "", txt))
    if (txt == "-0") txt <- "0"
    menos(txt)
  }, character(1))
}

# Enteros grandes con punto de miles: 18297 -> "18.297".
miles <- function(x) {
  vapply(x, function(v) menos(formatC(round(v), format = "d", big.mark = ".", decimal.mark = ",")), character(1))
}

# Tiempos, porcentajes y euros con el mismo formato.
tiempo_txt <- function(s) if (s < 0.01) "menos de 0,01 s" else paste(fmt(s, 2), "s")

pct <- function(x, d = 0) paste0(fmt(100 * x, d), "\u00a0%")

euros <- function(x) paste0(fmt(x), "\u00a0€")

# Fracciones sencillas cuando las hay (1/2, 2/3...); si no, decimales.
como_fraccion <- function(x, max_den = 24, tol = 1e-9) {
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    if (is.infinite(v)) return(if (v > 0) "+\u221e" else "\u2212\u221e")
    if (abs(v - round(v)) < tol) return(menos(format(round(v))))
    for (d in 2:max_den) {
      if (abs(v * d - round(v * d)) < tol * d) {
        return(menos(sprintf("%d/%d", as.integer(round(v * d)), d)))
      }
    }
    fmt(v, 3)
  }, character(1))
}

# Vector entre paréntesis. Si algún número lleva coma decimal, se separa con
# punto y coma para que no haya ambigüedad: (0,5; 1).
fmt_vector <- function(x) {
  txt <- como_fraccion(x)
  paste0("(", paste(txt, collapse = if (any(grepl(",", txt))) "; " else ", "), ")")
}
# Alias para los vectores de pagos.
fmt_pagos <- fmt_vector
