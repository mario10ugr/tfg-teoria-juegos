# =============================================================================
# Secciones 2.2, 2.5 y Ejemplo 2.3 · Tres en raya (y k en raya en n x n)
# =============================================================================
# El tablero es un vector de longitud n*n leído por filas: 0 = vacía,
# 1 = X (empieza) y -1 = O. Los valores siempre se dan desde el punto de vista
# de la IA, que actúa como MAX.

# Precalcula todas las líneas de k casillas (filas, columnas y diagonales) y,
# para cada celda, las líneas que pasan por ella.
kr_config <- function(n = 3, k = 3) {
  idx <- matrix(seq_len(n * n), n, n, byrow = TRUE)
  lineas <- list()
  for (f in seq_len(n)) for (c in seq_len(n)) {
    if (c + k - 1 <= n) lineas[[length(lineas) + 1]] <- idx[f, c:(c + k - 1)]
    if (f + k - 1 <= n) lineas[[length(lineas) + 1]] <- idx[f:(f + k - 1), c]
    if (f + k - 1 <= n && c + k - 1 <= n)
      lineas[[length(lineas) + 1]] <- idx[cbind(f:(f + k - 1), c:(c + k - 1))]
    if (f + k - 1 <= n && c - k + 1 >= 1)
      lineas[[length(lineas) + 1]] <- idx[cbind(f:(f + k - 1), c:(c - k + 1))]
  }
  L <- do.call(rbind, lineas)
  por_celda <- lapply(seq_len(n * n), function(i) L[rowSums(L == i) > 0, , drop = FALSE])
  list(n = n, k = k, lineas = L, nL = nrow(L), por_celda = por_celda)
}

# Comprueba si p ha ganado al poner su ficha en la celda c. Solo mira las líneas
# que pasan por c.
kr_gana <- function(tablero, c, p, cfg) {
  sub <- cfg$por_celda[[c]]
  if (!nrow(sub)) return(FALSE)
  any(rowSums(matrix(tablero[sub] == p, nrow = nrow(sub))) == cfg$k)
}

# Ganador del tablero: 1 (X), -1 (O) o 0 si nadie ha hecho línea.
kr_ganador <- function(tablero, cfg) {
  s <- rowSums(matrix(tablero[cfg$lineas], nrow = cfg$nL))
  if (any(s == cfg$k)) return(1L)
  if (any(s == -cfg$k)) return(-1L)
  0L
}

# Casillas de la línea ganadora, para resaltarla en el tablero.
kr_linea_ganadora <- function(tablero, cfg) {
  s <- rowSums(matrix(tablero[cfg$lineas], nrow = cfg$nL))
  r <- which(abs(s) == cfg$k)
  if (length(r)) cfg$lineas[r[1], ] else integer(0)
}

# Líneas abiertas del Ejemplo 2.3: líneas que el rival todavía no ha bloqueado.
kr_lineas_abiertas <- function(tablero, p_max, cfg) {
  M <- matrix(tablero[cfg$lineas], nrow = cfg$nL) * p_max
  c_max <- rowSums(M == 1); c_min <- rowSums(M == -1)
  list(abiertas_max = c_min == 0, abiertas_min = c_max == 0,
       c_max = c_max, c_min = c_min)
}

# h(s) = L_MAX(s) - L_MIN(s)  (simple)  o con pesos 10^(fichas)  (ponderada).
kr_heuristica <- function(tablero, p_max, cfg, tipo = "ponderada") {
  la <- kr_lineas_abiertas(tablero, p_max, cfg)
  if (tipo == "simple") {
    sum(la$abiertas_max) - sum(la$abiertas_min)
  } else {
    sum(10^la$c_max[la$abiertas_max]) - sum(10^la$c_min[la$abiertas_min])
  }
}

# Valor de un estado (Algoritmos 2.1, 2.2 y 2.3 en una sola función).
#   d     profundidad restante (Inf = hasta los estados terminales)
#   ctx   entorno con la configuración y los contadores
kr_valor <- function(tablero, turno, d, alfa, beta, ultimo, ctx) {
  ctx$nodos <- ctx$nodos + 1
  if (ctx$nodos > ctx$max_nodos) stop("limite_nodos")
  if (!is.na(ultimo)) {
    p <- tablero[ultimo]
    if (kr_gana(tablero, ultimo, p, ctx$cfg)) {
      ctx$terminales <- ctx$terminales + 1
      return(if (p == ctx$p_max) ctx$W else -ctx$W)
    }
  }
  libres <- which(tablero == 0)
  if (!length(libres)) { ctx$terminales <- ctx$terminales + 1; return(0) }
  if (d == 0) {
    ctx$heuristicas <- ctx$heuristicas + 1
    return(kr_heuristica(tablero, ctx$p_max, ctx$cfg, ctx$tipo_h))
  }
  es_max <- turno == ctx$p_max
  if (ctx$ordenar) libres <- kr_ordenar(tablero, libres, turno, es_max, ctx)
  v <- if (es_max) -Inf else Inf
  for (c in libres) {
    tablero[c] <- turno
    g <- kr_valor(tablero, -turno, d - 1, alfa, beta, c, ctx)
    tablero[c] <- 0
    if (es_max) {
      if (g > v) v <- g
      if (ctx$poda) { alfa <- max(alfa, v); if (alfa >= beta) { ctx$podas <- ctx$podas + 1; break } }
    } else {
      if (g < v) v <- g
      if (ctx$poda) { beta <- min(beta, v); if (alfa >= beta) { ctx$podas <- ctx$podas + 1; break } }
    }
  }
  v
}

# Ordena las jugadas por el valor heurístico del estado resultante, para que
# la poda alfa-beta encuentre antes los mejores movimientos (Teorema 2.1).
kr_ordenar <- function(tablero, libres, turno, es_max, ctx) {
  h <- vapply(libres, function(c) {
    tablero[c] <- turno
    kr_heuristica(tablero, ctx$p_max, ctx$cfg, "ponderada")
  }, numeric(1))
  libres[order(if (es_max) -h else h)]
}

# Decide la jugada de la IA desde la raíz (nodo MAX).
#   metodo  "minimax" (sin poda) o "alfabeta"
#   prof    profundidad máxima (Inf = búsqueda completa)
kr_decidir <- function(tablero, p_ia, cfg, metodo = "alfabeta", prof = Inf,
                       tipo_h = "ponderada", ordenar = FALSE, max_nodos = 2e6,
                       raiz_exacta = FALSE) {
  ctx <- new.env()
  ctx$cfg <- cfg; ctx$p_max <- p_ia; ctx$poda <- metodo == "alfabeta"
  ctx$tipo_h <- tipo_h; ctx$ordenar <- ordenar; ctx$max_nodos <- max_nodos
  # Valor de una victoria: 1 si se busca hasta el final; si no, un número muy
  # grande para que pese más que cualquier valor heurístico.
  ctx$W <- if (is.infinite(prof)) 1 else 1e6
  ctx$nodos <- 1; ctx$terminales <- 0; ctx$heuristicas <- 0; ctx$podas <- 0
  libres <- which(tablero == 0)
  if (ordenar) libres <- kr_ordenar(tablero, libres, p_ia, TRUE, ctx)
  # Las victorias inmediatas se examinan primero: así, entre jugadas con el
  # mismo valor, la IA no retrasa una victoria que ya tiene.
  gana_ya <- vapply(libres, function(c) {
    t2 <- tablero; t2[c] <- p_ia; kr_gana(t2, c, p_ia, cfg)
  }, logical(1))
  libres <- c(libres[gana_ya], libres[!gana_ya])
  alfa <- -Inf; mejor <- NA; v_mejor <- -Inf
  res <- data.frame(celda = libres, valor = NA_real_, exacto = TRUE)
  t0 <- Sys.time()
  # Si se supera max_nodos, kr_valor() lanza "limite_nodos" y la búsqueda se
  # interrumpe (ok = FALSE).
  ok <- tryCatch({
    for (r in seq_along(libres)) {
      c <- libres[r]
      tablero[c] <- p_ia
      g <- kr_valor(tablero, -p_ia, prof - 1, if (raiz_exacta) -Inf else alfa, Inf, c, ctx)
      tablero[c] <- 0
      res$valor[r] <- g
      # Lema 2.1: si g <= alfa, solo sabemos que el valor real es <= g.
      res$exacto[r] <- raiz_exacta || !(ctx$poda && g <= alfa)
      if (g > v_mejor) { v_mejor <- g; mejor <- c }
      if (ctx$poda) alfa <- max(alfa, g)
    }
    TRUE
  }, error = function(e) {
    if (identical(conditionMessage(e), "limite_nodos")) FALSE else stop(e)
  })
  list(ok = ok, jugada = mejor, valor = v_mejor, tabla = res,
       nodos = ctx$nodos, terminales = ctx$terminales,
       heuristicas = ctx$heuristicas, podas = ctx$podas,
       segundos = as.numeric(difftime(Sys.time(), t0, units = "secs")),
       W = ctx$W)
}

# Texto con el valor de una jugada, para la interfaz.
kr_texto_valor <- function(v, W, exacto = TRUE) {
  pref <- if (exacto) "" else "\u2264 "
  if (W > 1 && abs(v) >= W) return(paste0(pref, if (v > 0) "gana la IA" else "pierde la IA"))
  if (W == 1) {
    return(paste0(pref, como_fraccion(v), " (",
                  c("pierde la IA", "tablas", "gana la IA")[sign(v) + 2], ")"))
  }
  paste0(pref, fmt(v, 1))
}

kr_nombre_celda <- function(c, n) sprintf("(%d, %d)", (c - 1) %/% n + 1, (c - 1) %% n + 1)
