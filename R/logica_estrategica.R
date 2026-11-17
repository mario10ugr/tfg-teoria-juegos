# =============================================================================
# Secciones 1.4.1, 1.5 y 1.6 · Juegos en forma estratégica
# =============================================================================
# Un juego bipersonal finito se guarda como una lista con:
#   A, B      matrices m x n de pagos del Jugador 1 y del Jugador 2
#   filas     nombres de las estrategias del Jugador 1
#   columnas  nombres de las estrategias del Jugador 2
#   j1, j2    nombres de los jugadores

juego <- function(A, B, filas, columnas,
                  j1 = "Jugador 1", j2 = "Jugador 2", nombre = "") {
  list(A = A, B = B, filas = filas, columnas = columnas,
       j1 = j1, j2 = j2, nombre = nombre)
}

# Juegos de los ejemplos del TFG.
juegos_predefinidos <- list(
  pares = juego(
    matrix(c(5, -5, -5, 5), 2, byrow = TRUE),
    matrix(c(-5, 5, 5, -5), 2, byrow = TRUE),
    c("P", "N"), c("P", "N"),
    nombre = "Ejemplo 1.1 · Pares o nones"
  ),
  prisionero = juego(
    matrix(c(-1, -10, 0, -5), 2, byrow = TRUE),
    matrix(c(-1, 0, -10, -5), 2, byrow = TRUE),
    c("ND", "D"), c("ND", "D"), "Prisionero 1", "Prisionero 2",
    nombre = "Ejemplo 1.4 · Dilema del prisionero"
  ),
  canal = juego(
    matrix(c(0, 1, 1, 0), 2, byrow = TRUE),
    matrix(c(0, 1, 1, 0), 2, byrow = TRUE),
    c("Canal A", "Canal B"), c("Canal A", "Canal B"), "Usuario 1", "Usuario 2",
    nombre = "Ejemplo 1.6 · Acceso al canal"
  ),
  secuencial = juego(
    matrix(c(1, 0, 2, 2), 2, byrow = TRUE),
    matrix(c(2, 0, 1, 1), 2, byrow = TRUE),
    c("I", "D"), c("A", "B"),
    nombre = "Ejemplo 1.3 en forma estratégica"
  )
)

# Opciones para los selectores de juego (comunes a varias pestañas).
opciones_juegos <- c(
  "Ej. 1.1 · Pares o nones" = "pares",
  "Ej. 1.4 · Dilema del prisionero" = "prisionero",
  "Ej. 1.6 · Acceso al canal" = "canal",
  "Ej. 1.3 · Juego secuencial (forma estratégica)" = "secuencial",
  "Juego personalizado" = "personalizado"
)

# -----------------------------------------------------------------------------
# Lectura de juegos escritos por el usuario
# -----------------------------------------------------------------------------

# Convierte "3", "-1.5", "1/2" o "−2" en número.
parse_numero <- function(s) {
  s <- trimws(gsub("\u2212", "-", s))
  if (grepl("^-?[0-9.]+/[0-9.]+$", s)) {
    partes <- as.numeric(strsplit(sub("^-", "", s), "/")[[1]])
    signo <- if (startsWith(s, "-")) -1 else 1
    return(signo * partes[1] / partes[2])
  }
  x <- suppressWarnings(as.numeric(s))
  if (is.na(x)) stop(sprintf("«%s» no es un número válido.", s))
  x
}

# Bimatriz: una fila por línea, celdas separadas por "|" o ";",
# y en cada celda "u1, u2".
parse_bimatriz <- function(texto) {
  lineas <- trimws(strsplit(texto, "\n")[[1]])
  lineas <- lineas[nzchar(lineas)]
  if (!length(lineas)) stop("Escribe al menos una fila de pagos.")
  celdas <- lapply(lineas, function(l) trimws(strsplit(l, "[|;]")[[1]]))
  n <- unique(lengths(celdas))
  if (length(n) != 1) stop("Todas las filas deben tener el mismo número de celdas.")
  m <- length(lineas)
  A <- B <- matrix(NA_real_, m, n)
  for (i in seq_len(m)) for (j in seq_len(n)) {
    par_pagos <- strsplit(gsub("[()]", "", celdas[[i]][j]), ",")[[1]]
    if (length(par_pagos) != 2) {
      stop(sprintf("La celda (%d, %d) debe tener dos pagos separados por coma.", i, j))
    }
    A[i, j] <- parse_numero(par_pagos[1])
    B[i, j] <- parse_numero(par_pagos[2])
  }
  list(A = A, B = B)
}

# Matriz de un solo jugador (suma cero): una fila por línea.
parse_matriz <- function(texto) {
  lineas <- trimws(strsplit(texto, "\n")[[1]])
  lineas <- lineas[nzchar(lineas)]
  if (!length(lineas)) stop("Escribe al menos una fila de pagos.")
  filas <- lapply(lineas, function(l) {
    vapply(strsplit(l, "[[:space:],;|]+")[[1]], parse_numero, numeric(1))
  })
  n <- unique(lengths(filas))
  if (length(n) != 1) stop("Todas las filas deben tener el mismo número de valores.")
  do.call(rbind, filas)
}

# Nombres separados por comas. Si no hay k nombres distintos, se usan los
# nombres por defecto (prefijo1, prefijo2...).
parse_nombres <- function(texto, k, prefijo) {
  nombres <- trimws(strsplit(texto, ",")[[1]])
  nombres <- nombres[nzchar(nombres)]
  if (length(nombres) != k || anyDuplicated(nombres)) {
    nombres <- paste0(prefijo, seq_len(k))
  }
  nombres
}

# Texto de una bimatriz, para rellenar el editor a partir de un ejemplo.
bimatriz_a_texto <- function(A, B) {
  paste(vapply(seq_len(nrow(A)), function(i) {
    paste(sprintf("%s, %s", formatC(A[i, ], format = "fg"),
                  formatC(B[i, ], format = "fg")), collapse = " | ")
  }, character(1)), collapse = "\n")
}

# -----------------------------------------------------------------------------
# Mejores respuestas y equilibrios de Nash en estrategias puras (Def. 1.8, 1.9)
# -----------------------------------------------------------------------------

# br1[i, j] es TRUE si la fila i es mejor respuesta del Jugador 1 a la columna j
# (br2, lo mismo para el Jugador 2). Un perfil es equilibrio si lo es para ambos.
mejores_respuestas <- function(G, tol = 1e-9) {
  max_col <- apply(G$A, 2, max)
  max_fil <- apply(G$B, 1, max)
  br1 <- G$A >= matrix(max_col, nrow(G$A), ncol(G$A), byrow = TRUE) - tol
  br2 <- G$B >= matrix(max_fil, nrow(G$B), ncol(G$B)) - tol
  list(br1 = br1, br2 = br2, ne = br1 & br2)
}

# Tabla con los perfiles que son equilibrio y sus pagos.
equilibrios_puros <- function(G) {
  ne <- mejores_respuestas(G)$ne
  idx <- which(ne, arr.ind = TRUE)
  if (!nrow(idx)) return(data.frame())
  idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  data.frame(
    Perfil = sprintf("(%s, %s)", G$filas[idx[, 1]], G$columnas[idx[, 2]]),
    `Pago J1` = como_fraccion(G$A[idx]), `Pago J2` = como_fraccion(G$B[idx]),
    check.names = FALSE
  )
}

# -----------------------------------------------------------------------------
# Dominación (Def. 1.5, 1.6 y Proposición 1.2)
# -----------------------------------------------------------------------------
# M tiene en las filas las estrategias propias y en las columnas las del rival.

# Busca una estrategia mixta (sin usar s) que domine estrictamente a s. Devuelve
# la mezcla y el margen de dominación, o NULL si no existe.
dominada_por_mixta <- function(M, s, tol = 1e-9) {
  otras <- setdiff(seq_len(nrow(M)), s)
  if (length(otras) < 2) return(NULL)
  K <- length(otras); n <- ncol(M)
  # max eps  s.a.  sum_k sigma_k M[k, j] - eps >= M[s, j],  sum sigma = 1
  sol <- lpSolve::lp(
    "max", c(rep(0, K), 1),
    rbind(cbind(t(M[otras, , drop = FALSE]), -1), c(rep(1, K), 0)),
    c(rep(">=", n), "="), c(M[s, ], 1)
  )
  if (sol$status == 0 && sol$objval > tol) {
    sigma <- numeric(nrow(M)); sigma[otras] <- sol$solution[1:K]
    return(list(sigma = sigma, eps = sol$objval))
  }
  NULL
}

# Texto de una estrategia mixta, por ejemplo "1/2·P + 1/2·N".
describe_mixta <- function(sigma, nombres) {
  k <- which(sigma > 1e-9)
  paste(sprintf("%s·%s", como_fraccion(sigma[k]), nombres[k]), collapse = " + ")
}

# Todas las dominaciones (estrictas, débiles y por mixtas) entre las filas de M.
tabla_dominacion <- function(M, nombres, jugador, tol = 1e-9) {
  filas <- list()
  for (s in seq_len(nrow(M))) {
    estricta_pura <- FALSE
    for (s2 in setdiff(seq_len(nrow(M)), s)) {
      d <- M[s2, ] - M[s, ]
      if (all(d > tol)) {
        filas[[length(filas) + 1]] <- c(jugador, nombres[s], nombres[s2], "Estricta")
        estricta_pura <- TRUE
      } else if (all(d >= -tol) && any(d > tol)) {
        filas[[length(filas) + 1]] <- c(jugador, nombres[s], nombres[s2], "Débil")
      }
    }
    if (!estricta_pura) {
      mx <- dominada_por_mixta(M, s, tol)
      if (!is.null(mx)) {
        filas[[length(filas) + 1]] <- c(jugador, nombres[s],
                                        describe_mixta(mx$sigma, nombres),
                                        "Estricta (por una mixta)")
      }
    }
  }
  if (!length(filas)) return(NULL)
  df <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
  names(df) <- c("Jugador", "Estrategia dominada", "Dominada por", "Tipo")
  df
}

# Para el Jugador 2 se traspone B, de modo que sus estrategias queden en filas.
dominaciones <- function(G) {
  rbind(tabla_dominacion(G$A, G$filas, G$j1),
        tabla_dominacion(t(G$B), G$columnas, G$j2))
}

# Busca una estrategia (pura o mixta) que domine estrictamente a s.
dominador_estricto <- function(M, s, nombres, tol = 1e-9) {
  for (s2 in setdiff(seq_len(nrow(M)), s)) {
    if (all(M[s2, ] - M[s, ] > tol)) return(nombres[s2])
  }
  mx <- dominada_por_mixta(M, s, tol)
  if (!is.null(mx)) return(describe_mixta(mx$sigma, nombres))
  NULL
}

# Eliminación iterativa de estrategias estrictamente dominadas. En cada vuelta se
# elimina una sola estrategia (primero se mira el Jugador 1) y se repite la
# búsqueda en el juego reducido.
eliminacion_iterativa <- function(G) {
  f <- seq_len(nrow(G$A)); cc <- seq_len(ncol(G$A))
  pasos <- list()
  repeat {
    hecho <- FALSE
    M1 <- G$A[f, cc, drop = FALSE]
    if (length(f) > 1) for (ii in seq_along(f)) {
      dom <- dominador_estricto(M1, ii, G$filas[f])
      if (!is.null(dom)) {
        pasos[[length(pasos) + 1]] <- data.frame(
          Paso = length(pasos) + 1, Jugador = G$j1,
          Elimina = G$filas[f[ii]], `Dominada por` = dom, check.names = FALSE)
        f <- f[-ii]; hecho <- TRUE; break
      }
    }
    if (!hecho && length(cc) > 1) {
      M2 <- t(G$B[f, cc, drop = FALSE])
      for (jj in seq_along(cc)) {
        dom <- dominador_estricto(M2, jj, G$columnas[cc])
        if (!is.null(dom)) {
          pasos[[length(pasos) + 1]] <- data.frame(
            Paso = length(pasos) + 1, Jugador = G$j2,
            Elimina = G$columnas[cc[jj]], `Dominada por` = dom, check.names = FALSE)
          cc <- cc[-jj]; hecho <- TRUE; break
        }
      }
    }
    if (!hecho) break
  }
  list(pasos = if (length(pasos)) do.call(rbind, pasos) else NULL,
       filas = f, columnas = cc)
}

# -----------------------------------------------------------------------------
# Equilibrios en estrategias mixtas (Def. 1.11, Corolario 1.1, Teorema 1.2)
# -----------------------------------------------------------------------------
# Enumeración de soportes del mismo tamaño: en cada soporte se imponen las
# condiciones del principio de indiferencia y se comprueba con el Lema 1.1
# que ninguna estrategia pura da más. Encuentra todos los equilibrios de los
# juegos no degenerados.

equilibrios_mixtos <- function(G, tol = 1e-9) {
  A <- G$A; B <- G$B; m <- nrow(A); n <- ncol(A)
  res <- list()
  for (k in seq_len(min(m, n))) {
    for (I in combn(m, k, simplify = FALSE)) for (J in combn(n, k, simplify = FALSE)) {
      # Principio de indiferencia en el soporte: A[I, J] y = v con sum(y) = 1, y
      # t(B[I, J]) x = w con sum(x) = 1.
      My <- rbind(cbind(A[I, J, drop = FALSE], -1), c(rep(1, k), 0))
      Mx <- rbind(cbind(t(B[I, J, drop = FALSE]), -1), c(rep(1, k), 0))
      sy <- tryCatch(solve(My, c(rep(0, k), 1)), error = function(e) NULL)
      sx <- tryCatch(solve(Mx, c(rep(0, k), 1)), error = function(e) NULL)
      if (is.null(sy) || is.null(sx)) next
      y <- numeric(n); y[J] <- sy[1:k]
      x <- numeric(m); x[I] <- sx[1:k]
      if (any(x < -tol) || any(y < -tol)) next
      x <- pmax(x, 0); y <- pmax(y, 0)
      u1 <- as.vector(A %*% y); u2 <- as.vector(t(x) %*% B)
      v1 <- sum(x * u1); v2 <- sum(u2 * y)
      # Ninguna estrategia pura puede mejorar el pago (Lema 1.1).
      if (max(u1) > v1 + 1e-7 || max(u2) > v2 + 1e-7) next
      # El mismo equilibrio puede salir de soportes distintos: se guarda una vez.
      clave <- paste(round(c(x, y), 6), collapse = ",")
      if (!is.null(res[[clave]])) next
      res[[clave]] <- list(x = x, y = y, u1 = v1, u2 = v2)
    }
  }
  unname(res)
}

es_degenerado <- function(G) {
  # Un juego es degenerado si alguna estrategia pura tiene más mejores
  # respuestas puras de las que permite su soporte (caso k = 1).
  br <- mejores_respuestas(G)
  any(colSums(br$br1) > 1) || any(rowSums(br$br2) > 1)
}

# Pago esperado U_i(sigma) de la Definición 1.10.
pago_esperado <- function(M, x, y) as.numeric(t(x) %*% M %*% y)

# -----------------------------------------------------------------------------
# Juegos de suma cero (Sección 1.6)
# -----------------------------------------------------------------------------

# Valores maximin y minimax en estrategias puras, con la fila y la columna que
# los alcanzan.
valores_puros <- function(A) {
  list(maximin = max(apply(A, 1, min)), minimax = min(apply(A, 2, max)),
       fila_maximin = which.max(apply(A, 1, min)),
       col_minimax = which.min(apply(A, 2, max)))
}

# Programas lineales de la Sección 1.6.1 resueltos con lpSolve. Se suma una
# constante para que todos los pagos sean positivos (lpSolve exige variables
# no negativas, y así v > 0); al final se resta.
valor_suma_cero <- function(A) {
  m <- nrow(A); n <- ncol(A)
  c0 <- 1 - min(A)
  Ap <- A + c0
  # Jugador 1: max v  s.a.  v - sum_k x_k a_kj <= 0,  sum x = 1
  s1 <- lpSolve::lp("max", c(rep(0, m), 1),
                    rbind(cbind(-t(Ap), 1), c(rep(1, m), 0)),
                    c(rep("<=", n), "="), c(rep(0, n), 1))
  # Jugador 2: min w  s.a.  w - sum_j a_kj y_j >= 0,  sum y = 1
  s2 <- lpSolve::lp("min", c(rep(0, n), 1),
                    rbind(cbind(-Ap, 1), c(rep(1, n), 0)),
                    c(rep(">=", m), "="), c(rep(0, m), 1))
  if (s1$status != 0 || s2$status != 0) stop("El programa lineal no tiene solución.")
  list(x = s1$solution[1:m], v = s1$objval - c0,
       y = s2$solution[1:n], w = s2$objval - c0)
}
