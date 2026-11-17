# =============================================================================
# Secciones 1.4.2, 1.7, 2.3 y 2.4 · Árboles de juego
# =============================================================================
# Un nodo de decisión es list(jugador = 1 o 2, hijos = lista con nombre).
# Los nombres de 'hijos' son las acciones. Una hoja es list(pagos = c(u1, u2)).
# En los juegos de suma cero el Jugador 1 es MAX, el 2 es MIN y basta con el
# pago de MAX (pagos[1]).

nodo <- function(jugador, hijos, infoset = NA) {
  list(jugador = jugador, hijos = hijos, infoset = infoset)
}
hoja <- function(...) list(pagos = c(...))
es_hoja <- function(nd) is.null(nd$hijos)

# Asigna a cada nodo un id único y un nombre legible (su ruta de acciones).
numerar_arbol <- function(raiz) {
  contador <- 0
  rec <- function(nd, ruta) {
    contador <<- contador + 1
    nd$id <- paste0("n", contador)
    nd$ruta <- ruta
    if (!es_hoja(nd)) {
      for (a in names(nd$hijos)) {
        nd$hijos[[a]] <- rec(nd$hijos[[a]],
                             if (ruta == "raíz") a else paste0(ruta, "\u00b7", a))
      }
    }
    nd
  }
  rec(raiz, "raíz")
}

# Convierte el árbol en una tabla con posiciones para dibujarlo. Las hojas van en
# x = 1, 2, 3... y cada nodo interno, centrado sobre sus hijos; y = -profundidad.
aplanar_arbol <- function(raiz, suma_cero = FALSE) {
  filas <- list(); x_hoja <- 0
  rec <- function(nd, padre, accion, prof) {
    if (es_hoja(nd)) {
      x_hoja <<- x_hoja + 1
      x <- x_hoja
      etiqueta <- if (suma_cero) como_fraccion(nd$pagos[1]) else fmt_vector(nd$pagos)
    } else {
      xs <- vapply(names(nd$hijos), function(a) rec(nd$hijos[[a]], nd$id, a, prof + 1),
                   numeric(1))
      x <- mean(range(xs))
      etiqueta <- ""
    }
    filas[[length(filas) + 1]] <<- data.frame(
      id = nd$id, padre = padre, accion = accion, ruta = nd$ruta, x = x, y = -prof,
      es_hoja = es_hoja(nd),
      jugador = if (es_hoja(nd)) NA_integer_ else as.integer(nd$jugador),
      infoset = if (es_hoja(nd) || is.null(nd$infoset)) NA_character_ else as.character(nd$infoset),
      etiqueta_hoja = etiqueta, stringsAsFactors = FALSE)
    x
  }
  rec(raiz, NA_character_, "", 0)
  do.call(rbind, filas)
}

# Ids de los nodos agrupados por conjunto de información (solo grupos de 2 o más).
conjuntos_de_informacion <- function(df) {
  df <- df[!is.na(df$infoset), ]
  grupos <- split(df$id, df$infoset)
  grupos[lengths(grupos) > 1]
}

# Ids de todos los nodos del subárbol que cuelga de nd (incluido nd).
ids_subarbol <- function(nd) {
  if (es_hoja(nd)) return(nd$id)
  c(nd$id, unlist(lapply(nd$hijos, ids_subarbol), use.names = FALSE))
}

# -----------------------------------------------------------------------------
# Ejemplos del TFG
# -----------------------------------------------------------------------------

# Ejemplo 1.2: pares o nones en forma extensiva (J2 no observa a J1).
arbol_pares_nones <- function() {
  numerar_arbol(nodo(1, list(
    P = nodo(2, list(P = hoja(5, -5), N = hoja(-5, 5)), infoset = "h2"),
    N = nodo(2, list(P = hoja(-5, 5), N = hoja(5, -5)), infoset = "h2")
  )))
}

# Ejemplo 1.3: juego secuencial de información perfecta.
arbol_secuencial <- function(pIA = c(1, 2), pIB = c(0, 0), pD = c(2, 1)) {
  numerar_arbol(nodo(1, list(
    I = nodo(2, list(A = do.call(hoja, as.list(pIA)), B = do.call(hoja, as.list(pIB)))),
    D = do.call(hoja, as.list(pD))
  )))
}

# Árbol completo con factor de ramificación b, profundidad m y hojas dadas.
# Acciones: A, B, C... en el primer nivel; 1, 2, 3... en el segundo; a, b, c...
# en el tercero; y así alternando.
arbol_completo <- function(b, m, hojas) {
  stopifnot(length(hojas) == b^m)
  etiquetas <- function(nivel) {
    switch((nivel - 1) %% 3 + 1, LETTERS[1:b], as.character(1:b), letters[1:b])
  }
  i <- 0
  rec <- function(prof) {
    if (prof == m) { i <<- i + 1; return(hoja(hojas[i])) }
    hijos <- lapply(seq_len(b), function(k) rec(prof + 1))
    names(hijos) <- etiquetas(prof + 1)
    nodo(if (prof %% 2 == 0) 1 else 2, hijos)
  }
  numerar_arbol(rec(0))
}

# Árbol aleatorio de información perfecta y suma general (Teorema 1.4).
arbol_aleatorio <- function(b, m, semilla = 1) {
  set.seed(semilla)
  etiquetas <- c("I", "C", "D")[seq_len(b)]
  if (b == 2) etiquetas <- c("I", "D")
  rec <- function(prof, jugador) {
    if (prof == m || (prof >= 1 && runif(1) < 0.15)) {
      return(hoja(sample(0:9, 1), sample(0:9, 1)))
    }
    hijos <- lapply(seq_len(b), function(k) rec(prof + 1, 3 - jugador))
    names(hijos) <- etiquetas
    nodo(jugador, hijos)
  }
  numerar_arbol(rec(0, 1))
}

# -----------------------------------------------------------------------------
# Inducción hacia atrás (Sección 1.7)
# -----------------------------------------------------------------------------
induccion_hacia_atras <- function(raiz) {
  eleccion <- character(0); valor <- list(); pasos <- list()
  rec <- function(nd) {
    if (es_hoja(nd)) return(nd$pagos)
    vals <- lapply(nd$hijos, rec)
    i <- nd$jugador
    u <- vapply(vals, `[`, numeric(1), i)
    # Si hay empate, se queda con la primera acción.
    mejores <- which(u >= max(u) - 1e-9)
    k <- mejores[1]
    eleccion[nd$id] <<- names(nd$hijos)[k]
    valor[[nd$id]] <<- vals[[k]]
    pasos[[length(pasos) + 1]] <<- data.frame(
      id = nd$id, jugador = i, accion = names(nd$hijos)[k],
      Paso = length(pasos) + 1,
      Nodo = nd$ruta,
      Decide = paste0("Jugador ", i),
      Opciones = paste(sprintf("%s \u2192 %s", names(nd$hijos),
                               vapply(vals, fmt_vector, character(1))), collapse = ";  "),
      Elige = if (length(mejores) > 1) {
        paste0(names(nd$hijos)[k], " (empate con ",
               paste(names(nd$hijos)[mejores[-1]], collapse = ", "), ")")
      } else names(nd$hijos)[k],
      check.names = FALSE, stringsAsFactors = FALSE)
    vals[[k]]
  }
  pagos <- rec(raiz)
  list(pagos = pagos, eleccion = eleccion, valor = valor,
       pasos = do.call(rbind, pasos))
}

# Ids de los hijos elegidos en cada nodo de decisión (todo el plan) y del
# camino que realmente se recorre.
aristas_elegidas <- function(raiz, eleccion) {
  plan <- character(0); camino <- character(0)
  rec <- function(nd, en_camino) {
    if (es_hoja(nd)) return(invisible())
    a <- eleccion[nd$id]
    if (is.na(a)) a <- ""   # nodo no visitado (por ejemplo, podado)
    for (acc in names(nd$hijos)) {
      h <- nd$hijos[[acc]]
      if (acc == a) {
        plan <<- c(plan, h$id)
        if (en_camino) camino <<- c(camino, h$id)
      }
      rec(h, en_camino && acc == a)
    }
  }
  rec(raiz, TRUE)
  list(plan = plan, camino = camino)
}

# -----------------------------------------------------------------------------
# Minimax (Algoritmo 2.1)
# -----------------------------------------------------------------------------
minimax_arbol <- function(raiz) {
  valor <- numeric(0); eleccion <- character(0); visitados <- 0; hojas <- 0
  orden <- character(0)
  rec <- function(nd) {
    visitados <<- visitados + 1
    orden <<- c(orden, nd$ruta)
    if (es_hoja(nd)) {
      hojas <<- hojas + 1
      valor[nd$id] <<- nd$pagos[1]
      return(nd$pagos[1])
    }
    vs <- vapply(nd$hijos, rec, numeric(1))
    k <- if (nd$jugador == 1) which.max(vs) else which.min(vs)
    valor[nd$id] <<- vs[k]
    eleccion[nd$id] <<- names(nd$hijos)[k]
    vs[k]
  }
  v <- rec(raiz)
  list(v = v, valor = valor, eleccion = eleccion, visitados = visitados,
       hojas = hojas, orden = orden)
}

# -----------------------------------------------------------------------------
# Poda alfa-beta con traza paso a paso (Algoritmo 2.2)
# -----------------------------------------------------------------------------
alfabeta_arbol <- function(raiz) {
  valor <- numeric(0); eleccion <- character(0); cota <- character(0)
  podados <- character(0); raices_podadas <- character(0)
  visitados <- 0; hojas <- 0; traza <- list()
  anota <- function(nd, alfa, beta, que) {
    traza[[length(traza) + 1]] <<- data.frame(
      Paso = length(traza) + 1, Nodo = nd$ruta,
      Tipo = if (es_hoja(nd)) "hoja" else if (nd$jugador == 1) "MAX" else "MIN",
      alfa = fmt(alfa), beta = fmt(beta), Evento = que,
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  rec <- function(nd, alfa, beta) {
    visitados <<- visitados + 1
    if (es_hoja(nd)) {
      hojas <<- hojas + 1
      v <- nd$pagos[1]
      valor[nd$id] <<- v
      anota(nd, alfa, beta, paste0("Evalúa la hoja: U = ", fmt(v)))
      return(v)
    }
    es_max <- nd$jugador == 1
    anota(nd, alfa, beta, "Entra en el nodo")
    v <- if (es_max) -Inf else Inf
    acciones <- names(nd$hijos)
    for (k in seq_along(acciones)) {
      g <- rec(nd$hijos[[k]], alfa, beta)
      if ((es_max && g > v) || (!es_max && g < v)) {
        v <- g; eleccion[nd$id] <<- acciones[k]
      }
      if (es_max) alfa <- max(alfa, v) else beta <- min(beta, v)
      if (alfa >= beta && k < length(acciones)) {
        resto <- nd$hijos[(k + 1):length(acciones)]
        raices_podadas <<- c(raices_podadas, vapply(resto, `[[`, "", "id"))
        podados <<- c(podados, unlist(lapply(resto, ids_subarbol), use.names = FALSE))
        anota(nd, alfa, beta, sprintf(
          "Poda %s: \u03b1 = %s \u2265 \u03b2 = %s; no se exploran %s",
          if (es_max) "\u03b2" else "\u03b1", fmt(alfa), fmt(beta),
          paste(vapply(resto, `[[`, "", "ruta"), collapse = ", ")))
        break
      }
    }
    valor[nd$id] <<- v
    # Lema 2.1: si se ha alcanzado alfa >= beta, v es solo una cota.
    if (alfa >= beta) cota[nd$id] <<- if (es_max) "\u2265" else "\u2264"
    anota(nd, alfa, beta, paste0("Devuelve v = ", fmt(v)))
    v
  }
  v <- rec(raiz, -Inf, Inf)
  list(v = v, valor = valor, eleccion = eleccion, cota = cota, podados = podados,
       raices_podadas = raices_podadas, visitados = visitados, hojas = hojas,
       traza = setNames(do.call(rbind, traza),
                        c("Paso", "Nodo", "Tipo", "\u03b1", "\u03b2", "Evento")))
}

# Texto "v = 3" o "v ≤ 3" para cada nodo de decisión.
etiquetas_valor <- function(df, valor, cota = character(0)) {
  ids <- df$id[!df$es_hoja & df$id %in% names(valor)]
  et <- setNames(rep(NA_character_, nrow(df)), df$id)
  for (id in ids) {
    signo <- if (id %in% names(cota)) cota[[id]] else "="
    et[id] <- paste("v", signo, como_fraccion(valor[[id]]))
  }
  et
}

# -----------------------------------------------------------------------------
# Experimento del Teorema 2.1 (Knuth y Moore)
# -----------------------------------------------------------------------------
# El árbol completo se guarda por niveles: vals[[d + 1]] contiene los valores
# minimax de los b^d nodos del nivel d, de modo que los hijos del nodo k del
# nivel d son los nodos (k - 1) * b + 1:b del nivel d + 1.
hojas_evaluadas_alfabeta <- function(b, m, orden = c("mejor", "natural", "peor"),
                                     semilla = 1) {
  orden <- match.arg(orden)
  set.seed(semilla)
  vals <- vector("list", m + 1)
  vals[[m + 1]] <- runif(b^m)
  for (d in (m - 1):0) {
    mat <- matrix(vals[[d + 2]], nrow = b)
    vals[[d + 1]] <- if (d %% 2 == 0) apply(mat, 2, max) else apply(mat, 2, min)
  }
  hojas <- 0
  rec <- function(d, k, alfa, beta) {
    if (d == m) { hojas <<- hojas + 1; return(vals[[m + 1]][k]) }
    hijos <- (k - 1) * b + seq_len(b)
    es_max <- d %% 2 == 0
    if (orden != "natural") {
      clave <- vals[[d + 2]][hijos]
      mejor_primero <- if (es_max) order(-clave) else order(clave)
      hijos <- hijos[if (orden == "mejor") mejor_primero else rev(mejor_primero)]
    }
    v <- if (es_max) -Inf else Inf
    for (h in hijos) {
      g <- rec(d + 1, h, alfa, beta)
      if (es_max) { v <- max(v, g); alfa <- max(alfa, v) }
      else { v <- min(v, g); beta <- min(beta, v) }
      if (alfa >= beta) break
    }
    v
  }
  v <- rec(0, 1, -Inf, Inf)
  stopifnot(abs(v - vals[[1]]) < 1e-12)  # la poda no cambia el valor
  hojas
}

# Hojas que evalúa alfa-beta con el orden perfecto (Teorema 2.1).
knuth_moore <- function(b, m) b^ceiling(m / 2) + b^floor(m / 2) - 1

# -----------------------------------------------------------------------------
# Registros paso a paso para las animaciones
# -----------------------------------------------------------------------------

# Minimax: un paso por cada nodo de decisión, en el orden en que se resuelve
# (de abajo arriba, de izquierda a derecha).
minimax_eventos <- function(raiz) {
  ev <- list(); valor <- numeric(0); eleccion <- character(0)
  rec <- function(nd) {
    if (es_hoja(nd)) { valor[nd$id] <<- nd$pagos[1]; return(nd$pagos[1]) }
    vs <- vapply(nd$hijos, rec, numeric(1))
    k <- if (nd$jugador == 1) which.max(vs) else which.min(vs)
    valor[nd$id] <<- vs[k]; eleccion[nd$id] <<- names(nd$hijos)[k]
    ev[[length(ev) + 1]] <<- list(id = nd$id, jugador = nd$jugador, valores = vs,
                                  acciones = names(nd$hijos), elegida = names(nd$hijos)[k],
                                  hijo = nd$hijos[[k]]$id, v = vs[k])
    vs[k]
  }
  v <- rec(raiz)
  list(v = v, eventos = ev, valor = valor, eleccion = eleccion)
}

# Alfa-beta: un evento por cada acción del algoritmo (entrar en un nodo,
# mirar una hoja, podar o devolver un valor), con los valores de α y β.
alfabeta_eventos <- function(raiz) {
  ev <- list(); hojas <- 0
  anota <- function(...) ev[[length(ev) + 1]] <<- list(...)
  rec <- function(nd, alfa, beta, padre_es_max) {
    if (es_hoja(nd)) {
      hojas <<- hojas + 1
      anota(tipo = "hoja", id = nd$id, ruta = nd$ruta, alfa = alfa, beta = beta,
            v = nd$pagos[1], hojas = hojas)
      return(nd$pagos[1])
    }
    es_max <- nd$jugador == 1
    anota(tipo = "entra", id = nd$id, ruta = nd$ruta, es_max = es_max, alfa = alfa,
          beta = beta, hojas = hojas)
    v <- if (es_max) -Inf else Inf
    elegida <- NA_character_; cota <- FALSE
    for (k in seq_along(nd$hijos)) {
      g <- rec(nd$hijos[[k]], alfa, beta, es_max)
      if ((es_max && g > v) || (!es_max && g < v)) { v <- g; elegida <- nd$hijos[[k]]$id }
      if (es_max) alfa <- max(alfa, v) else beta <- min(beta, v)
      if (alfa >= beta) {
        cota <- TRUE
        if (k < length(nd$hijos)) {
          resto <- nd$hijos[(k + 1):length(nd$hijos)]
          anota(tipo = "poda", id = nd$id, ruta = nd$ruta, es_max = es_max, alfa = alfa,
                beta = beta, v = v, hojas = hojas,
                raices = vapply(resto, `[[`, "", "id"),
                podados = unlist(lapply(resto, ids_subarbol), use.names = FALSE))
        }
        break
      }
    }
    anota(tipo = "devuelve", id = nd$id, ruta = nd$ruta, es_max = es_max, alfa = alfa,
          beta = beta, v = v, cota = cota, elegida = elegida, hojas = hojas)
    v
  }
  v <- rec(raiz, -Inf, Inf, NA)
  list(v = v, eventos = ev, hojas = hojas)
}

# Estado del árbol tras los k primeros eventos de alfabeta_eventos().
estado_alfabeta <- function(eventos, k) {
  valor <- character(0); visitados <- character(0); podados <- character(0)
  raices_podadas <- character(0); elegidas <- character(0); activo <- NULL
  for (e in eventos[seq_len(k)]) {
    visitados <- union(visitados, e$id); activo <- e$id
    if (e$tipo == "hoja") valor[e$id] <- como_fraccion(e$v)
    if (e$tipo == "poda") { podados <- union(podados, e$podados); raices_podadas <- union(raices_podadas, e$raices) }
    if (e$tipo == "devuelve") {
      valor[e$id] <- paste0(if (e$cota) (if (e$es_max) "\u2265 " else "\u2264 ") else "", como_fraccion(e$v))
      if (!is.na(e$elegida)) elegidas <- union(elegidas, e$elegida)
    }
  }
  list(valor = valor, visitados = visitados, podados = podados,
       raices_podadas = raices_podadas, elegidas = elegidas, activo = activo,
       evento = if (k > 0) eventos[[k]] else NULL)
}
