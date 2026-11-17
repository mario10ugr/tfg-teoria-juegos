# =============================================================================
# Dibujos en SVG: matrices de pagos con flechas y árboles de juego
# =============================================================================

# Escapa el texto antes de insertarlo en el SVG.
esc <- function(x) htmltools::htmlEscape(as.character(x))

# Icono de Font Awesome como trazado dentro de un SVG, centrado en (x, y).
svg_icono <- function(nombre, x, y, tam, color) {
  svg <- as.character(fontawesome::fa(nombre))
  vb <- as.numeric(strsplit(sub('.*viewBox="([^"]+)".*', "\\1", svg), " ")[[1]])
  d <- sub('.*<path d="([^"]+)".*', "\\1", svg)
  k <- tam / max(vb[3], vb[4])
  sprintf('<path d="%s" fill="%s" transform="translate(%g %g) scale(%g)"/>',
          d, color, x - vb[3] * k / 2, y - vb[4] * k / 2, k)
}

# Cada dibujo usa identificadores propios para las puntas de flecha: si se
# repitieran, el navegador usaría las de otra página oculta y no se verían.
svg_flechas_defs <- function(sufijo) {
  paste0(
    '<defs>',
    sprintf('<marker id="punta-j1-%s" viewBox="0 0 10 10" refX="5" refY="5" markerWidth="4" markerHeight="4" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="%s"/></marker>', sufijo, COL_J1),
    sprintf('<marker id="punta-j2-%s" viewBox="0 0 10 10" refX="5" refY="5" markerWidth="4" markerHeight="4" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="%s"/></marker>', sufijo, COL_J2),
    '</defs>')
}

# -----------------------------------------------------------------------------
# Matriz de pagos
# -----------------------------------------------------------------------------
# G             juego (A, B, filas, columnas, j1, j2)
# textos1/2     matrices de texto opcionales para los pagos (p. ej. "5 años")
# flechas_j1    columnas en las que se dibuja la flecha del Jugador 1 (solo 2 × 2)
# flechas_j2    filas en las que se dibuja la flecha del Jugador 2 (solo 2 × 2)
# estrellas     marca los equilibrios de Nash en estrategias puras
# atenuadas     matriz lógica m × n: casillas que se dibujan apagadas
# marcos        lista de list(i, j, color, discontinuo): casillas recuadradas
# chips         lista de list(i, j, jugador): pagos que se rodean para compararlos
# solo_j1       en juegos de suma cero, muestra solo el pago del Jugador 1
# corto1/2      etiqueta corta bajo cada pago ("J1", "tú"...)
svg_matriz <- function(G, textos1 = NULL, textos2 = NULL,
                       flechas_j1 = integer(0), flechas_j2 = integer(0),
                       estrellas = FALSE, atenuadas = NULL, marcos = list(),
                       chips = list(), solo_j1 = FALSE,
                       corto1 = "J1", corto2 = "J2") {
  m <- nrow(G$A); n <- ncol(G$A)
  if (is.null(textos1)) textos1 <- matrix(como_fraccion(G$A), m, n)
  if (is.null(textos2)) textos2 <- matrix(como_fraccion(G$B), m, n)
  if (is.null(atenuadas)) atenuadas <- matrix(FALSE, m, n)
  ne <- if (estrellas) mejores_respuestas(G)$ne else matrix(FALSE, m, n)
  es22 <- m == 2 && n == 2
  sufijo <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")

  # Medidas en unidades del viewBox: casilla W × H, separación GX y GY entre
  # casillas y márgenes izquierdo (L) y superior (T).
  W <- 196; H <- 104; GX <- if (es22) 58 else 18; GY <- if (es22) 50 else 16
  L <- 150; T <- 86
  ancho <- L + n * W + (n - 1) * GX + 16
  alto <- T + m * H + (m - 1) * GY + 16
  cx <- function(j) L + (j - 1) * (W + GX)
  cy <- function(i) T + (i - 1) * (H + GY)
  chipw <- if (solo_j1) W - 40 else W / 2 - 22

  out <- c(sprintf('<svg class="svg-matriz" viewBox="0 0 %d %d" role="img" aria-label="Matriz de pagos">',
                   ancho, alto), svg_flechas_defs(sufijo))
  # Nombres de los jugadores y estrategias
  out <- c(out, sprintf('<text x="%g" y="26" text-anchor="middle" class="m-jugador" fill="%s">%s</text>',
                        L + (n * W + (n - 1) * GX) / 2, COL_J2, esc(G$j2)))
  for (j in seq_len(n)) {
    out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" class="m-estrategia" fill="%s">%s</text>',
                          cx(j) + W / 2, T - 16, COL_J2, esc(G$columnas[j])))
  }
  out <- c(out, sprintf('<text x="22" y="%g" text-anchor="middle" class="m-jugador" fill="%s" transform="rotate(-90 22 %g)">%s</text>',
                        T + (m * H + (m - 1) * GY) / 2, COL_J1, T + (m * H + (m - 1) * GY) / 2, esc(G$j1)))
  for (i in seq_len(m)) {
    out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="end" dominant-baseline="middle" class="m-estrategia" fill="%s">%s</text>',
                          L - 14, cy(i) + H / 2, COL_J1, esc(G$filas[i])))
  }
  # Casillas
  for (i in seq_len(m)) for (j in seq_len(n)) {
    x <- cx(j); y <- cy(i)
    op <- if (atenuadas[i, j]) 0.25 else 1
    fondo <- if (ne[i, j]) COL_EQ_SU else "#FFFFFF"
    borde <- if (ne[i, j]) COL_EQ else "#D5D9DF"
    out <- c(out, sprintf('<g opacity="%g"><rect x="%g" y="%g" width="%g" height="%g" rx="4" fill="%s" stroke="%s" stroke-width="%g"/>',
                          op, x, y, W, H, fondo, borde, if (ne[i, j]) 3 else 1.5))
    # Recuadro con un pago, en el color del jugador.
    chip <- function(xc, txt, col, colsu, quien, etiqueta) {
      rodeado <- any(vapply(chips, function(ch) ch$i == i && ch$j == j && ch$jugador == quien, logical(1)))
      tam <- if (nchar(txt) > 6) 17 else if (nchar(txt) > 3) 21 else 28
      c(sprintf('<rect x="%g" y="%g" width="%g" height="56" rx="3" fill="%s" stroke="%s" stroke-width="%g"/>',
                xc, y + 16, chipw, colsu, col, if (rodeado) 4 else 0),
        sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" class="m-pago" font-size="%d" fill="%s">%s</text>',
                xc + chipw / 2, y + 44, tam, col, esc(txt)),
        sprintf('<text x="%g" y="%g" text-anchor="middle" class="m-quien">%s</text>',
                xc + chipw / 2, y + H - 12, esc(etiqueta)))
    }
    out <- c(out, chip(x + 14, textos1[i, j], COL_J1, COL_J1_SU, 1, corto1))
    if (!solo_j1) out <- c(out, chip(x + W / 2 + 8, textos2[i, j], COL_J2, COL_J2_SU, 2, corto2))
    if (ne[i, j]) {
      out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" font-size="26" fill="%s">\u2605</text>',
                            x + W - 2, y + 2, COL_EQ))
    }
    out <- c(out, "</g>")
  }
  for (mc in marcos) {
    out <- c(out, sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="6" fill="none" stroke="%s" stroke-width="5" stroke-dasharray="%s"/>',
                          cx(mc$j) - 5, cy(mc$i) - 5, W + 10, H + 10, mc$color,
                          if (isTRUE(mc$discontinuo)) "10 6" else "none"))
  }
  # Flechas (solo en juegos 2 × 2)
  if (es22) {
    for (j in flechas_j1) {
      xa <- cx(j) + 14 + chipw / 2
      y1 <- cy(1) + H + 7; y2 <- cy(2) - 7
      d <- G$A[2, j] - G$A[1, j]
      if (abs(d) < 1e-9) {
        out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" font-size="26" fill="%s">=</text>', xa, (y1 + y2) / 2, COL_J1))
      } else {
        if (d < 0) { tmp <- y1; y1 <- y2; y2 <- tmp }
        out <- c(out, sprintf('<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="6" stroke-linecap="round" marker-end="url(#punta-j1-%s)"/>',
                              xa, y1, xa, y2 + (if (d > 0) -8 else 8), COL_J1, sufijo))
      }
    }
    for (i in flechas_j2) {
      ya <- cy(i) + 44
      x1 <- cx(1) + W + 7; x2 <- cx(2) - 7
      d <- G$B[i, 2] - G$B[i, 1]
      if (abs(d) < 1e-9) {
        out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" font-size="26" fill="%s">=</text>', (x1 + x2) / 2, ya, COL_J2))
      } else {
        if (d < 0) { tmp <- x1; x1 <- x2; x2 <- tmp }
        out <- c(out, sprintf('<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="6" stroke-linecap="round" marker-end="url(#punta-j2-%s)"/>',
                              x1, ya, x2 + (if (d > 0) -8 else 8), ya, COL_J2, sufijo))
      }
    }
  }
  out <- c(out, "</svg>")
  div(class = "dibujo", HTML(paste(out, collapse = "")))
}

# -----------------------------------------------------------------------------
# Árbol de juego
# -----------------------------------------------------------------------------
# df          resultado de aplanar_arbol()
# suma_cero   TRUE dibuja MAX (triángulo arriba) y MIN (triángulo abajo)
# elegidas    ids de los hijos cuya arista se resalta (decisión tomada)
# podadas     ids de nodos que no se exploran (rama discontinua y tijeras)
# atenuadas   ids de nodos que se dibujan apagados
# pendientes  ids de nodos todavía no visitados (gris claro)
# activo      id del nodo que se está mirando ahora
# valores     vector con nombre id -> texto que aparece en una burbuja
# bocadillo   list(id, texto): bocadillo de diálogo junto a un nodo
# conjuntos   lista de vectores de ids en el mismo conjunto de información
# nombres     nombres de los jugadores dentro de los círculos
# amenazas    ids de nodos cuya arista se dibuja como amenaza (granate discontinua)
# ux, uy      separación horizontal entre hojas y vertical entre niveles
svg_arbol <- function(df, suma_cero = FALSE, elegidas = character(0),
                      podadas = character(0), atenuadas = character(0),
                      pendientes = character(0), activo = NULL, valores = NULL,
                      bocadillo = NULL, conjuntos = list(), amenazas = character(0),
                      nombres = c("J1", "J2"), ux = 130, uy = 118) {
  n_hojas <- sum(df$es_hoja)
  pad <- 70
  prof <- max(-df$y)
  ancho <- 2 * pad + (n_hojas - 1) * ux
  alto <- 70 + prof * uy + 70
  px <- function(x) pad + (x - 1) * ux
  py <- function(y) 48 + (-y) * uy
  pos <- function(id) df[match(id, df$id), ]
  estado_op <- function(id) {
    if (id %in% podadas) 0.35 else if (id %in% atenuadas) 0.22 else if (id %in% pendientes) 0.45 else 1
  }

  out <- c(sprintf('<svg class="svg-arbol" viewBox="0 0 %g %g" style="max-width:%gpx" role="img" aria-label="Árbol de juego">',
                   ancho, alto, ancho * 1.35))
  tijeras <- character(0)
  # Aristas
  for (r in which(!is.na(df$padre))) {
    h <- df[r, ]; p <- pos(h$padre)
    x1 <- px(p$x); y1 <- py(p$y); x2 <- px(h$x); y2 <- py(h$y)
    podada <- h$id %in% podadas; elegida <- h$id %in% elegidas; amenaza <- h$id %in% amenazas
    col <- if (podada) COL_GRIS else if (amenaza) COL_J2 else if (elegida) COL_EQ else "#8A919C"
    out <- c(out, sprintf('<g opacity="%g"><line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="%g" stroke-linecap="round" %s/>',
                          estado_op(h$id), x1, y1, x2, y2, col, if (elegida || amenaza) 7 else 2.5,
                          if (podada) 'stroke-dasharray="8 7"' else if (amenaza) 'stroke-dasharray="12 9"' else ""))
    if (nzchar(h$accion)) {
      # La etiqueta se coloca hacia fuera de la arista para no chocar con la de su hermana.
      mx <- (x1 + x2) / 2; my <- (y1 + y2) / 2
      w <- 14 + 8.6 * nchar(h$accion)
      lado <- sign(round(x2 - x1))
      cxl <- if (lado < 0) mx - w / 2 - 6 else if (lado > 0) mx + w / 2 + 6 else mx + w / 2 + 8
      out <- c(out, sprintf('<rect x="%g" y="%g" width="%g" height="24" rx="2" fill="white" stroke="%s" stroke-width="1.5"/>',
                            cxl - w / 2, my - 12, w, if (elegida) COL_EQ else "#D5D9DF"),
               sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" class="a-accion" %s>%s</text>',
                       cxl, my, if (elegida) 'font-weight="700"' else "", esc(h$accion)))
    }
    if (podada && !(h$padre %in% podadas)) {
      # Las tijeras cortan la rama; se dibujan al final, por encima de todo y sin atenuar.
      tx <- x1 + 0.55 * (x2 - x1); ty <- y1 + 0.55 * (y2 - y1)
      tijeras <- c(tijeras, sprintf('<circle cx="%g" cy="%g" r="18" fill="white" stroke="%s" stroke-width="2"/>', tx, ty, COL_MAL),
                   svg_icono("scissors", tx, ty, 20, COL_MAL))
    }
    out <- c(out, "</g>")
  }
  # Conjuntos de información
  for (ci in conjuntos) {
    pts <- pos(ci); pts <- pts[order(pts$x), ]
    out <- c(out, sprintf('<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="3" stroke-dasharray="10 8"/>',
                          px(pts$x[1]), py(pts$y[1]), px(pts$x[nrow(pts)]), py(pts$y[1]), COL_J2),
             sprintf('<text x="%g" y="%g" text-anchor="middle" class="a-nota" fill="%s">no sabe en cuál está</text>',
                     (px(pts$x[1]) + px(pts$x[nrow(pts)])) / 2, py(pts$y[1]) + 30, COL_J2))
  }
  # Nodos
  for (r in seq_len(nrow(df))) {
    nd <- df[r, ]; x <- px(nd$x); y <- py(nd$y)
    out <- c(out, sprintf('<g opacity="%g">', estado_op(nd$id)))
    if (!is.null(activo) && nd$id == activo) {
      out <- c(out, sprintf('<circle cx="%g" cy="%g" r="38" fill="%s" class="pulso"/>', x, y, COL_EQ_SU),
               sprintf('<circle cx="%g" cy="%g" r="38" fill="none" stroke="%s" stroke-width="3"/>', x, y, COL_EQ))
    }
    if (nd$es_hoja) {
      txt <- nd$etiqueta_hoja
      w <- max(min(52, ux - 8), 13 * nchar(txt) + 18)
      out <- c(out, sprintf('<rect x="%g" y="%g" width="%g" height="36" rx="3" fill="#F3F4F6" stroke="#BFC5CE" stroke-width="1.5"/>',
                            x - w / 2, y - 18, w))
      if (suma_cero) {
        out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" class="a-hoja">%s</text>', x, y, esc(txt)))
      } else {
        p <- strsplit(gsub("[()]", "", txt), ", |; ")[[1]]
        out <- c(out, sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" class="a-hoja">(<tspan fill="%s">%s</tspan>, <tspan fill="%s">%s</tspan>)</text>',
                              x, y, COL_J1, esc(p[1]), COL_J2, esc(p[2])))
      }
    } else {
      col <- if (nd$jugador == 1) COL_J1 else COL_J2
      if (suma_cero) {
        pts <- if (nd$jugador == 1) c(x, y - 30, x - 33, y + 22, x + 33, y + 22)
               else c(x, y + 30, x - 33, y - 22, x + 33, y - 22)
        out <- c(out, sprintf('<polygon points="%s" fill="%s" stroke="white" stroke-width="2"/>',
                              paste(pts, collapse = ","), col),
                 sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" class="a-nodo a-nodo-sc">%s</text>',
                         x, y + if (nd$jugador == 1) 7 else -7, if (nd$jugador == 1) "MAX" else "MIN"))
      } else {
        out <- c(out, sprintf('<circle cx="%g" cy="%g" r="25" fill="%s" stroke="white" stroke-width="2"/>', x, y, col),
                 sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" class="a-nodo">%s</text>',
                         x, y, esc(nombres[nd$jugador])))
      }
    }
    # Burbuja con el valor: debajo de las hojas y a la derecha de los nodos.
    if (!is.null(valores) && !is.na(valores[nd$id])) {
      txt <- valores[[nd$id]]
      col <- if (nd$es_hoja) COL_TINTA else if (nd$jugador == 1) COL_J1 else COL_J2
      w <- 16 + 11 * nchar(txt)
      bx <- if (nd$es_hoja) x - w / 2 else x + if (suma_cero) 38 else 32
      by <- if (nd$es_hoja) y + 24 else y - 14
      out <- c(out, sprintf('<rect x="%g" y="%g" width="%g" height="28" rx="3" fill="%s"/>', bx, by, w, col),
               sprintf('<text x="%g" y="%g" text-anchor="middle" dominant-baseline="central" class="a-valor">%s</text>',
                       bx + w / 2, by + 14, esc(txt)))
    }
    out <- c(out, "</g>")
  }
  out <- c(out, tijeras)
  # Bocadillo de diálogo, por encima y a la izquierda del nodo.
  if (!is.null(bocadillo)) {
    nd <- pos(bocadillo$id); x <- px(nd$x); y <- py(nd$y)
    lineas <- strwrap(bocadillo$texto, 30)
    w <- 22 + 9.2 * max(nchar(lineas)); h <- 18 + 22 * length(lineas)
    bx <- max(4, min(ancho - w - 4, x - w - 40)); by <- max(4, y - h - 30)
    out <- c(out, sprintf('<path d="M%g,%g L%g,%g L%g,%g z" fill="white" stroke="%s" stroke-width="2"/>',
                          bx + w - 30, by + h - 2, x - 22, y - 16, bx + w - 10, by + h - 2, COL_J2),
             sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="14" fill="white" stroke="%s" stroke-width="2"/>',
                     bx, by, w, h, COL_J2),
             sprintf('<rect x="%g" y="%g" width="24" height="6" fill="white"/>', bx + w - 32, by + h - 3))
    for (k in seq_along(lineas)) {
      out <- c(out, sprintf('<text x="%g" y="%g" class="a-bocadillo">%s</text>', bx + 11, by + 8 + 22 * k, esc(lineas[k])))
    }
  }
  out <- c(out, "</svg>")
  div(class = "dibujo", HTML(paste(out, collapse = "")))
}
