# =============================================================================
# Tres en raya contra la IA
# =============================================================================

# Niveles de la IA: profundidad de búsqueda, heurística y si ordena las jugadas.
NIVELES <- list(
  facil     = list(nombre = "Fácil", prof = 1, h = "simple", ordenar = FALSE,
                   desc = "Solo mira su propia jugada y la puntúa contando líneas abiertas."),
  medio     = list(nombre = "Normal", prof = 2, h = "ponderada", ordenar = FALSE,
                   desc = "Mira su jugada y tu respuesta: ya no se deja ganar a la primera."),
  dificil   = list(nombre = "Difícil", prof = 4, h = "ponderada", ordenar = TRUE,
                   desc = "Mira cuatro jugadas por delante."),
  imbatible = list(nombre = "Imbatible", prof = Inf, h = "ponderada", ordenar = TRUE,
                   desc = "Mira la partida entera hasta el final (en tableros grandes, cuatro o cinco jugadas).")
)

# Color de una casilla del mapa, de rojo (mala) a verde (buena). Los resultados
# seguros (ganar, empatar o perder) usan colores fijos.
color_valor <- function(v, W, todos) {
  if (W == 1 || abs(v) >= W) return(if (v > 0) COL_OK else if (v < 0) COL_MAL else "#C9A227")
  normales <- todos[abs(todos) < W]
  r <- if (length(unique(normales)) > 1) (v - min(normales)) / (max(normales) - min(normales)) else 0.5
  grDevices::rgb(grDevices::colorRamp(c(COL_MAL, "#C9A227", COL_OK))(r), maxColorValue = 255)
}

# Texto de cada casilla del mapa.
texto_celda <- function(v, W) {
  if (W == 1) return(c("pierde", "empata", "gana")[sign(v) + 2])
  if (abs(v) >= W) return(if (v > 0) "gana" else "pierde")
  fmt(v, 0)
}

pag_tres_en_raya_ui <- function(id) {
  ns <- NS(id)
  pagina("tresenraya",
    "Juega contra la máquina. Después de cada jugada verás qué ha pensado: qué casillas le parecían buenas, cuáles malas y cuántas posiciones ha imaginado.",

    nav_panel("1. Juega",
      panel_columnas(proporciones = c(0.8, 1.35, 1),
        tagList(
          selectInput(ns("tablero"), "Tablero", c(
            "Clásico: 3 × 3, tres en raya" = "3-3", "4 × 4, tres en raya" = "4-3",
            "4 × 4, cuatro en raya" = "4-4", "5 × 5, cuatro en raya" = "5-4")),
          radioButtons(ns("humano"), "Juegas con", c("X (empiezas tú)" = "1", "O (empieza la IA)" = "-1")),
          selectInput(ns("nivel"), "Nivel de la IA", c("Fácil" = "facil", "Normal" = "medio", "Difícil" = "dificil",
                                                       "Imbatible" = "imbatible"), selected = "medio"),
          uiOutput(ns("nivel_desc")),
          actionButton(ns("nueva"), "Nueva partida", icon = icon("rotate-left"), class = "btn-primary w-100"),
          actionButton(ns("pista"), "Pedir una pista", icon = icon("lightbulb"), class = "btn-outline-primary w-100")
        ),
        div(class = "tablero-zona", uiOutput(ns("estado")), uiOutput(ns("tablero_ui"), class = "salida-tablero"),
            div(class = "leyenda", "El recuadro gris marca la última jugada de la IA.")),
        uiOutput(ns("pensamiento"))
      )),

    nav_panel("2. ¿Cómo juzga la IA el tablero?",
      panel(
        div(class = "pila-centrada", uiOutput(ns("lineas"))),
        tagList(
          explicacion("Contando líneas abiertas",
            p("Una línea (fila, columna o diagonal) está ", negrita("abierta"), " para un jugador si el rival no tiene ninguna ficha en ella: todavía podría completarla."),
            p("La IA cuenta sus líneas abiertas y le resta las tuyas. Cuanto más alto, mejor para ella."),
            p("Desde el nivel Normal, las líneas con más fichas propias cuentan muchísimo más (10, 100, 1.000…), para que una amenaza de ganar pese más que muchas líneas vacías.")),
          idea_clave("Mirar toda la partida es imposible en juegos grandes: la IA mira unas pocas jugadas y estima el resto. ",
                     "Si mira poco, cae en trampas que quedan más allá de su «horizonte». Prueba a ganarle en Fácil con una doble amenaza.")
        ))),

    nav_panel("3. ¿Cuánto le cuesta pensar?",
      panel(
        div(class = "pila-centrada", uiOutput(ns("comparacion"))),
        tagList(
          explicacion("Mismo tablero, distintos métodos",
            p("Se calcula la jugada de quien tenga el turno en la partida actual con cada algoritmo y se cuentan las posiciones que imagina."),
            p("Juega unas jugadas en la pestaña 1 y vuelve para comparar.")),
          actionButton(ns("comparar"), "Comparar en esta posición", icon = icon("stopwatch"), class = "btn-primary")
        ))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Los tres métodos"),
            tags$ul(
              tags$li(negrita("Minimax"), ": recorre todo el árbol. En el 3 × 3 vacío son 549.946 posiciones."),
              tags$li(negrita("Alfa-beta"), ": el mismo resultado podando ramas inútiles."),
              tags$li(negrita("Alfa-beta con profundidad limitada"), ": al llegar a d jugadas aplica la función de evaluación ",
                      "h(s) = L_MAX(s) − L_MIN(s), o su versión ponderada, que suma 10^c por cada línea abierta con c fichas propias."))),
        div(h3("Sin garantías"),
            p("Con profundidad limitada ya no hay garantía de jugar de forma óptima: el valor de la raíz es el minimax de un árbol recortado ",
              "cuyas hojas se puntúan con h. El ", negrita("efecto horizonte"), " aparece cuando una amenaza queda justo más allá de la profundidad explorada.")),
        div(h3("El mapa de colores"),
            p("Cada casilla candidata se evalúa con la ventana completa (−∞, +∞), así su valor es exacto. En la comparación de la pestaña 3 se usa la poda normal desde la raíz."))
      ))
  )
}

pag_tres_en_raya_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Estado de la partida. 'decision' es el último cálculo de la IA y 'vista', lo
    # que se muestra en el mapa (una jugada de la IA o una pista).
    rv <- reactiveValues(cfg = kr_config(3, 3), tablero = integer(9), turno = 1L, humano = 1L,
                         fin = FALSE, ganador = 0L, linea = integer(0), ultima = NA,
                         decision = NULL, vista = NULL)

    # En tableros grandes la búsqueda completa es inviable: el nivel Imbatible se
    # limita a 4 o 5 jugadas.
    conf_nivel <- function(libres) {
      nv <- NIVELES[[input$nivel %||% "medio"]]
      prof <- nv$prof
      if (is.infinite(prof) && libres > 9) prof <- if (rv$cfg$n >= 5) 4 else 5
      list(prof = prof, h = nv$h, ordenar = nv$ordenar)
    }

    # Coloca la ficha de p en c y comprueba si la partida ha terminado.
    aplicar <- function(c, p) {
      t <- rv$tablero; t[c] <- p; rv$tablero <- t
      if (kr_gana(t, c, p, rv$cfg)) { rv$fin <- TRUE; rv$ganador <- p; rv$linea <- kr_linea_ganadora(t, rv$cfg) }
      else if (!any(t == 0)) { rv$fin <- TRUE; rv$ganador <- 0L }
      rv$turno <- -p
    }

    # Calcula la jugada de 'jugador'. Las pistas usan siempre la búsqueda más fuerte.
    pensar <- function(jugador, fuerte = FALSE) {
      libres <- sum(rv$tablero == 0)
      cf <- if (fuerte) list(prof = if (libres <= 9) Inf else 4, h = "ponderada", ordenar = TRUE) else conf_nivel(libres)
      d <- withProgress(message = "Pensando", value = 0.5,
        kr_decidir(rv$tablero, jugador, rv$cfg, "alfabeta", cf$prof, cf$h, cf$ordenar,
                   max_nodos = 3e6, raiz_exacta = TRUE))
      d$prof <- cf$prof; d$tablero <- rv$tablero; d$jugador <- jugador
      d
    }

    mover_ia <- function() {
      if (rv$fin) return()
      d <- pensar(-rv$humano); d$tipo <- "ia"
      rv$decision <- d; rv$vista <- d; rv$ultima <- d$jugada
      aplicar(d$jugada, -rv$humano)
    }

    nueva <- function() {
      nk <- as.integer(strsplit(input$tablero, "-")[[1]])
      rv$cfg <- kr_config(nk[1], nk[2]); rv$tablero <- integer(nk[1]^2)
      rv$turno <- 1L; rv$humano <- as.integer(input$humano)
      rv$fin <- FALSE; rv$ganador <- 0L; rv$linea <- integer(0); rv$ultima <- NA
      rv$decision <- NULL; rv$vista <- NULL
      if (rv$humano == -1L) mover_ia()
    }
    observeEvent(input$nueva, nueva())
    observeEvent(list(input$tablero, input$humano), nueva(), ignoreInit = TRUE)

    # Jugada del humano (clic en una casilla) y respuesta de la IA.
    observeEvent(input$click, {
      c <- as.integer(input$click)
      if (rv$fin || rv$turno != rv$humano || rv$tablero[c] != 0) return()
      aplicar(c, rv$humano); rv$ultima <- NA
      mover_ia()
    })

    observeEvent(input$pista, {
      if (rv$fin || rv$turno != rv$humano) return()
      d <- pensar(rv$humano, fuerte = TRUE); d$tipo <- "pista"
      rv$vista <- d
    })

    output$nivel_desc <- renderUI(p(class = "nota", NIVELES[[input$nivel %||% "medio"]]$desc))

    output$estado <- renderUI({
      txt <- if (rv$fin) {
        if (rv$ganador == rv$humano) "¡Has ganado!" else if (rv$ganador == -rv$humano) "Gana la IA." else "Tablas."
      } else if (rv$turno == rv$humano) sprintf("Te toca. Juegas con %s.", if (rv$humano == 1) "X" else "O")
      else "Turno de la IA"
      div(class = "estado", txt, div(class = "nota", sprintf("Gana quien ponga %d en línea.", rv$cfg$k)))
    })

    output$tablero_ui <- renderUI({
      n <- rv$cfg$n; t <- rv$tablero
      activo <- !rv$fin && rv$turno == rv$humano
      celdas <- lapply(seq_len(n * n), function(i) {
        clases <- c(if (i %in% rv$linea) "ganadora", if (!is.na(rv$ultima) && i == rv$ultima && !rv$fin) "ultima")
        b <- tags$button(type = "button", class = paste(clases, collapse = " "),
          `aria-label` = sprintf("Fila %d, columna %d%s", (i - 1) %/% n + 1, (i - 1) %% n + 1,
                                 if (t[i] == 1) ": equis" else if (t[i] == -1) ": círculo" else ", vacía"),
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("click"), i),
          ficha_xo(t[i]))
        if (!activo || t[i] != 0) b$attribs$disabled <- "disabled"
        b
      })
      div(class = "tablero", style = sprintf("grid-template-columns: repeat(%d, 1fr); grid-template-rows: repeat(%d, 1fr);", n, n), celdas)
    })

    output$pensamiento <- renderUI({
      d <- rv$vista
      if (is.null(d)) return(tagList(
        explicacion("Lo que piensa la IA",
          p("Cuando la IA juegue, aquí verás un mapa del tablero: en verde las casillas que le parecían buenas y en rojo las malas."),
          p("También puedes pedir una pista y verás el tablero desde tu lado.")),
        div(class = "roles",
            div(class = "rol j1", tags$strong("X"), "Juega primero."),
            div(class = "rol j2", tags$strong("O"), "Juega segundo."))))
      n <- rv$cfg$n; t <- d$tablero
      vals <- setNames(d$tabla$valor, d$tabla$celda)
      celdas <- lapply(seq_len(n * n), function(i) {
        if (t[i] != 0) return(div(class = "mapa-celda ocupada", ficha_xo(t[i])))
        v <- vals[[as.character(i)]]
        div(class = paste("mapa-celda", if (i == d$jugada) "elegida"),
            style = sprintf("background:%s", color_valor(v, d$W, d$tabla$valor)), span(texto_celda(v, d$W)))
      })
      mirar <- if (is.infinite(d$prof)) "hasta el final de la partida" else sprintf("%d jugada%s por delante", d$prof, if (d$prof == 1) "" else "s")
      tagList(
        div(class = "exp-titulo", if (d$tipo == "pista") "Pista: así ve el tablero la IA desde tu lado" else "Lo que ha pensado la IA"),
        div(class = "mapa", style = sprintf("grid-template-columns: repeat(%d, 1fr); grid-template-rows: repeat(%d, 1fr);", n, n), celdas),
        div(class = "leyenda", paste0("Verde: buena para ", if (d$tipo == "pista") "ti" else "la IA",
            ". Rojo: mala. Estrella: ", if (d$tipo == "pista") "la casilla que te recomienda." else "la casilla que ha elegido.")),
        div(class = "cifras", cifra(miles(d$nodos), "posiciones imaginadas"), cifra(tiempo_txt(d$segundos), "tiempo")),
        p(class = "nota", sprintf("Ha mirado %s.", mirar),
          if (d$W == 1) " Como llega al final, sabe con certeza si cada casilla gana, empata o pierde."
          else " Como no llega al final, los números son estimaciones con la regla de las líneas abiertas.")
      )
    })

    output$lineas <- renderUI({
      p_ia <- -rv$humano; n <- rv$cfg$n
      la <- kr_lineas_abiertas(rv$tablero, p_ia, rv$cfg)
      h <- kr_heuristica(rv$tablero, p_ia, rv$cfg, "simple")
      div(class = "juicio",
        div(class = "mapa juicio-tablero", style = sprintf("grid-template-columns: repeat(%d, 1fr); grid-template-rows: repeat(%d, 1fr);", n, n),
            lapply(rv$tablero, function(v) div(class = "mapa-celda ocupada", ficha_xo(v)))),
        div(class = "cifras",
            cifra(sum(la$abiertas_max), "Líneas abiertas para la IA", COL_J1),
            cifra(sum(la$abiertas_min), "Líneas abiertas para ti", COL_J2),
            cifra(fmt(h), "Puntuación de la IA (la resta)")),
        p(class = "nota", sprintf("Tablero actual de la partida. En este tablero hay %d líneas posibles de %d casillas.", rv$cfg$nL, rv$cfg$k)))
    })

    # Pestaña 3: la misma posición con cada método. Los que superan el límite de
    # nodos se dan por interrumpidos.
    comparacion <- eventReactive(input$comparar, {
      validate(need(!rv$fin, "La partida ha terminado: empieza otra para comparar."))
      t <- rv$tablero; p <- rv$turno; libres <- sum(t == 0)
      configs <- list(
        list("Minimax: mira todo el árbol", "minimax", Inf, FALSE),
        list("Alfa-beta: mira todo, podando", "alfabeta", Inf, FALSE),
        list("Alfa-beta mirando primero lo prometedor", "alfabeta", Inf, TRUE),
        list("Mirando 1 jugada", "alfabeta", 1, FALSE),
        list("Mirando 2 jugadas", "alfabeta", 2, FALSE),
        list("Mirando 4 jugadas", "alfabeta", 4, FALSE))
      withProgress(message = "Comparando", value = 0, {
        lapply(configs, function(cf) {
          incProgress(1 / length(configs), detail = cf[[1]])
          if (is.infinite(cf[[3]]) && libres > 9) return(list(nombre = cf[[1]], nodos = NA, txt = "demasiado grande: más de 9 casillas libres"))
          d <- kr_decidir(t, p, rv$cfg, cf[[2]], cf[[3]], "ponderada", cf[[4]], max_nodos = 3e6)
          if (!d$ok) return(list(nombre = cf[[1]], nodos = NA, txt = "más de 3 millones: se interrumpe"))
          list(nombre = cf[[1]], nodos = d$nodos,
               txt = sprintf("%s. Jugaría en la fila %d, columna %d.", tiempo_txt(d$segundos),
                             (d$jugada - 1) %/% rv$cfg$n + 1, (d$jugada - 1) %% rv$cfg$n + 1))
        })
      })
    })

    output$comparacion <- renderUI({
      if (!isTruthy(input$comparar)) return(explicacion(NULL, p("Pulsa «Comparar en esta posición».")))
      f <- comparacion()
      maxn <- max(c(1, unlist(lapply(f, `[[`, "nodos"))), na.rm = TRUE)
      div(class = "barras-orden comparacion",
        lapply(f, function(x) {
          div(class = "barra-orden",
              span(class = "bo-nombre", x$nombre),
              div(class = "bo-pista", if (!is.na(x$nodos)) div(class = "bo-relleno eq", style = sprintf("width:%g%%", max(0.6, 100 * x$nodos / maxn)))),
              span(class = "bo-num", if (is.na(x$nodos)) "—" else miles(x$nodos), span(class = "bo-pct", x$txt)))
        }),
        div(class = "leyenda", "Posiciones imaginadas por cada método para decidir una sola jugada."))
    })
  })
}
