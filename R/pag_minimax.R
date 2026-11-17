# =============================================================================
# Minimax
# =============================================================================

# Árbol binario completo de 3 o 4 niveles con hojas al azar entre 1 y 9.
arbol_minimax_aleatorio <- function(tam, semilla) {
  set.seed(semilla)
  m <- if (identical(tam, "g")) 4 else 3
  arbol_completo(2, m, sample(1:9, 2^m, replace = TRUE))
}

# Tamaño aproximado del árbol de cada juego (en log10), para la pestaña 3.
JUEGOS_TAMANO <- list(
  tres = list(nombre = "Tres en raya", icono = "hashtag", log10 = log10(549946),
              texto = "549.946 posiciones, contadas de verdad por la IA de esta aplicación."),
  ajedrez = list(nombre = "Ajedrez", icono = "chess-knight", log10 = 100 * log10(35),
                 texto = "Unas 35 jugadas posibles en cada turno y partidas de unos 100 movimientos: 35¹⁰⁰ ≈ 10¹⁵⁴."),
  go = list(nombre = "Go", icono = "circle", log10 = 150 * log10(250),
            texto = "Unas 250 jugadas posibles en cada turno y partidas de unos 150 movimientos: 250¹⁵⁰ ≈ 10³⁶⁰.")
)

# Los números pequeños se escriben enteros; los grandes, como potencia de 10.
potencia10 <- function(l) {
  e <- if (l < 6) floor(l) else round(l)
  if (e < 6) return(miles(10^l))
  tagList("10", tags$sup(e, .noWS = "before"))
}

pag_minimax_ui <- function(id) {
  ns <- NS(id)
  pagina("minimax",
    "Los programas que juegan al ajedrez usan una idea sencilla: «haré la mejor jugada posible suponiendo que mi rival hará la peor para mí».",

    nav_panel("1. Mira cómo piensa",
      panel(
        llenar(uiOutput(ns("arbol"))),
        tagList(
          div(class = "roles",
              div(class = "rol j1", tags$strong("\u25b2 MAX"), "Quiere que el número final sea lo más alto posible."),
              div(class = "rol j2", tags$strong("\u25bc MIN"), "Quiere que sea lo más bajo posible.")),
          p(class = "nota", "Los números de abajo son lo que gana MAX al final de cada partida; lo que gana uno lo pierde el otro."),
          div(class = "fila-botones",
              radioButtons(ns("tam"), NULL, c("8 hojas" = "m", "16 hojas" = "g"), inline = TRUE),
              actionButton(ns("otro"), "Otros números", icon = icon("shuffle"), class = "btn-outline-primary btn-sm")),
          uiOutput(ns("texto_paso")), controles_pasos(ns),
          idea_clave("Minimax rellena el árbol desde abajo: MAX se queda con el mayor y MIN con el menor. ",
                     "El número que llega arriba es lo que MAX puede garantizarse.")
        ))),

    nav_panel("2. ¿Y si el rival no es tan malo?",
      panel(
        div(class = "dos-arboles",
            div(class = "da-col", h3(class = "titulo-mini", "Rival egoísta: busca su premio"), llenar(uiOutput(ns("arbol_egoista")))),
            div(class = "da-col", h3(class = "titulo-mini", "Rival hostil: minimax"), llenar(uiOutput(ns("arbol_hostil"))))),
        tagList(
          explicacion("Minimax es pesimista",
            p("Supone que el rival solo quiere fastidiarte. En un juego normal, el rival busca su propio premio."),
            p("Es el juego por turnos de la otra página: a la izquierda, resuelto con premios para los dos; a la derecha, con minimax.")),
          checkboxInput(ns("medio"), "Cambiar el premio del Jugador 1 en «Derecha» de 2 a ½", FALSE),
          uiOutput(ns("comparacion"))
        ))),

    nav_panel("3. ¿Por qué no se usa con el ajedrez?",
      panel(
        div(class = "pila-centrada", uiOutput(ns("barras"))),
        tagList(
          explicacion("Hay que mirarlo todo", p("Minimax recorre todas las partidas posibles hasta el final. Elige un juego:")),
          radioButtons(ns("juego"), NULL, c("Tres en raya" = "tres", "Ajedrez" = "ajedrez", "Go" = "go"), selected = "ajedrez", inline = TRUE),
          uiOutput(ns("tamano"))
        ))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Definición recursiva"),
            tags$pre(class = "programa", paste(
              "Minimax(s) = utilidad(s)                          si s es final",
              "Minimax(s) = máx sobre a de Minimax(Result(s, a))   si juega MAX",
              "Minimax(s) = mín sobre a de Minimax(Result(s, a))   si juega MIN", sep = "\n"))),
        div(h3("Relación con la inducción hacia atrás"),
            p("Es la inducción hacia atrás aplicada a un juego de suma cero, y el valor de la raíz es el valor del juego.")),
        div(h3("Coste"),
            p("Recorre todo el árbol, así que su coste es del orden de b^m, donde b es el número de jugadas por turno y m la ",
              "longitud de la partida. La memoria que necesita solo crece como b·m."))
      ))
  )
}

pag_minimax_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    semilla <- reactiveVal(5)
    observeEvent(input$otro, semilla(sample.int(1e5, 1)))
    arbol <- reactive(arbol_minimax_aleatorio(input$tam, semilla()))
    mm <- reactive(minimax_eventos(arbol()))
    # Un paso por nodo de decisión, más el inicial y el final.
    paso <- servidor_pasos(input, output, session, reactive(length(mm()$eventos) + 2), intervalo = 1100)

    output$arbol <- renderUI({
      df <- aplanar_arbol(arbol(), suma_cero = TRUE); r <- mm(); k <- paso() - 1
      n <- length(r$eventos)
      valores <- setNames(character(0), character(0)); elegidas <- character(0)
      for (e in r$eventos[seq_len(min(k, n))]) { valores[e$id] <- como_fraccion(e$v); elegidas <- c(elegidas, e$hijo) }
      activo <- if (k >= 1 && k <= n) r$eventos[[k]]$id else NULL
      # Al terminar se resalta la partida principal, desde la raíz.
      if (k > n) {
        camino <- character(0); actual <- "n1"
        repeat {
          ev <- Filter(function(e) e$id == actual, r$eventos)
          if (!length(ev)) break
          camino <- c(camino, ev[[1]]$hijo); actual <- ev[[1]]$hijo
        }
        elegidas <- camino
      }
      svg_arbol(df, suma_cero = TRUE, elegidas = elegidas, valores = valores, activo = activo,
                ux = if (sum(df$es_hoja) > 8) 74 else 112, uy = 100)
    })

    output$texto_paso <- renderUI({
      r <- mm(); k <- paso() - 1; n <- length(r$eventos)
      if (k == 0) return(explicacion("Empezamos por abajo", p("Pulsa «Siguiente paso» o «Reproducir».")))
      if (k > n) {
        return(explicacion(sprintf("MAX puede garantizarse %s", como_fraccion(r$v)),
          p(sprintf("Su mejor primera jugada es %s. El camino ocre es la partida que se jugaría si los dos lo hacen lo mejor posible.",
                    r$eventos[[n]]$elegida)),
          p("Si MIN se equivoca en algún momento, a MAX solo le puede ir mejor.")))
      }
      e <- r$eventos[[k]]
      explicacion(sprintf("Le toca a %s", if (e$jugador == 1) "MAX" else "MIN"),
        p(sprintf("Puede elegir entre: %s.", paste(sprintf("%s (vale %s)", e$acciones, como_fraccion(e$valores)), collapse = ", "))),
        p(if (e$jugador == 1) sprintf("Quiere lo más alto: se queda con %s.", como_fraccion(e$v))
          else sprintf("Quiere lo más bajo: se queda con %s.", como_fraccion(e$v))))
    })

    # Pestaña 2: el mismo árbol con inducción hacia atrás y con minimax.
    arbol_cmp <- reactive(arbol_turnos(derecha = c(if (isTRUE(input$medio)) 0.5 else 2, 1)))
    output$arbol_egoista <- renderUI({
      t <- arbol_cmp(); r <- induccion_hacia_atras(t); df <- aplanar_arbol(t)
      v <- setNames(vapply(names(r$valor), function(i) fmt_pagos(r$valor[[i]]), ""), names(r$valor))
      svg_arbol(df, elegidas = aristas_elegidas(t, r$eleccion)$plan, valores = v, ux = 150)
    })
    output$arbol_hostil <- renderUI({
      t <- arbol_cmp(); r <- minimax_arbol(t); df <- aplanar_arbol(t, suma_cero = TRUE)
      svg_arbol(df, suma_cero = TRUE, elegidas = aristas_elegidas(t, r$eleccion)$plan,
                valores = etiquetas_valor(df, r$valor), ux = 150)
    })
    output$comparacion <- renderUI({
      t <- arbol_cmp(); e1 <- induccion_hacia_atras(t)$eleccion[["n1"]]; e2 <- minimax_arbol(t)$eleccion[["n1"]]
      if (e1 == e2) {
        explicacion("Aquí deciden lo mismo",
          p("El rival egoísta elegiría Arriba (le da 2). Minimax supone que elegiría Abajo (lo peor para MAX)."),
          p(sprintf("Aun así, en los dos casos al Jugador 1 le conviene %s. Pero no siempre coinciden: marca la casilla.", e1)))
      } else {
        explicacion("Ahora deciden distinto",
          p(sprintf("El rival egoísta iría Arriba, así que al Jugador 1 le conviene %s (gana 1 en vez de ½).", e1)),
          p(sprintf("Minimax teme que el rival elija Abajo y prefiere %s para asegurarse ½.", e2)),
          p(negrita("Minimax solo acierta de lleno cuando el rival quiere justo lo contrario que tú"), " (juegos de suma cero)."))
      }
    })

    output$barras <- renderUI({
      maximo <- 360
      div(class = "barras-tam",
          lapply(names(JUEGOS_TAMANO), function(n) {
            g <- JUEGOS_TAMANO[[n]]
            div(class = paste("barra-tam", if (n == input$juego) "actual"),
                span(class = "bt-nombre", ico(g$icono), " ", g$nombre),
                div(class = "bt-pista", div(class = "bt-relleno", style = sprintf("width:%g%%", max(1.5, 100 * g$log10 / maximo)))),
                span(class = "bt-num", potencia10(g$log10)))
          }),
          div(class = "leyenda", "Cada barra mide cuántas cifras tiene el número, no el número: el Go no tiene el doble de partidas que el ajedrez, ",
              "tiene unas 10", tags$sup("206", .noWS = "before"), " veces más."))
    })

    # Tiempo necesario a mil millones de posiciones por segundo, comparado con la
    # edad del universo (unos 4,35·10^17 s).
    output$tamano <- renderUI({
      j <- JUEGOS_TAMANO[[input$juego]]
      seg <- 10^(j$log10 - 9); edad <- 4.35e17
      tiempo <- if (seg < 1) "menos de una milésima de segundo"
        else if (log10(seg) < log10(edad)) sprintf("%s segundos", fmt(seg))
        else tagList("unas ", potencia10(log10(seg) - log10(edad)), " veces la edad del universo")
      tagList(
        div(class = "cifras",
            cifra(potencia10(j$log10), "Partidas o posiciones que habría que mirar"),
            cifra(tiempo, "Tiempo, mirando mil millones por segundo")),
        p(j$texto),
        if (input$juego != "tres") idea_clave("Por eso los programas reales solo miran unas pocas jugadas por delante y estiman el resto (lo verás en el tres en raya).")
      )
    })
  })
}
