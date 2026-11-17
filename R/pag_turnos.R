# =============================================================================
# Juego por turnos (forma extensiva e inducción hacia atrás)
# =============================================================================

# Árbol del Ejemplo 1.3 con los pagos que se elijan en la interfaz.
arbol_turnos <- function(iz_arriba = c(1, 2), iz_abajo = c(0, 0), derecha = c(2, 1)) {
  numerar_arbol(nodo(1, list(
    Izquierda = nodo(2, list(Arriba = do.call(hoja, as.list(iz_arriba)),
                             Abajo = do.call(hoja, as.list(iz_abajo)))),
    Derecha = do.call(hoja, as.list(derecha))
  )))
}

# Árbol aleatorio de profundidad 3 (algunas ramas terminan antes).
arbol_aleatorio_turnos <- function(grande, semilla) {
  set.seed(semilla)
  b <- if (grande) 3 else 2; m <- 3
  acciones <- if (b == 2) c("Izq.", "Der.") else c("Izq.", "Centro", "Der.")
  rec <- function(prof, jugador) {
    if (prof == m || (prof >= 2 && runif(1) < 0.25)) return(hoja(sample(0:9, 1), sample(0:9, 1)))
    hijos <- lapply(seq_len(b), function(k) rec(prof + 1, 3 - jugador))
    names(hijos) <- acciones
    nodo(jugador, hijos)
  }
  numerar_arbol(rec(0, 1))
}

# Pareja de entradas numéricas para los pagos de una hoja.
entrada_par <- function(ns, id, etiqueta, v) {
  div(class = "entrada-par",
      div(class = "ep-etiqueta", etiqueta),
      numericInput(ns(paste0(id, "_1")), span(class = "j1", "Jugador 1"), v[1], step = 1),
      numericInput(ns(paste0(id, "_2")), span(class = "j2", "Jugador 2"), v[2], step = 1))
}

pag_turnos_ui <- function(id) {
  ns <- NS(id)
  dfp <- aplanar_arbol(arbol_pares_nones())
  pagina("turnos",
    "Aquí los jugadores no eligen a la vez: primero decide el Jugador 1 y, si hace falta, después decide el Jugador 2 sabiendo lo que hizo el primero.",

    nav_panel("1. Razonar desde el final",
      panel(
        llenar(uiOutput(ns("arbol"))),
        tagList(
          uiOutput(ns("texto_paso")), controles_pasos(ns),
          accordion(open = FALSE, class = "ajustes",
            accordion_panel("Cambiar los premios", icon = icon("sliders"),
              entrada_par(ns, "iza", "Izquierda, Arriba", c(1, 2)),
              entrada_par(ns, "izb", "Izquierda, Abajo", c(0, 0)),
              entrada_par(ns, "der", "Derecha", c(2, 1)),
              actionButton(ns("restaurar"), "Volver a los premios originales", class = "btn-link btn-sm")))
        ))),

    nav_panel("2. ¿Y si no ve lo que hizo el otro?",
      panel(
        llenar(svg_arbol(dfp, conjuntos = conjuntos_de_informacion(dfp))),
        tagList(
          explicacion("Pares o nones dibujado como árbol",
            p("Se dibuja como si el Jugador 1 eligiera primero, pero el Jugador 2 decide ", negrita("sin saber"),
              " qué ha elegido. La línea discontinua une los dos sitios en los que podría estar."),
            p("Como no sabe en qué rama está, no puede razonar desde el final. En realidad es lo mismo que elegir a la vez: ",
              "es el juego de pares o nones de siempre.")),
          idea_clave("Razonar desde el final solo funciona si cada jugador sabe exactamente lo que ha pasado antes.")
        ))),

    nav_panel("3. Un árbol más grande",
      panel(
        llenar(uiOutput(ns("arbol_grande"))),
        tagList(
          div(class = "fila-botones",
              radioButtons(ns("tam"), NULL, c("Mediano" = "m", "Grande" = "g"), inline = TRUE),
              actionButton(ns("otro"), "Otro árbol", icon = icon("shuffle"), class = "btn-outline-primary btn-sm")),
          uiOutput(ns("texto_grande")), controles_pasos(ns, "pg"),
          idea_clave("Siempre se puede resolver así un juego por turnos en el que todos ven lo que pasó antes, ",
                     "y siempre sale una forma de jugar estable sin necesidad de azar.")
        ))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Estrategias como planes completos"),
            p("Una ", negrita("estrategia"), " en un juego por turnos dice qué haría el jugador en cada uno de sus nodos, ",
              "incluso en los que no se alcanzan. El Jugador 1 tiene {Izquierda, Derecha} y el Jugador 2, {Arriba, Abajo}. ",
              "Escrito como tabla:"),
            uiOutput(ns("forma_estrategica"))),
        div(uiOutput(ns("texto_equilibrios"))),
        div(h3("Equilibrio perfecto en subjuegos"),
            p("La inducción hacia atrás encuentra un ", negrita("equilibrio perfecto en subjuegos"),
              ": un equilibrio de Nash en cada parte del árbol. Descarta los equilibrios que se apoyan en amenazas no creíbles.")),
        div(h3("Teorema de Zermelo-Kuhn"),
            p("Todo juego finito con información perfecta tiene al menos un equilibrio de Nash en estrategias puras. ",
              "Por eso el ajedrez está «resuelto» en teoría: jugando los dos de forma perfecta, o ganan las blancas, o ganan las negras, o es tablas; ",
              "el árbol es tan grande que nadie sabe cuál."))
      ))
  )
}

pag_turnos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$restaurar, {
      vals <- c(iza_1 = 1, iza_2 = 2, izb_1 = 0, izb_2 = 0, der_1 = 2, der_2 = 1)
      for (nm in names(vals)) updateNumericInput(session, nm, value = vals[[nm]])
    })

    # Pagos de las tres hojas (los originales mientras no existen los controles).
    pagos <- reactive({
      v <- c(input$iza_1, input$iza_2, input$izb_1, input$izb_2, input$der_1, input$der_2)
      if (length(v) < 6) v <- c(1, 2, 0, 0, 2, 1)
      validate(need(all(!is.na(v)), "Completa los seis premios."))
      list(iza = v[1:2], izb = v[3:4], der = v[5:6])
    })
    arbol <- reactive({ p <- pagos(); arbol_turnos(p$iza, p$izb, p$der) })
    bi <- reactive(induccion_hacia_atras(arbol()))

    # Equilibrios de la forma estratégica y perfectos en subjuegos. Los que no son
    # perfectos en subjuegos se apoyan en una amenaza no creíble.
    analisis <- reactive({
      p <- pagos()
      G <- juego(matrix(c(p$iza[1], p$izb[1], p$der[1], p$der[1]), 2, byrow = TRUE),
                 matrix(c(p$iza[2], p$izb[2], p$der[2], p$der[2]), 2, byrow = TRUE),
                 c("Izquierda", "Derecha"), c("Arriba", "Abajo"))
      u2 <- c(Arriba = p$iza[2], Abajo = p$izb[2])
      spe <- character(0)
      for (a in names(u2)[u2 >= max(u2) - 1e-9]) {
        u1 <- c(Izquierda = if (a == "Arriba") p$iza[1] else p$izb[1], Derecha = p$der[1])
        for (b in names(u1)[u1 >= max(u1) - 1e-9]) spe <- c(spe, sprintf("(%s, %s)", b, a))
      }
      ne <- equilibrios_puros(G)$Perfil
      list(G = G, spe = spe, ne = ne, no_creibles = setdiff(ne, spe))
    })

    paso <- servidor_pasos(input, output, session, 6)

    output$arbol <- renderUI({
      df <- aplanar_arbol(arbol()); r <- bi(); k <- paso()
      j2 <- "n2"; raiz <- "n1"
      hijo <- function(nodo_id, accion) df$id[df$padre %in% nodo_id & df$accion == accion]
      el2 <- hijo(j2, r$eleccion[[j2]]); el1 <- hijo(raiz, r$eleccion[[raiz]])
      otro2 <- setdiff(df$id[df$padre %in% j2], el2)
      activo <- NULL; elegidas <- character(0); atenuadas <- character(0); valores <- NULL
      boc <- NULL; amen <- character(0)
      v2 <- setNames(fmt_pagos(r$valor[[j2]]), j2); v1 <- setNames(fmt_pagos(r$valor[[raiz]]), raiz)
      # Qué se resalta en cada paso de la explicación.
      if (k == 2) { activo <- j2; atenuadas <- hijo(raiz, "Derecha") }
      if (k == 3) { activo <- j2; elegidas <- el2; atenuadas <- c(otro2, hijo(raiz, "Derecha")); valores <- v2 }
      if (k == 4) { activo <- raiz; elegidas <- el2; atenuadas <- otro2; valores <- v2 }
      if (k >= 5) { elegidas <- c(el2, el1); valores <- c(v2, v1)
                    atenuadas <- if (r$eleccion[[raiz]] == "Derecha") c(j2, df$id[df$padre %in% j2]) else otro2 }
      if (k == 6) {
        a <- analisis()
        if (length(a$no_creibles)) {
          amenaza <- sub("^\\([^,]+, (.+)\\)$", "\\1", a$no_creibles[1])
          boc <- div(class = "bocadillo", span(class = "bocadillo-quien", "Jugador 2:"),
                     sprintf("«¡Si vas a la izquierda, elegiré %s!»", amenaza))
          amen <- hijo(j2, amenaza); atenuadas <- character(0); activo <- j2
        }
      }
      tagList(boc, svg_arbol(df, elegidas = elegidas, atenuadas = atenuadas, activo = activo,
                             valores = valores, amenazas = amen))
    })

    output$texto_paso <- renderUI({
      r <- bi(); p <- pagos(); k <- paso(); a <- analisis()
      e2 <- r$eleccion[["n2"]]; e1 <- r$eleccion[["n1"]]
      u_iz <- r$valor[["n2"]]
      switch(k,
        explicacion("Así es el juego",
          p("El Jugador 1 (azul) decide primero entre Izquierda y Derecha. Si va a la derecha, se acaba."),
          p("Si va a la izquierda, decide el Jugador 2 (granate) entre Arriba y Abajo."),
          p("Abajo están los premios: el azul para el Jugador 1 y el granate para el Jugador 2. ¿Qué le conviene al Jugador 1?")),
        explicacion("Empieza por el final",
          p("Imagina que el Jugador 1 ha ido a la izquierda y le toca al Jugador 2."),
          p(sprintf("Si elige Arriba, gana %s. Si elige Abajo, gana %s.", como_fraccion(p$iza[2]), como_fraccion(p$izb[2])))),
        explicacion(sprintf("El Jugador 2 elegiría %s", e2),
          p(if (abs(p$iza[2] - p$izb[2]) < 1e-9) "Le da igual; suponemos que elige Arriba."
            else sprintf("Porque %s es más que %s.", como_fraccion(max(p$iza[2], p$izb[2])), como_fraccion(min(p$iza[2], p$izb[2])))),
          p(sprintf("Así que ir a la izquierda lleva a %s.", fmt_pagos(u_iz)))),
        explicacion("Ahora decide el Jugador 1",
          p("Ya sabe lo que pasará en cada caso:"),
          tags$ul(tags$li(sprintf("Izquierda: el Jugador 2 elegirá %s y él ganará %s.", e2, como_fraccion(u_iz[1]))),
                  tags$li(sprintf("Derecha: gana %s.", como_fraccion(p$der[1]))))),
        explicacion(sprintf("El Jugador 1 elige %s", e1),
          p(sprintf("El juego termina con premios %s.", fmt_pagos(r$pagos))),
          p("Nadie se arrepiente de lo que ha hecho: es un equilibrio.")),
        if (length(a$no_creibles)) {
          amenaza <- sub("^\\([^,]+, (.+)\\)$", "\\1", a$no_creibles[1])
          explicacion("¿Y si el Jugador 2 amenaza?",
            p(sprintf("Podría decir: «si vas a la izquierda, elegiré %s» (rama granate discontinua). Si el Jugador 1 se lo creyera, iría a la derecha.", amenaza)),
            p(negrita("Pero la amenaza no es creíble"), sprintf(": si de verdad le tocara decidir, al Jugador 2 le convendría %s (rama ocre). ", e2),
              "Un jugador listo no se deja asustar por amenazas que el otro no cumpliría."))
        } else explicacion("¿Y si el Jugador 2 amenaza?",
          p("Con estos premios no hay ninguna amenaza que pueda cambiar el resultado: el equilibrio que hemos encontrado es el único."))
      )
    })

    output$forma_estrategica <- renderUI(svg_matriz(analisis()$G, estrellas = TRUE, flechas_j1 = 1:2, flechas_j2 = 1:2))
    output$texto_equilibrios <- renderUI({
      a <- analisis()
      p("Equilibrios de Nash (estrellas): ", negrita(paste(a$ne, collapse = " y ")),
        ". Perfectos en subjuegos (los que da la inducción hacia atrás): ", negrita(paste(a$spe, collapse = " y ")), ".")
    })

    # --- Árbol grande -------------------------------------------------------
    semilla <- reactiveVal(11)
    observeEvent(input$otro, semilla(sample.int(1e5, 1)))
    grande <- reactive(arbol_aleatorio_turnos(identical(input$tam, "g"), semilla()))
    bi_g <- reactive(induccion_hacia_atras(grande()))
    paso_g <- servidor_pasos(input, output, session, reactive(nrow(bi_g()$pasos) + 1), "pg", intervalo = 1100)

    output$arbol_grande <- renderUI({
      df <- aplanar_arbol(grande()); r <- bi_g(); k <- paso_g() - 1
      hechos <- if (k > 0) r$pasos[seq_len(k), ] else r$pasos[0, ]
      elegidas <- character(0); valores <- setNames(character(0), character(0))
      # Decisiones ya tomadas en los k primeros pasos de la inducción.
      for (q in seq_len(nrow(hechos))) {
        idn <- hechos$id[q]
        elegidas <- c(elegidas, df$id[df$padre %in% idn & df$accion == hechos$accion[q]])
        valores[idn] <- fmt_pagos(r$valor[[idn]])
      }
      activo <- if (k > 0 && k <= nrow(r$pasos)) r$pasos$id[k] else NULL
      svg_arbol(df, elegidas = elegidas, valores = valores, activo = activo,
                ux = if (sum(df$es_hoja) > 12) 92 else 120, uy = 104)
    })

    output$texto_grande <- renderUI({
      r <- bi_g(); k <- paso_g() - 1
      if (k == 0) return(explicacion("Pulsa «Siguiente paso» o «Reproducir»",
        p("Se resolverá el árbol nodo a nodo, siempre empezando por los más cercanos al final.")))
      fila <- r$pasos[k, ]
      opciones <- strsplit(fila$Opciones, ";  ")[[1]]
      explicacion(sprintf("Le toca al Jugador %d", fila$jugador),
        p("Sus opciones y lo que pasaría con cada una:"),
        tags$ul(lapply(opciones, tags$li)),
        p(sprintf("Elige %s, porque es lo que más le da a él.", fila$accion)),
        if (k == nrow(r$pasos)) p(negrita(sprintf("Resultado final: %s.", fmt_pagos(r$pagos)))))
    })
  })
}
