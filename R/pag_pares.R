# =============================================================================
# Pares o nones
# =============================================================================

# Icono y nombre de cada opción.
MANO <- c(P = "hand-peace", N = "hand-point-up")
NOMBRE_PN <- c(P = "Pares", N = "Nones")

pag_pares_ui <- function(id) {
  ns <- NS(id)
  G <- juegos_predefinidos$pares
  pagina("pares",
    "Dos jugadores dicen a la vez «pares» o «nones». Si dicen lo mismo, gana el Jugador 1 y se lleva 5 €; si no, gana el Jugador 2.",

    nav_panel("1. Juega contra el ordenador",
      panel(
        div(class = "pila-centrada",
            uiOutput(ns("resultado")), uiOutput(ns("marcador")), uiOutput(ns("historial")),
            uiOutput(ns("comentario"))),
        tagList(
          explicacion("Tú eres el Jugador 1", p("Ganas cuando dices lo mismo que el ordenador.")),
          div(class = "botones-grandes",
              boton_eleccion(ns("pares"), MANO[["P"]], "Pares"),
              boton_eleccion(ns("nones"), MANO[["N"]], "Nones")),
          radioButtons(ns("modo"), "¿Cómo juega el ordenador?", c(
            "Lanza una moneda: elige al azar" = "azar",
            "Te observa e intenta adivinarte" = "observa"), selected = "observa"),
          div(class = "fila-botones",
              actionButton(ns("cien"), "Jugar 100 veces al azar", icon = icon("dice"), class = "btn-outline-primary btn-sm"),
              actionButton(ns("reiniciar"), "Empezar de cero", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm"))
        ))),

    nav_panel("2. ¿Hay alguna jugada segura?",
      panel(
        tagList(llenar(svg_matriz(G, flechas_j1 = 1:2, flechas_j2 = 1:2, estrellas = TRUE,
                                  textos1 = matrix(c("+5 €", "\u22125 €", "\u22125 €", "+5 €"), 2),
                                  textos2 = matrix(c("\u22125 €", "+5 €", "+5 €", "\u22125 €"), 2))),
                leyenda_matriz(con_estrella = FALSE)),
        tagList(
          explicacion("Sigue las flechas",
            p("Cada casilla dice lo que gana cada uno. Empieza arriba a la izquierda: los dos dicen pares y gana el Jugador 1."),
            p("Al Jugador 2 le conviene cambiar a nones (flecha roja). Ahora pierde el Jugador 1, que quiere cambiar a nones (flecha azul)… y así sin parar."),
            p("Las flechas dan vueltas en círculo: de todas las casillas sale alguna. ", negrita("Ninguna jugada es segura."))),
          idea_clave("Si eres predecible, pierdes: si el rival sabe lo que vas a decir, siempre te gana.")
        ))),

    nav_panel("3. La solución: mezclar al azar",
      panel(
        tagList(llenar(plotOutput(ns("mezcla_grafico"), height = "100%")),
                div(class = "leyenda", "Líneas finas: lo que ganas si el rival dice siempre lo mismo. ",
                    "Línea gruesa: lo que ganas si el rival te conoce y elige lo peor para ti.")),
        tagList(
          sliderInput(ns("p"), "Digo «pares» este porcentaje de las veces", 0, 100, 70, 5, post = " %"),
          uiOutput(ns("mezcla_barra")),
          uiOutput(ns("mezcla_texto")),
          idea_clave("Lo mejor es mezclar mitad y mitad. Así el rival no puede aprovecharse y, de media, ",
                     "nadie gana nada: ese 0 € es el «valor» del juego.")
        ))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Un juego de suma cero"),
            p("Lo que gana uno lo pierde el otro, así que basta con la tabla de ganancias del Jugador 1:"),
            svg_matriz(G, solo_j1 = TRUE, corto1 = "gana J1")),
        div(h3("El hueco entre maximin y minimax"),
            p("Sin mezclar, lo máximo que el Jugador 1 puede asegurarse es −5 € (valor maximin) y lo mínimo a lo que ",
              "el Jugador 2 puede limitarle es 5 € (valor minimax).")),
        div(h3("El teorema minimax"),
            p("Con estrategias mixtas los dos valores coinciden y valen 0: es el ", negrita("teorema minimax de von Neumann"),
              ". El único equilibrio de Nash es que los dos mezclen ½ y ½.")),
        div(h3("Cálculo con programación lineal"),
            p("Si el Jugador 1 dice pares con probabilidad x₁ y nones con x₂:"),
            tags$pre(class = "programa", paste(
              "Maximizar v sujeto a",
              "  v ≤  5x₁ − 5x₂     (si el rival dice pares)",
              "  v ≤ −5x₁ + 5x₂     (si el rival dice nones)",
              "  x₁ + x₂ = 1,  x₁, x₂ ≥ 0", sep = "\n")),
            uiOutput(ns("solucion_lp")))
      ))
  )
}

pag_pares_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Historial: lo que dijiste, lo que dijo el ordenador y si ganaste.
    rv <- reactiveValues(hist = data.frame(tu = character(0), ord = character(0), gana = logical(0)))

    elige_ordenador <- function(hist) {
      if (input$modo == "azar" || nrow(hist) == 0) return(sample(c("P", "N"), 1))
      # Predice lo que más has dicho y elige lo contrario (el Jugador 2 gana si no coincidís).
      cp <- sum(hist$tu == "P") + 1; cn <- sum(hist$tu == "N") + 1
      pred <- if (cp > cn) "P" else if (cn > cp) "N" else sample(c("P", "N"), 1)
      if (pred == "P") "N" else "P"
    }
    jugar <- function(tu) {
      ord <- elige_ordenador(rv$hist)
      rv$hist <- rbind(rv$hist, data.frame(tu = tu, ord = ord, gana = tu == ord))
    }
    observeEvent(input$pares, jugar("P"))
    observeEvent(input$nones, jugar("N"))
    observeEvent(input$cien, for (k in 1:100) jugar(sample(c("P", "N"), 1)))
    observeEvent(input$reiniciar, rv$hist <- rv$hist[0, ])
    # Al cambiar la forma de jugar del ordenador se empieza de cero.
    observeEvent(input$modo, rv$hist <- rv$hist[0, ], ignoreInit = TRUE)

    output$resultado <- renderUI({
      h <- rv$hist
      if (!nrow(h)) return(div(class = "revelacion vacia", "Elige «Pares» o «Nones» para empezar."))
      u <- h[nrow(h), ]
      div(class = paste("revelacion", if (u$gana) "gana" else "pierde"),
          div(div(class = "rev-quien", "Tú"), div(class = "rev-ico", ico(MANO[[u$tu]])), div(NOMBRE_PN[[u$tu]])),
          div(class = "rev-centro", if (u$gana) "Ganas 5 €" else "Pierdes 5 €"),
          div(div(class = "rev-quien", "Ordenador"), div(class = "rev-ico", ico(MANO[[u$ord]])), div(NOMBRE_PN[[u$ord]])))
    })

    output$marcador <- renderUI({
      h <- rv$hist; n <- nrow(h)
      if (!n) return(NULL)
      saldo <- 5 * sum(h$gana) - 5 * sum(!h$gana)
      div(class = "cifras",
          cifra(euros(saldo), "Tu saldo", if (saldo > 0) COL_OK else if (saldo < 0) COL_MAL else NULL),
          cifra(miles(n), "Partidas"),
          cifra(pct(mean(h$gana)), "Ganadas"),
          cifra(pct(mean(h$tu == "P")), "Veces que has dicho pares"))
    })

    output$historial <- renderUI({
      h <- rv$hist; n <- nrow(h)
      if (!n) return(NULL)
      ult <- tail(h, 50)
      tagList(
        div(class = "historial", `aria-label` = "Últimas partidas",
            lapply(seq_len(nrow(ult)), function(k) {
              span(class = paste("ficha", if (ult$gana[k]) "gana" else "pierde"),
                   title = sprintf("Tú: %s, ordenador: %s", NOMBRE_PN[[ult$tu[k]]], NOMBRE_PN[[ult$ord[k]]]),
                   ult$tu[k])
            })),
        div(class = "leyenda", "Últimas partidas: verde si ganaste, rojo si perdiste. La letra es lo que dijiste."))
    })

    output$comentario <- renderUI({
      h <- rv$hist; n <- nrow(h)
      if (n < 6) return(NULL)
      fp <- mean(h$tu == "P"); g <- mean(h$gana)
      txt <- if (input$modo == "observa" && abs(fp - 0.5) >= 0.2) {
        sprintf("Has dicho pares el %s de las veces. El ordenador lo ha notado y te contesta lo contrario: por eso solo has ganado el %s de las partidas.",
                pct(fp), pct(g))
      } else if (input$modo == "observa") {
        "Estás mezclando bastante, así que al ordenador le cuesta adivinarte. Si repites siempre lo mismo, verás cómo empieza a ganarte."
      } else {
        "El ordenador elige al azar, mitad y mitad. Hagas lo que hagas, a la larga ganarás más o menos la mitad de las veces."
      }
      explicacion(NULL, p(txt))
    })

    # Pestaña 3: estrategia mixta del Jugador 1.
    output$mezcla_barra <- renderUI({
      p <- input$p / 100
      barra_mixta(c(p, 1 - p), c("Pares", "Nones"), c(COL_J1, "#9DB0CC"))
    })

    output$mezcla_texto <- renderUI({
      p <- input$p / 100
      si_pares <- 10 * p - 5; si_nones <- 5 - 10 * p
      peor <- min(si_pares, si_nones)
      if (abs(p - 0.5) < 1e-9) {
        return(explicacion("Mitad y mitad",
          p("Si el rival dice siempre pares, ganas de media 0 €. Si dice siempre nones, también. Haga lo que haga, ",
            negrita("no puede sacarte ventaja"), "."),
          p("Es lo mejor que puedes garantizarte.")))
      }
      contra <- if (si_pares < si_nones) "pares" else "nones"
      explicacion(sprintf("Dices pares el %s", pct(p)),
        p(sprintf("Si tu rival lo sabe, le basta con decir siempre «%s». Entonces ", contra),
          negrita(sprintf("pierdes de media %s por partida", euros(-peor))), "."),
        p("Prueba a acercarte al 50 %."))
    })

    # Pago esperado frente a cada opción fija del rival y el peor de los dos.
    output$mezcla_grafico <- renderPlot({
      p <- input$p / 100
      op <- estilo_grafico(); on.exit(par(op))
      lienzo(c(0, 100), c(-5, 5), "% de veces que dices pares", "Lo que ganas de media (€)", xat = seq(0, 100, 20))
      x <- 0:100
      a <- 10 * x / 100 - 5; b <- 5 - 10 * x / 100
      lines(x, a, col = "#A6AEB9", lwd = 2); lines(x, b, col = "#A6AEB9", lwd = 2)
      text(96, 4.3, "rival: siempre pares", adj = 1, col = "#5B6270", cex = 0.95)
      text(4, 4.3, "rival: siempre nones", adj = 0, col = "#5B6270", cex = 0.95)
      lines(x, pmin(a, b), col = COL_J1, lwd = 5)
      abline(h = 0, col = "#C9CED6", lty = 2)
      y <- min(10 * p - 5, 5 - 10 * p)
      points(100 * p, y, pch = 21, bg = COL_EQ, col = "white", cex = 3, lwd = 2)
    }, res = 96)

    output$solucion_lp <- renderUI({
      s <- valor_suma_cero(juegos_predefinidos$pares$A)
      p(sprintf("Resolviendo este programa (y el del Jugador 2, que es su dual) se obtiene x = %s, y = %s y valor v = %s.",
                fmt_vector(s$x), fmt_vector(s$y), como_fraccion(s$v)))
    })
  })
}
