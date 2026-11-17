# =============================================================================
# Aprender jugando (juegos repetidos)
# =============================================================================

# Juegos disponibles, con las etiquetas cortas y largas de cada acción.
JUEGOS_APRENDER <- list(
  pares = list(nombre = "Pares o nones", G = juegos_predefinidos$pares,
               cortos = c("P", "N"), largos = c("pares", "nones")),
  prisionero = list(nombre = "Dilema del prisionero",
                    G = { g <- juegos_predefinidos$prisionero; g$filas <- g$columnas <- c("Callar", "Declarar"); g },
                    cortos = c("C", "D"), largos = c("callar", "declarar")),
  canal = list(nombre = "Acceso al canal", G = juegos_predefinidos$canal,
               cortos = c("A", "B"), largos = c("el canal A", "el canal B"))
)

pag_aprender_ui <- function(id) {
  ns <- NS(id)
  pagina("aprender",
    "Dos jugadores repiten el mismo juego cientos de veces. No saben qué hará el otro, pero lo van observando y ajustan su forma de jugar. ¿Acaban jugando como dice la teoría?",

    nav_panel("1. Muchas rondas seguidas",
      panel(
        tagList(uiOutput(ns("linea_tiempo")),
                llenar(plotOutput(ns("frecuencias"), height = "100%")),
                div(class = "leyenda", "Porcentaje de rondas, hasta ese momento, en las que cada jugador ha elegido la primera opción. ",
                    "La línea discontinua es lo que predice la teoría.")),
        tagList(
          radioButtons(ns("juego"), "Juego", c("Pares o nones" = "pares", "Dilema del prisionero" = "prisionero",
                                                "Acceso al canal" = "canal"), inline = TRUE),
          radioButtons(ns("metodo"), "Cómo aprende cada jugador", c(
            "Responde a lo que el otro hace más a menudo" = "fictitious",
            "Juega más lo que le habría ido mejor" = "regret")),
          div(class = "fila-botones",
              div(style = "flex:1; min-width: 180px", sliderInput(ns("T"), "Rondas", 50, 2000, 500, 50, sep = ".", ticks = FALSE)),
              actionButton(ns("otra"), "Jugar otra vez", icon = icon("shuffle"), class = "btn-outline-primary btn-sm")),
          uiOutput(ns("resumen"))
        ), ancho = 430)),

    nav_panel("Matemáticas",
      panel(
        tagList(llenar(plotOutput(ns("arrepentimiento"), height = "100%")),
                div(class = "leyenda", "Arrepentimiento medio de cada jugador frente a la referencia 1/√T (escalas logarítmicas).")),
        tagList(
          explicacion("Fictitious play",
            p("Cada jugador responde de la mejor forma a la frecuencia de las acciones pasadas del rival (Brown, 1951). ",
              "En suma cero, el pago de las frecuencias converge al valor del juego (teorema de Robinson).")),
          explicacion("Regret matching",
            p("Cada acción se juega con probabilidad proporcional a su arrepentimiento acumulado positivo. El arrepentimiento ",
              "medio tiende a 0 como 1/√T y, si ambos lo tienen ≤ ε, las frecuencias forman un 2ε-equilibrio.")),
          uiOutput(ns("epsilon"))
        )))
  )
}

pag_aprender_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    semilla <- reactiveVal(1)
    observeEvent(input$otra, semilla(semilla() + 1))
    info <- reactive(JUEGOS_APRENDER[[input$juego %||% "pares"]])
    sim <- reactive({
      simular_aprendizaje(info()$G, input$T %||% 500, input$metodo %||% "fictitious", desempate = "primera", semilla = semilla())
    })
    # Frecuencia de la primera acción en el equilibrio (línea discontinua).
    teoria <- reactive(switch(input$juego %||% "pares", pares = 0.5, prisionero = 0, canal = 0.5))

    output$linea_tiempo <- renderUI({
      j <- info(); r <- sim(); k <- seq_len(min(24, length(r$a1)))
      ficha <- function(a) span(class = paste("chip-accion", if (a == 1) "op1" else "op2"), j$cortos[a])
      fila <- function(nombre, clase, contenido) div(class = "lt-fila", span(class = paste("lt-nombre", clase), nombre), contenido)
      tagList(
        div(class = "linea-tiempo",
          fila("Jugador 1", "j1", lapply(k, function(t) ficha(r$a1[t]))),
          fila("Jugador 2", "j2", lapply(k, function(t) ficha(r$a2[t]))),
          fila("Ronda", "", lapply(k, function(t) span(class = "chip-num", t)))),
        div(class = "leyenda", sprintf("Las 24 primeras rondas. %s = %s, %s = %s.", j$cortos[1], j$largos[1], j$cortos[2], j$largos[2]),
            if (identical(input$juego, "canal")) " Si los dos eligen la misma letra, chocan."))
    })

    output$frecuencias <- renderPlot({
      j <- info(); r <- sim(); T <- length(r$a1); tt <- seq_len(T)
      op <- estilo_grafico(); on.exit(par(op))
      lienzo(c(1, T), c(0, 100), "Ronda", sprintf("%% de veces que elige %s", j$largos[1]), yat = seq(0, 100, 25))
      abline(h = 100 * teoria(), col = COL_EQ, lwd = 2, lty = 2)
      lines(tt, 100 * r$F1[, 1], col = COL_J1, lwd = 3)
      lines(tt, 100 * r$F2[, 1], col = COL_J2, lwd = 3)
      legend("topright", c("Jugador 1", "Jugador 2", "Teoría"), col = c(COL_J1, COL_J2, COL_EQ),
             lwd = c(3, 3, 2), lty = c(1, 1, 2), bty = "n", cex = 0.95, horiz = TRUE, seg.len = 1.6)
    }, res = 96)

    output$resumen <- renderUI({
      j <- info(); r <- sim(); T <- length(r$a1)
      f1 <- r$F1[T, 1]; f2 <- r$F2[T, 1]
      ultimas <- max(1, T - 99):T
      cifras <- div(class = "cifras",
        cifra(pct(f1), sprintf("Jugador 1: %s", j$largos[1]), COL_J1),
        cifra(pct(f2), sprintf("Jugador 2: %s", j$largos[1]), COL_J2))
      txt <- switch(input$juego %||% "pares",
        pares = explicacion("Acaban mezclando mitad y mitad",
          p("Si uno se inclina hacia una opción, el otro lo aprovecha y le obliga a corregir. A la larga los dos dicen pares la mitad de las veces: justo el equilibrio.")),
        prisionero = explicacion("Acaban declarando",
          p("Declarar siempre da mejor resultado que callar, así que los dos lo aprenden enseguida."),
          p(sprintf("En las últimas rondas han declarado los dos el %s de las veces.",
                    pct(mean(r$a1[ultimas] == 2 & r$a2[ultimas] == 2))))),
        canal = {
          ok <- mean(r$a1[ultimas] != r$a2[ultimas])
          if (ok > 0.9) explicacion("Se han repartido los canales",
            p("Cada uno ha acabado en un canal distinto y ya no chocan."),
            p(sprintf("En las últimas rondas no han chocado el %s de las veces.", pct(ok))))
          else explicacion("No consiguen coordinarse",
            p("Como piensan exactamente igual, cambian de canal a la vez y siguen chocando."),
            p(sprintf("En las últimas rondas solo han evitado el choque el %s de las veces. Prueba «Juega más lo que le habría ido mejor»: su azar les ayuda.", pct(ok))))
        })
      tagList(cifras, txt)
    })

    # Pestaña de matemáticas: arrepentimiento medio frente a la referencia 1/√T.
    output$arrepentimiento <- renderPlot({
      j <- info(); r <- sim(); T <- length(r$a1); tt <- seq_len(T)
      op <- estilo_grafico(); on.exit(par(op))
      ref <- (max(c(j$G$A, j$G$B)) - min(c(j$G$A, j$G$B))) / sqrt(tt)
      ys <- c(r$reg1, r$reg2, ref); ys <- ys[ys > 0]
      plot(NA, xlim = c(1, T), ylim = range(ys), log = "xy", xlab = "Ronda (escala log)",
           ylab = "Arrepentimiento medio (escala log)", axes = FALSE)
      axis(1, lwd = 0); axis(2, lwd = 0)
      pos <- function(v) ifelse(v > 0, v, NA)
      lines(tt, pos(r$reg1), col = COL_J1, lwd = 2.5); lines(tt, pos(r$reg2), col = COL_J2, lwd = 2.5)
      lines(tt, ref, lty = 2, col = "#5B6270")
      legend("bottomleft", c("Jugador 1", "Jugador 2", "referencia 1/√T"), col = c(COL_J1, COL_J2, "#5B6270"),
             lty = c(1, 1, 2), lwd = 2, bty = "n")
    }, res = 96)

    # Comprobación de la Proposición 2.3 con las frecuencias finales.
    output$epsilon <- renderUI({
      j <- info(); r <- sim(); T <- length(r$a1)
      x <- r$F1[T, ]; y <- r$F2[T, ]; eps <- max(r$reg1[T], r$reg2[T])
      g <- ganancias_desviacion(j$G, x, y)
      tagList(
        div(class = "cifras", cifra(fmt(eps, 3), "ε: mayor arrepentimiento medio"),
            cifra(fmt(max(g), 3), "Lo máximo que alguien ganaría desviándose")),
        p(class = "nota", if (es_suma_cero(j$G)) sprintf("Como el juego es de suma cero, esa ganancia no puede superar 2ε = %s.", fmt(2 * eps, 3))
          else "Este juego no es de suma cero, así que la cota 2ε no está garantizada, aunque suele cumplirse."))
    })
  })
}
