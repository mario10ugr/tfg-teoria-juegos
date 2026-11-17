# =============================================================================
# Acceso al canal
# =============================================================================

juego_canal <- function() {
  G <- juegos_predefinidos$canal
  G$j1 <- "Móvil 1"; G$j2 <- "Móvil 2"
  G
}

# Ficha con la letra del canal, para la línea de tiempo.
chip_canal <- function(canal) {
  span(class = paste("chip-canal", if (canal == 1) "canal-a" else "canal-b"), if (canal == 1) "A" else "B")
}

pag_canal_ui <- function(id) {
  ns <- NS(id)
  G <- juego_canal()
  pagina("canal",
    "Dos móviles quieren enviar datos a la vez por uno de dos canales de radio. Si eligen el mismo, las señales se pisan y ninguno lo consigue. No pueden ponerse de acuerdo antes.",

    nav_panel("1. Pruébalo",
      panel(
        llenar(uiOutput(ns("carriles"))),
        tagList(
          explicacion("Elige un canal para cada móvil", p("Mira qué pasa en cada combinación.")),
          div(class = "selector-canal",
              radioButtons(ns("m1"), span(class = "j1", ico("mobile-screen-button"), " Móvil 1"),
                           c("Canal A" = 1, "Canal B" = 2), inline = TRUE),
              radioButtons(ns("m2"), span(class = "j2", ico("mobile-screen-button"), " Móvil 2"),
                           c("Canal A" = 1, "Canal B" = 2), inline = TRUE)),
          idea_clave("Hay dos formas de que salga bien: que cada móvil esté en un canal distinto.")
        ))),

    nav_panel("2. ¿Qué situaciones son estables?",
      panel(
        tagList(llenar(svg_matriz(G, flechas_j1 = 1:2, flechas_j2 = 1:2, estrellas = TRUE,
                                  textos1 = matrix(c("choca", "envía", "envía", "choca"), 2),
                                  textos2 = matrix(c("choca", "envía", "envía", "choca"), 2),
                                  corto1 = "móvil 1", corto2 = "móvil 2")),
                leyenda_matriz()),
        tagList(
          explicacion("Dos equilibrios",
            p("Si cada móvil está en un canal distinto, ninguno quiere moverse: si se cambia, chocaría."),
            p("Hay dos situaciones así (las estrellas): Móvil 1 en A y Móvil 2 en B, o al revés."),
            p("El problema no es encontrar una buena solución, sino ", negrita("ponerse de acuerdo en cuál"), " sin poder hablar.")),
          idea_clave("En los juegos de coordinación puede haber varios equilibrios. Lo difícil es que todos elijan el mismo.")
        ))),

    nav_panel("3. ¿Y si eligen al azar?",
      panel(
        tagList(llenar(uiOutput(ns("rejilla"))),
                div(class = "leyenda", span(class = "cuadro ok"), "Los dos envían.", span(class = "cuadro mal"), "Chocan. ",
                    "Cada cuadrado es una ronda.")),
        tagList(
          sliderInput(ns("p"), "Móvil 1: probabilidad de usar el canal A", 0, 100, 50, 5, post = " %", ticks = FALSE),
          sliderInput(ns("q"), "Móvil 2: probabilidad de usar el canal A", 0, 100, 50, 5, post = " %", ticks = FALSE),
          actionButton(ns("otra"), "Lanzar otras 100 rondas", icon = icon("dice"), class = "btn-outline-primary btn-sm"),
          uiOutput(ns("azar_texto"))
        ))),

    nav_panel("4. ¿Pueden aprender a coordinarse?",
      panel(
        div(class = "pila-centrada", uiOutput(ns("linea_tiempo"))),
        tagList(
          explicacion("Repiten muchas veces", p("En cada ronda, cada móvil mira qué canal ha usado más el otro hasta ahora y elige el contrario.")),
          radioButtons(ns("escenario"), "¿Cómo empiezan?", c(
            "Los dos piensan exactamente lo mismo" = "iguales",
            "Cada uno cree algo distinto del otro" = "asimetricas",
            "Piensan lo mismo, pero si dudan lanzan una moneda" = "azar")),
          uiOutput(ns("aprende_texto"))
        ))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Los pagos"), p("1 si el móvil transmite y 0 si hay interferencia."),
            svg_matriz(juegos_predefinidos$canal, estrellas = TRUE)),
        div(h3("Tres equilibrios de Nash"),
            p("Los dos puros, (A, B) y (B, A), y uno mixto en el que cada móvil elige cada canal con probabilidad ½. ",
              "En el mixto, cada uno es indiferente entre A y B (principio de indiferencia): 1 − q = q, de donde q = ½. ",
              "La probabilidad de interferencia es entonces ½.")),
        div(h3("Fictitious play"),
            p("En la pestaña 4 los móviles juegan en cada ronda la mejor respuesta a la frecuencia con la que el otro ha usado ",
              "cada canal. Si empiezan con las mismas creencias y deshacen los empates igual, eligen siempre el mismo canal: fallan en ",
              "todas las rondas aunque sus frecuencias tiendan a (½, ½), el equilibrio mixto."))
      ))
  )
}

pag_canal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    G <- juego_canal()

    # Pestaña 1: los dos canales con los móviles que hay en cada uno.
    output$carriles <- renderUI({
      a <- as.integer(input$m1 %||% 1); b <- as.integer(input$m2 %||% 1)
      choca <- a == b
      movil <- function(k) span(class = paste("movil", if (k == 1) "j1" else "j2"), ico("mobile-screen-button"), paste("Móvil", k))
      carril <- function(c) {
        dentro <- c(if (a == c) 1, if (b == c) 2)
        div(class = paste("carril", if (c == 1) "canal-a" else "canal-b", if (choca && a == c) "choque"),
            span(class = "carril-nombre", if (c == 1) "Canal A" else "Canal B"),
            div(class = "carril-moviles", lapply(dentro, movil),
                if (choca && a == c) span(class = "explosion", ico("burst"))))
      }
      div(class = "carriles", carril(1), carril(2),
          div(class = paste("veredicto", if (choca) "mal" else "ok"),
              ico(if (choca) "burst" else "circle-check"),
              if (choca) "Interferencia: ningún mensaje llega. Los dos obtienen 0."
              else "Los dos mensajes llegan. Los dos obtienen 1."))
    })

    # Pestaña 3: 100 rondas en las que cada móvil elige al azar con las
    # probabilidades de los deslizadores.
    semilla <- reactiveVal(1)
    observeEvent(input$otra, semilla(semilla() + 1))
    rondas <- reactive({
      set.seed(semilla())
      p <- input$p / 100; q <- input$q / 100
      a <- ifelse(runif(100) < p, 1, 2); b <- ifelse(runif(100) < q, 1, 2)
      list(a = a, b = b, choca = a == b, teoria = p * q + (1 - p) * (1 - q))
    })

    output$rejilla <- renderUI({
      r <- rondas()
      div(class = "rejilla-100", lapply(seq_along(r$choca), function(k) {
        span(class = paste("cuadro", if (r$choca[k]) "mal" else "ok"),
             title = sprintf("Ronda %d: Móvil 1 en %s, Móvil 2 en %s", k, LETTERS[r$a[k]], LETTERS[r$b[k]]))
      }))
    })

    output$azar_texto <- renderUI({
      r <- rondas(); p <- input$p; q <- input$q
      tagList(
        div(class = "cifras",
            cifra(pct(mean(r$choca)), "Rondas con choque", COL_MAL),
            cifra(pct(r$teoria), "Lo que dice la probabilidad")),
        if (p == 50 && q == 50) explicacion(NULL,
          p("Cada móvil lanza una moneda: chocan la mitad de las veces. Curiosamente, ", negrita("también es un equilibrio"),
            ": si el otro elige al azar, a ti te da igual el canal. Es estable, pero desperdicia la mitad de las rondas."))
        else explicacion(NULL, p("Cuanto más distintas sean sus preferencias, menos chocan. Prueba con 100 % y 0 %."))
      )
    })

    # Pestaña 4: fictitious play partiendo de distintas creencias iniciales.
    sim <- reactive({
      esc <- input$escenario
      c1 <- if (esc == "asimetricas") c(2, 1) else c(1, 1)
      c2 <- if (esc == "asimetricas") c(1, 2) else c(1, 1)
      simular_aprendizaje(G, 200, "fictitious", c1, c2,
                          if (esc == "azar") "aleatorio" else "primera", semilla = 4)
    })

    output$linea_tiempo <- renderUI({
      r <- sim(); k <- seq_len(24)
      fila <- function(nombre, clase, contenido) div(class = "lt-fila", span(class = paste("lt-nombre", clase), nombre), contenido)
      tagList(
        div(class = "linea-tiempo",
          fila("Móvil 1", "j1", lapply(k, function(t) chip_canal(r$a1[t]))),
          fila("Móvil 2", "j2", lapply(k, function(t) chip_canal(r$a2[t]))),
          fila("Resultado", "", lapply(k, function(t) {
            choca <- r$a1[t] == r$a2[t]
            span(class = paste("chip-res", if (choca) "mal" else "ok"), ico(if (choca) "burst" else "check"))
          })),
          fila("Ronda", "", lapply(k, function(t) span(class = "chip-num", t)))),
        div(class = "leyenda", "Las 24 primeras rondas. A y B: el canal que usa cada móvil."),
        div(class = "cifras", style = "margin-top:1rem",
            cifra(pct(mean(r$a1 != r$a2)), "Rondas sin choque (de 200)", if (mean(r$a1 != r$a2) > 0.5) COL_OK else COL_MAL),
            cifra(pct(mean(r$a1 == 1)), "Móvil 1 usa el canal A"),
            cifra(pct(mean(r$a2 == 1)), "Móvil 2 usa el canal A"))
      )
    })

    output$aprende_texto <- renderUI({
      switch(input$escenario,
        iguales = explicacion("Siempre chocan",
          p("Como piensan igual, en cada ronda toman la misma decisión: los dos al A, luego los dos al B, y así sin parar."),
          p("Cada uno usa cada canal la mitad de las veces, pero ", negrita("no aciertan nunca"), ".")),
        asimetricas = explicacion("Se coordinan enseguida",
          p("Una pequeña diferencia al principio basta: cada uno se va a un canal y, como funciona, se quedan ahí."),
          p("Han aprendido a repartirse los canales sin hablar.")),
        azar = explicacion("Un poco de azar lo arregla",
          p("Al principio dudan y lanzan una moneda. En cuanto aciertan una vez, cada uno se queda en su canal."),
          p("Un poco de azar sirve para romper la simetría."))
      )
    })
  })
}
