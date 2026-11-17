# =============================================================================
# Dilema del prisionero
# =============================================================================

# Pagos como años de condena (-5 -> "5 años").
anios <- function(x) {
  vapply(x, function(v) {
    if (v == 0) "Libre" else if (v == -1) "1 año" else paste(-v, "años")
  }, character(1))
}

# El juego del Ejemplo 1.4 con nombres más cercanos para la interfaz.
juego_prisionero <- function() {
  G <- juegos_predefinidos$prisionero
  G$filas <- c("Callar", "Declarar"); G$columnas <- c("Callar", "Declarar")
  G$j1 <- "Tú (Prisionero 1)"; G$j2 <- "Tu compañero (Prisionero 2)"
  G
}

# Textos de la explicación paso a paso (pestaña 2).
PASOS_PRISIONERO <- list(
  list(titulo = "Así es el trato",
       texto = list(
         "Cada casilla dice cuántos años de cárcel le caen a cada uno.",
         "Azul: a ti. Granate: a tu compañero. Por ejemplo, si los dos calláis, 1 año para cada uno.",
         "Lo malo: no puedes hablar con tu compañero antes de decidir.")),
  list(titulo = "Imagina que tu compañero calla",
       texto = list(
         "Mira solo la columna «Callar».",
         "Si tú también callas: 1 año. Si declaras: sales libre.",
         "Te conviene declarar (flecha azul).")),
  list(titulo = "Imagina que tu compañero declara",
       texto = list(
         "Ahora mira la columna «Declarar».",
         "Si callas: 10 años. Si declaras: 5 años.",
         "Otra vez te conviene declarar.")),
  list(titulo = "Declarar gana siempre",
       texto = list(
         "Haga lo que haga tu compañero, a ti te va mejor declarando.",
         "Se dice que «declarar» es una estrategia dominante y que «callar» está dominada: no tiene sentido elegirla.")),
  list(titulo = "Tu compañero piensa lo mismo",
       texto = list(
         "Él está en la misma situación, así que también declara (flechas granates).",
         "Resultado: los dos declaráis y os caen 5 años a cada uno. Es el equilibrio (estrella): ninguno gana cambiando solo.")),
  list(titulo = "La paradoja",
       texto = list(
         "Si los dos hubierais callado, solo 1 año cada uno: ¡mucho mejor para ambos!",
         "Pero esa casilla no es estable: cada uno tiene la tentación de declarar para salir libre.",
         "Pensar solo en uno mismo lleva a un resultado peor para todos."))
)

pag_prisionero_ui <- function(id) {
  ns <- NS(id)
  pagina("prisionero",
    "La policía interroga por separado a dos sospechosos y ofrece a cada uno lo mismo: si declaras contra tu compañero, te rebajamos la condena.",

    nav_panel("1. ¿Qué harías tú?",
      panel(
        div(class = "pila-centrada",
            explicacion("Ponte en su lugar",
              p("Estás en una sala de interrogatorios. Tu compañero está en otra y no podéis hablar."),
              p("Si los dos calláis, 1 año cada uno. Si los dos declaráis, 5 años cada uno."),
              p("Si uno declara y el otro calla, el que declara sale libre y el otro cumple 10 años.")),
            div(class = "botones-grandes",
                boton_eleccion(ns("callar"), "comment-slash", "Callar"),
                boton_eleccion(ns("declarar"), "bullhorn", "Declarar")),
            uiOutput(ns("respuesta"))),
        tagList(
          idea_clave("Antes de seguir, decide. Después, en la pestaña 2, verás paso a paso qué haría alguien que piensa solo en sí mismo."),
          p(class = "nota", "Este juego es uno de los más famosos de la teoría de juegos: aparece en economía, política, biología y hasta en el reciclaje.")
        ))),

    nav_panel("2. Razónalo paso a paso",
      panel(
        tagList(llenar(uiOutput(ns("matriz"))), uiOutput(ns("leyenda"))),
        tagList(uiOutput(ns("texto_paso")), controles_pasos(ns),
                idea_clave("Cuando cada uno busca solo lo mejor para sí, el resultado puede ser malo para todos. ",
                           "Por eso existen los contratos, las multas o la confianza: sirven para salir del dilema.")))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Los pagos"),
            p("Son los años de condena con signo negativo (menos años es mejor):"),
            svg_matriz(juegos_predefinidos$prisionero, estrellas = TRUE)),
        div(h3("Dominación"),
            p("Para cada jugador, ", negrita("Declarar (D) domina estrictamente a No declarar (ND)"),
              ": 0 > −1 y −5 > −10. Eliminando las estrategias dominadas queda solo (D, D), que es el único equilibrio de Nash.")),
        div(h3("Equilibrio frente a eficiencia"),
            p("(ND, ND) da (−1, −1), mejor para los dos que (−5, −5): el equilibrio no es eficiente (no es óptimo de Pareto). ",
              "Un equilibrio de Nash es estable para cada individuo, pero no tiene por qué ser lo mejor para el grupo."))
      ))
  )
}

pag_prisionero_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    eleccion <- reactiveVal(NULL)
    observeEvent(input$callar, eleccion("callar"))
    observeEvent(input$declarar, eleccion("declarar"))
    paso <- servidor_pasos(input, output, session, length(PASOS_PRISIONERO))

    output$respuesta <- renderUI({
      e <- eleccion()
      if (is.null(e)) return(NULL)
      explicacion(if (e == "callar") "Has elegido callar" else "Has elegido declarar",
        p(if (e == "callar")
          "Es lo que harías si confiaras en tu compañero. En la pestaña 2 verás qué pasa si cada uno piensa solo en sí mismo."
        else "Es justo lo que predice la teoría. En la pestaña 2 verás por qué, paso a paso."))
    })

    output$matriz <- renderUI({
      G <- juego_prisionero(); k <- paso()
      at <- matrix(FALSE, 2, 2); fl1 <- integer(0); fl2 <- integer(0); chips <- list(); marcos <- list()
      est <- FALSE
      # Qué se atenúa, qué flechas aparecen y qué pagos se rodean en cada paso.
      if (k == 2) { at[, 2] <- TRUE; fl1 <- 1; chips <- list(list(i = 1, j = 1, jugador = 1), list(i = 2, j = 1, jugador = 1)) }
      if (k == 3) { at[, 1] <- TRUE; fl1 <- 2; chips <- list(list(i = 1, j = 2, jugador = 1), list(i = 2, j = 2, jugador = 1)) }
      if (k == 4) { at[1, ] <- TRUE; fl1 <- 1:2 }
      if (k == 5) { fl1 <- 1:2; fl2 <- 1:2; est <- TRUE }
      if (k == 6) { fl1 <- 1:2; fl2 <- 1:2; est <- TRUE
                    marcos <- list(list(i = 1, j = 1, color = COL_OK, discontinuo = TRUE)) }
      svg_matriz(G, textos1 = matrix(anios(G$A), 2), textos2 = matrix(anios(G$B), 2),
                 flechas_j1 = fl1, flechas_j2 = fl2, estrellas = est, atenuadas = at,
                 chips = chips, marcos = marcos, corto1 = "tú", corto2 = "compañero")
    })

    output$leyenda <- renderUI({
      k <- paso()
      if (k == 6) div(class = "leyenda", span(class = "ley-marco"), "Lo mejor para los dos juntos, pero inestable. ",
                      span(class = "ley-estrella", "\u2605"), "Lo que acaba pasando.")
      else if (k >= 2) leyenda_matriz(con_estrella = k >= 5)
    })

    output$texto_paso <- renderUI({
      s <- PASOS_PRISIONERO[[paso()]]
      explicacion(s$titulo, lapply(s$texto, p))
    })
  })
}
