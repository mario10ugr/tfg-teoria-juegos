# =============================================================================
# Crea tu propio juego
# =============================================================================

# Juegos de ejemplo que se pueden cargar en el editor.
PLANTILLAS <- list(
  pares = list(G = juegos_predefinidos$pares, nombre = "Pares o nones"),
  prisionero = list(G = { g <- juegos_predefinidos$prisionero; g$filas <- g$columnas <- c("Callar", "Declarar"); g },
                    nombre = "Prisionero"),
  canal = list(G = { g <- juegos_predefinidos$canal; g$filas <- g$columnas <- c("A", "B"); g }, nombre = "Canal"),
  turnos = list(G = { g <- juegos_predefinidos$secuencial; g$filas <- c("Izquierda", "Derecha"); g$columnas <- c("Arriba", "Abajo"); g },
                nombre = "Por turnos (en tabla)"),
  ppt = list(G = juego(matrix(c(0, -1, 1, 1, 0, -1, -1, 1, 0), 3, byrow = TRUE),
                       -matrix(c(0, -1, 1, 1, 0, -1, -1, 1, 0), 3, byrow = TRUE),
                       c("Piedra", "Papel", "Tijera"), c("Piedra", "Papel", "Tijera")),
             nombre = "Piedra, papel o tijera")
)

# k tonos del color de un jugador, del más intenso al más claro.
colores_opciones <- function(k, base) {
  claro <- grDevices::colorRampPalette(c(base, "white"))(5)[4]
  grDevices::colorRampPalette(c(base, claro))(k)
}

pag_tu_juego_ui <- function(id) {
  ns <- NS(id)
  pagina("tujuego",
    "Inventa un juego de dos jugadores que eligen a la vez: cuántas opciones tiene cada uno y qué gana cada uno en cada caso. La aplicación busca los equilibrios y te explica lo que encuentra.",

    nav_panel("1. Escribe los premios",
      panel(
        div(class = "pila-centrada",
            uiOutput(ns("rejilla")),
            div(class = "leyenda", style = "text-align:center",
                "En cada casilla: arriba lo que gana el ", span(class = "j1", "Jugador 1", .noWS = "outside"),
                ", abajo lo que gana el ", span(class = "j2", "Jugador 2", .noWS = "outside"), ". Admite negativos y decimales.")),
        tagList(
          div(tags$label(class = "control-label", "Empezar desde un ejemplo"),
              div(class = "fila-botones plantillas",
                  lapply(names(PLANTILLAS), function(k) actionButton(ns(paste0("pl_", k)), PLANTILLAS[[k]]$nombre, class = "btn-outline-secondary btn-sm")))),
          div(class = "fila-botones",
              div(style = "flex:1", numericInput(ns("m"), span(class = "j1", "Opciones del Jugador 1"), 2, 2, 4, 1)),
              div(style = "flex:1", numericInput(ns("n"), span(class = "j2", "Opciones del Jugador 2"), 2, 2, 4, 1))),
          textInput(ns("filas"), span(class = "j1", "Nombres de las opciones del Jugador 1"), "Callar, Declarar"),
          textInput(ns("columnas"), span(class = "j2", "Nombres de las opciones del Jugador 2"), "Callar, Declarar"),
          p(class = "nota", "Cuando lo tengas, mira las pestañas 2, 3 y 4.")
        ))),

    nav_panel("2. Qué acaba pasando",
      panel(tagList(llenar(uiOutput(ns("matriz"))), uiOutput(ns("leyenda"))),
            uiOutput(ns("equilibrios")))),

    nav_panel("3. Descartar lo que nunca conviene",
      panel(llenar(uiOutput(ns("matriz_elim"))),
            tagList(explicacion("Tachar opciones", p("Si una opción es peor que otra pase lo que pase, nadie sensato la elegiría. Se tacha y se vuelve a mirar.")),
                    uiOutput(ns("texto_elim")), controles_pasos(ns, "el")))),

    nav_panel("4. Si es de suma cero",
      panel(div(class = "pila-centrada", uiOutput(ns("suma_cero_barras"))),
            uiOutput(ns("suma_cero_texto")))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Equilibrio de Nash"),
            p("Es una forma de jugar en la que ningún jugador gana cambiando él solo. Puede ser en estrategias puras (siempre la misma opción) ",
              "o mixtas (cada opción con cierta probabilidad). El ", negrita("teorema de Nash"), " garantiza que todo juego finito tiene al menos uno.")),
        div(h3("Cómo se buscan los mixtos"),
            p("Por enumeración de soportes: para cada conjunto de opciones que se usan, se impone que cada jugador sea indiferente entre las suyas ",
              "(principio de indiferencia) y se comprueba que ninguna otra opción dé más. Si hay empates (juego degenerado), puede haber más equilibrios de los listados.")),
        div(h3("Dominación por mezclas"),
            p("Una opción también puede estar dominada por una mezcla de otras: se comprueba con un problema de programación lineal.")),
        div(h3("Suma cero"),
            p("Si lo que gana uno lo pierde el otro, la estrategia óptima de cada jugador y el valor del juego se calculan con programación lineal (teorema minimax)."))
      ))
  )
}

pag_tu_juego_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    G0 <- PLANTILLAS$prisionero$G
    # 'version' aumenta cada vez que se regenera la rejilla: así los controles nuevos
    # tienen identificadores distintos y no heredan valores antiguos.
    rv <- reactiveValues(A = G0$A, B = G0$B, version = 0)

    cargar <- function(G) {
      rv$A <- G$A; rv$B <- G$B; rv$version <- rv$version + 1
      updateNumericInput(session, "m", value = nrow(G$A)); updateNumericInput(session, "n", value = ncol(G$A))
      updateTextInput(session, "filas", value = paste(G$filas, collapse = ", "))
      updateTextInput(session, "columnas", value = paste(G$columnas, collapse = ", "))
    }
    # local() fija k en cada vuelta para que cada botón cargue su plantilla.
    for (k in names(PLANTILLAS)) local({
      kk <- k
      observeEvent(input[[paste0("pl_", kk)]], cargar(PLANTILLAS[[kk]]$G))
    })

    # Al cambiar el tamaño se conservan los pagos que siguen cabiendo.
    observeEvent(list(input$m, input$n), {
      m <- input$m; n <- input$n
      req(!is.na(m), !is.na(n))
      m <- max(2, min(4, m)); n <- max(2, min(4, n))
      if (m == nrow(rv$A) && n == ncol(rv$A)) return()
      A <- matrix(0, m, n); B <- matrix(0, m, n)
      mm <- min(m, nrow(rv$A)); nn <- min(n, ncol(rv$A))
      A[1:mm, 1:nn] <- rv$A[1:mm, 1:nn]; B[1:mm, 1:nn] <- rv$B[1:mm, 1:nn]
      rv$A <- A; rv$B <- B; rv$version <- rv$version + 1
    }, ignoreInit = TRUE)

    idc <- function(q, v, i, j) sprintf("%s_%d_%d_%d", q, v, i, j)

    output$rejilla <- renderUI({
      v <- rv$version; A <- isolate(rv$A); B <- isolate(rv$B)
      m <- nrow(A); n <- ncol(A)
      fil <- parse_nombres(input$filas %||% "", m, "Opción ")
      col <- parse_nombres(input$columnas %||% "", n, "Opción ")
      tags$table(class = "rejilla-pagos",
        tags$thead(tags$tr(tags$th(), lapply(col, function(cn) tags$th(class = "j2", cn)))),
        tags$tbody(lapply(seq_len(m), function(i) tags$tr(
          tags$th(class = "j1", fil[i]),
          lapply(seq_len(n), function(j) tags$td(
            div(class = "celda-entrada j1", numericInput(ns(idc("a", v, i, j)), NULL, A[i, j], step = 1, width = "96px")),
            div(class = "celda-entrada j2", numericInput(ns(idc("b", v, i, j)), NULL, B[i, j], step = 1, width = "96px"))))
        ))))
    })

    # Lee los pagos de la rejilla cuando están todos completos.
    observe({
      v <- rv$version; m <- nrow(isolate(rv$A)); n <- ncol(isolate(rv$A))
      A <- matrix(NA_real_, m, n); B <- matrix(NA_real_, m, n)
      for (i in seq_len(m)) for (j in seq_len(n)) {
        a <- input[[idc("a", v, i, j)]]; b <- input[[idc("b", v, i, j)]]
        if (is.null(a) || is.null(b) || is.na(a) || is.na(b)) return()
        A[i, j] <- a; B[i, j] <- b
      }
      if (!identical(A, isolate(rv$A)) || !identical(B, isolate(rv$B))) { rv$A <- A; rv$B <- B }
    })

    G <- reactive({
      A <- rv$A; B <- rv$B
      juego(A, B, parse_nombres(input$filas %||% "", nrow(A), "Opción "), parse_nombres(input$columnas %||% "", ncol(A), "Opción "))
    })
    es22 <- reactive(nrow(G()$A) == 2 && ncol(G()$A) == 2)

    output$matriz <- renderUI({
      svg_matriz(G(), estrellas = TRUE, flechas_j1 = if (es22()) 1:2 else integer(0),
                 flechas_j2 = if (es22()) 1:2 else integer(0))
    })
    output$leyenda <- renderUI(leyenda_matriz(con_flechas = es22()))

    output$equilibrios <- renderUI({
      g <- G()
      puros <- equilibrios_puros(g)
      # Los mixtos que en realidad son puros ya aparecen en 'puros'.
      mixtos <- Filter(function(e) max(e$x) < 1 - 1e-9 || max(e$y) < 1 - 1e-9, equilibrios_mixtos(g))
      tagList(
        explicacion(
          if (nrow(puros) == 0) "Ninguna casilla es estable"
          else if (nrow(puros) == 1) "Hay una casilla estable" else sprintf("Hay %d casillas estables", nrow(puros)),
          if (nrow(puros)) tagList(
            p("En ", if (nrow(puros) == 1) "esta casilla" else "estas casillas", " (con estrella) ningún jugador gana cambiando él solo:"),
            tags$ul(lapply(seq_len(nrow(puros)), function(k) tags$li(
              sprintf("%s: el Jugador 1 gana %s y el Jugador 2, %s.", puros$Perfil[k], puros$`Pago J1`[k], puros$`Pago J2`[k])))))
          else p("Desde cualquier casilla, algún jugador quiere cambiar. Hay que mezclar al azar.")),
        if (length(mixtos)) explicacion(
          if (length(mixtos) == 1) "Y una forma de jugar mezclando al azar" else sprintf("Y %d formas de jugar mezclando al azar", length(mixtos)),
          lapply(mixtos, function(e) div(
            div(class = "mixto-quien j1", "Jugador 1"), barra_mixta(e$x, g$filas, colores_opciones(length(e$x), COL_J1)),
            div(class = "mixto-quien j2", "Jugador 2"), barra_mixta(e$y, g$columnas, colores_opciones(length(e$y), COL_J2)),
            p(class = "nota", sprintf("Ganancia media: %s para el Jugador 1 y %s para el Jugador 2.", fmt(e$u1), fmt(e$u2)))))),
        if (es_degenerado(g)) p(class = "nota", "Ojo: hay empates en los premios, así que puede haber más equilibrios de los que se muestran.")
      )
    })

    # Pestaña 3: eliminación iterativa, un paso por estrategia tachada.
    elim <- reactive(eliminacion_iterativa(G()))
    paso <- servidor_pasos(input, output, session, reactive(max(1, NROW(elim()$pasos)) + 1), "el")

    output$matriz_elim <- renderUI({
      g <- G(); e <- elim(); k <- paso() - 1
      at <- matrix(FALSE, nrow(g$A), ncol(g$A))
      if (k > 0 && !is.null(e$pasos)) for (q in seq_len(min(k, nrow(e$pasos)))) {
        f <- e$pasos[q, ]
        if (f$Jugador == g$j1) at[match(f$Elimina, g$filas), ] <- TRUE else at[, match(f$Elimina, g$columnas)] <- TRUE
      }
      svg_matriz(g, atenuadas = at, estrellas = FALSE)
    })

    output$texto_elim <- renderUI({
      g <- G(); e <- elim(); k <- paso() - 1
      if (is.null(e$pasos)) return(explicacion("No se puede descartar nada",
        p("Ninguna opción es peor que otra en todos los casos. Este método no ayuda en este juego.")))
      if (k == 0) return(explicacion("Empecemos", p("Pulsa «Siguiente paso».")))
      if (k > nrow(e$pasos)) {
        quedan <- sprintf("%s para el Jugador 1 y %s para el Jugador 2", paste(g$filas[e$filas], collapse = ", "),
                          paste(g$columnas[e$columnas], collapse = ", "))
        return(explicacion("Ya no se puede tachar nada más", p("Quedan: ", quedan, "."),
          if (length(e$filas) == 1 && length(e$columnas) == 1) p(negrita("Ha quedado una sola casilla: es lo que harán dos jugadores sensatos."))))
      }
      f <- e$pasos[k, ]
      explicacion(sprintf("Se tacha «%s»", f$Elimina),
        p(sprintf("Para el %s, «%s» da siempre más que «%s», haga lo que haga el otro.", f$Jugador, f$`Dominada por`, f$Elimina)),
        p("Nadie sensato elegiría la opción tachada, así que podemos olvidarnos de ella."))
    })

    # Pestaña 4: si el juego no es de suma cero, se marca una casilla que lo incumple.
    output$suma_cero_barras <- renderUI({
      g <- G()
      if (!es_suma_cero(g)) {
        S <- g$A + g$B
        idx <- which(abs(S) > 1e-9, arr.ind = TRUE)[1, , drop = TRUE]
        return(tagList(
          svg_matriz(g, marcos = list(list(i = idx[1], j = idx[2], color = COL_MAL, discontinuo = TRUE))),
          div(class = "leyenda", sprintf("En la casilla marcada los pagos suman %s, no 0: lo que gana uno no es lo que pierde el otro.",
                                         como_fraccion(S[idx[1], idx[2]])))))
      }
      s <- valor_suma_cero(g$A)
      div(div(class = "mixto-quien j1", "Lo mejor para el Jugador 1"), barra_mixta(s$x, g$filas, colores_opciones(length(s$x), COL_J1)),
          div(class = "mixto-quien j2", style = "margin-top:1rem", "Lo mejor para el Jugador 2"),
          barra_mixta(s$y, g$columnas, colores_opciones(length(s$y), COL_J2)))
    })

    output$suma_cero_texto <- renderUI({
      g <- G()
      if (!es_suma_cero(g)) return(explicacion("No es de suma cero",
        p("Un juego es de suma cero cuando en todas las casillas los dos pagos suman 0. Aquí no pasa, así que no hay un «valor del juego»."),
        p("Prueba con la plantilla «Pares o nones» o «Piedra, papel o tijera», o inventa un juego en el que el premio de uno sea siempre el contrario del otro.")))
      s <- valor_suma_cero(g$A)
      tagList(
        explicacion(sprintf("Valor del juego: %s", como_fraccion(s$v)),
          p(sprintf("Jugando así, el Jugador 1 se asegura ganar al menos %s de media, y el Jugador 2 se asegura no perder más de eso.", como_fraccion(s$v))),
          p("Se calcula resolviendo un problema de programación lineal.")),
        idea_clave("En los juegos de suma cero siempre existe una forma óptima de jugar, aunque a veces haya que mezclar al azar."))
    })
  })
}
