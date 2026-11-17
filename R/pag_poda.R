# =============================================================================
# Poda alfa-beta
# =============================================================================

# Textos de α y β mientras todavía valen ±∞.
alfa_texto <- function(a) if (is.infinite(a)) "nada aún" else como_fraccion(a)
beta_texto <- function(b) if (is.infinite(b)) "sin límite" else como_fraccion(b)

pag_poda_ui <- function(id) {
  ns <- NS(id)
  pagina("poda",
    "Minimax mira todas las hojas. Pero a menudo, a mitad de camino, ya se sabe que una rama no va a cambiar la decisión: entonces se corta y no se mira.",

    nav_panel("1. Míralo paso a paso",
      panel(
        llenar(uiOutput(ns("arbol"))),
        tagList(
          div(class = "frase-grande", "«Ya tengo asegurado un 5. Si en otra opción mi rival me puede dejar en 3, no necesito mirar más esa opción.»"),
          div(class = "fila-botones",
              radioButtons(ns("arbol_tipo"), NULL, c("4 hojas" = "p", "8 hojas" = "m", "27 hojas" = "g"), inline = TRUE),
              actionButton(ns("otro"), "Otros números", icon = icon("shuffle"), class = "btn-outline-primary btn-sm")),
          uiOutput(ns("marcadores")),
          uiOutput(ns("texto_paso")), controles_pasos(ns)
        ), ancho = 430)),

    nav_panel("2. ¿Importa el orden?",
      panel(
        div(class = "pila-centrada", uiOutput(ns("orden_barras"))),
        tagList(
          explicacion("Mucho", p("Si se miran primero las mejores jugadas, se poda muchísimo más.")),
          sliderInput(ns("b"), "Jugadas posibles en cada turno", 2, 6, 4, 1, ticks = FALSE),
          sliderInput(ns("m"), "Turnos que se miran hacia delante", 2, 6, 5, 1, ticks = FALSE),
          uiOutput(ns("orden_texto")),
          idea_clave("La poda nunca cambia la decisión: llega al mismo resultado que minimax mirando menos.")
        ))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Las dos cotas"),
            tags$ul(
              tags$li(negrita("α"), ": el mejor valor que MAX ya tiene asegurado en el camino actual."),
              tags$li(negrita("β"), ": el mejor valor que MIN ya tiene asegurado, es decir, lo máximo que dejará conseguir a MAX.")),
            p("En cuanto α ≥ β en un nodo, se dejan de explorar sus hijos restantes.")),
        div(h3("Corrección"),
            p("Se demuestra (Knuth y Moore) que el valor de la raíz es exactamente el de minimax. Si el valor devuelto por un nodo cae ",
              "fuera de (α, β), solo es una cota: por eso en el dibujo aparecen valores como «≤ 3».")),
        div(h3("El mejor caso"),
            p("Con el orden perfecto y b jugadas por turno durante m turnos se evalúan exactamente b^⌈m/2⌉ + b^⌊m/2⌋ − 1 hojas ",
              "en lugar de b^m: es como reducir b a √b, o mirar el doble de profundo con el mismo esfuerzo. Con orden aleatorio ",
              "se evalúan del orden de b^(3m/4) (para b moderado)."),
            uiOutput(ns("formula")))
      ))
  )
}

pag_poda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    semilla <- reactiveVal(2)
    observeEvent(input$otro, semilla(sample.int(1e5, 1)))
    # El primer árbol pequeño es fijo (el de la frase de la pestaña); el resto, al azar.
    arbol <- reactive({
      tipo <- input$arbol_tipo %||% "p"
      if (tipo == "p" && semilla() == 2) return(arbol_completo(2, 2, c(5, 8, 3, 9)))
      set.seed(semilla())
      b <- if (tipo == "g") 3 else 2; m <- if (tipo == "p") 2 else 3
      arbol_completo(b, m, sample(1:9, b^m, replace = TRUE))
    })
    ab <- reactive(alfabeta_eventos(arbol()))
    total_hojas <- reactive(sum(aplanar_arbol(arbol())$es_hoja))
    paso <- servidor_pasos(input, output, session, reactive(length(ab()$eventos) + 1), intervalo = 1000)
    estado <- reactive(estado_alfabeta(ab()$eventos, paso() - 1))

    output$arbol <- renderUI({
      df <- aplanar_arbol(arbol(), suma_cero = TRUE); e <- estado()
      decision <- df$id[!df$es_hoja]
      valores <- e$valor[names(e$valor) %in% decision]
      pendientes <- setdiff(df$id, c(e$visitados, e$podados))
      nh <- sum(df$es_hoja)
      svg_arbol(df, suma_cero = TRUE, elegidas = e$elegidas, podadas = e$podados,
                pendientes = pendientes, activo = e$activo, valores = valores,
                ux = if (nh > 12) 54 else if (nh > 4) 104 else 150, uy = if (nh > 12) 108 else 112)
    })

    output$marcadores <- renderUI({
      e <- estado(); ev <- e$evento
      hojas <- if (is.null(ev)) 0 else ev$hojas
      alfa <- if (is.null(ev)) -Inf else ev$alfa; beta <- if (is.null(ev)) Inf else ev$beta
      div(class = "marcadores",
        div(class = "marcador j1", div(class = "mc-texto", "α: MAX ya tiene asegurado"), div(class = "mc-valor", alfa_texto(alfa))),
        div(class = "marcador j2", div(class = "mc-texto", "β: MIN no dejará pasar de"), div(class = "mc-valor", beta_texto(beta))),
        div(class = "marcador neutro", div(class = "mc-texto", "Hojas miradas"), div(class = "mc-valor", sprintf("%d de %d", hojas, total_hojas()))))
    })

    output$texto_paso <- renderUI({
      r <- ab(); k <- paso() - 1; n <- length(r$eventos)
      if (k == 0) return(explicacion("Empezamos",
        p("Se recorre el árbol como minimax, pero apuntando lo que MAX ya tiene asegurado (α) y lo máximo que MIN le va a permitir (β)."),
        p("Los nodos grises todavía no se han mirado.")))
      ev <- r$eventos[[k]]
      quien <- function(es_max) if (es_max) "MAX" else "MIN"
      n_restantes <- function(ids) {
        df <- aplanar_arbol(arbol()); sum(df$es_hoja & df$id %in% ids)
      }
      switch(ev$tipo,
        entra = explicacion(if (ev$ruta == "raíz") "Arriba del todo" else sprintf("Bajamos al nodo %s", ev$ruta),
          p(sprintf("Le toca a %s, que quiere el número más %s.", quien(ev$es_max), if (ev$es_max) "alto" else "bajo"))),
        hoja = explicacion(sprintf("Miramos la hoja %s", ev$ruta), p(sprintf("Vale %s.", como_fraccion(ev$v)))),
        poda = {
          nr <- n_restantes(ev$podados)
          quedan <- if (nr == 1) "la hoja que queda" else sprintf("las %d hojas que quedan", nr)
          if (!ev$es_max) explicacion(span(ico("scissors"), " Se poda"),
            p(sprintf("En este nodo MIN ya puede dejar a MAX en %s o menos.", como_fraccion(ev$v))),
            p(sprintf("Pero MAX tiene asegurado %s por otro camino, así que nunca vendrá aquí.", como_fraccion(ev$alfa))),
            p(negrita(sprintf("No hace falta mirar %s.", quedan))))
          else explicacion(span(ico("scissors"), " Se poda"),
            p(sprintf("En este nodo MAX ya puede conseguir %s o más.", como_fraccion(ev$v))),
            p(sprintf("Pero MIN puede impedir que pase de %s por otro camino, así que nunca dejará que se llegue aquí.", como_fraccion(ev$beta))),
            p(negrita(sprintf("No hace falta mirar %s.", quedan))))
        },
        devuelve = if (k == n) explicacion("Terminado",
            p(sprintf("El valor del juego es %s, el mismo que daría minimax.", como_fraccion(ev$v))),
            p(negrita(sprintf("Se han mirado %d de %d hojas.", r$hojas, total_hojas())),
              if (r$hojas < total_hojas()) sprintf(" Nos hemos ahorrado el %s del trabajo.", pct(1 - r$hojas / total_hojas()))
              else " Esta vez no se ha podado nada: prueba con otros números."))
          else explicacion(sprintf("El nodo %s ya está decidido", ev$ruta),
            p(if (ev$cota) sprintf("Solo sabemos que vale %s %s, pero es suficiente.", if (ev$es_max) "como poco" else "como mucho", como_fraccion(ev$v))
              else sprintf("Vale %s. Se lo comunica al nodo de arriba.", como_fraccion(ev$v))))
      )
    })

    # Pestaña 2: hojas evaluadas según el orden de exploración. El orden «al azar»
    # es la media de 5 árboles.
    orden <- reactive({
      b <- input$b; m <- input$m
      nat <- mean(vapply(1:5, function(s) hojas_evaluadas_alfabeta(b, m, "natural", s), numeric(1)))
      list(total = b^m, peor = hojas_evaluadas_alfabeta(b, m, "peor", 1), azar = nat,
           mejor = hojas_evaluadas_alfabeta(b, m, "mejor", 1), km = knuth_moore(b, m))
    })

    output$orden_barras <- renderUI({
      o <- orden()
      fila <- function(nombre, valor, clase) {
        div(class = "barra-orden",
            span(class = "bo-nombre", nombre),
            div(class = "bo-pista", div(class = paste("bo-relleno", clase), style = sprintf("width:%g%%", max(0.8, 100 * valor / o$total)))),
            span(class = "bo-num", miles(valor), span(class = "bo-pct", pct(valor / o$total))))
      }
      div(class = "barras-orden",
          fila("Sin podar (minimax)", o$total, "gris"),
          fila("Poda, mirando primero las peores", o$peor, "mal"),
          fila("Poda, en orden al azar", o$azar, "eq"),
          fila("Poda, mirando primero las mejores", o$mejor, "ok"),
          div(class = "leyenda", "Hojas que hay que mirar. La barra «en orden al azar» es la media de 5 árboles con números al azar."))
    })

    output$orden_texto <- renderUI({
      o <- orden()
      explicacion(NULL,
        p(sprintf("El árbol tiene %s hojas. Mirando primero las mejores jugadas basta con revisar el ", miles(o$total)),
          negrita(pct(o$mejor / o$total, 1)), ": es como pensar el doble de jugadas por delante con el mismo esfuerzo."))
    })

    output$formula <- renderUI({
      o <- orden()
      p(sprintf("Con b = %d y m = %d: b^⌈m/2⌉ + b^⌊m/2⌋ − 1 = %s, y el experimento con el mejor orden ha evaluado %s hojas.",
                input$b, input$m, miles(o$km), miles(o$mejor)))
    })
  })
}
