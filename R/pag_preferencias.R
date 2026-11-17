# =============================================================================
# Preferencias y utilidad
# =============================================================================

# Alternativas de la pestaña 1.
COMIDAS <- list(
  pizza = list(icono = "pizza-slice", nombre = "Pizza"),
  sushi = list(icono = "fish", nombre = "Sushi"),
  burger = list(icono = "burger", nombre = "Hamburguesa"),
  ensalada = list(icono = "leaf", nombre = "Ensalada")
)

# Tres actitudes ante el riesgo, cada una con su función de utilidad.
PERSONAS <- list(
  neutral = list(nombre = "Neutral", icono = "scale-balanced", f = function(x) x,
                 desc = "Le da igual el riesgo: cada euro vale lo mismo.", formula = "u(x) = x"),
  prudente = list(nombre = "Prudente", icono = "shield-halved", f = function(x) sqrt(x),
                  desc = "No le gusta arriesgar: perder le duele más de lo que le alegra ganar.", formula = "u(x) = √x"),
  atrevida = list(nombre = "Atrevida", icono = "rocket", f = function(x) x^3,
                  desc = "Le encanta arriesgar: los premios grandes le ilusionan muchísimo.", formula = "u(x) = x³")
)

pag_preferencias_ui <- function(id) {
  ns <- NS(id)
  pagina("preferencias",
    "Antes de jugar hay que saber qué quiere cada jugador. La utilidad es una forma de ponerle nota a cada resultado: cuanto más te gusta, más nota.",

    nav_panel("1. Ordena lo que prefieres",
      panel(
        uiOutput(ns("podio"), class = "podio-salida"),
        tagList(
          explicacion("Pon un puesto a cada comida", p("Si dos te gustan igual, dales el mismo puesto.")),
          div(class = "rejilla-comidas",
            lapply(names(COMIDAS), function(k) {
              cm <- COMIDAS[[k]]
              div(class = "comida", span(class = "comida-ico", ico(cm$icono)),
                  selectInput(ns(paste0("r_", k)), cm$nombre, c("1.º" = 1, "2.º" = 2, "3.º" = 3, "4.º" = 4),
                              selected = switch(k, pizza = 1, sushi = 2, burger = 2, ensalada = 4)))
            })),
          uiOutput(ns("podio_texto")),
          idea_clave("La nota solo sirve para ordenar. Si multiplicas todas las notas por 10 o las elevas al cubo, ",
                     "el orden no cambia y representan las mismas preferencias.")
        ))),

    nav_panel("2. Decidir cuando hay riesgo",
      panel(
        uiOutput(ns("personas"), class = "personas-salida"),
        tagList(
          explicacion("¿Dinero seguro o lotería?",
            p("La utilidad de una lotería es la ", negrita("media"), " de las utilidades de sus resultados, pesando cada una por su probabilidad."),
            p("Cada persona valora el dinero a su manera, así que pueden decidir distinto.")),
          sliderInput(ns("seguro"), "Opción segura: te dan", 0, 2, 1.2, 0.1, post = " €"),
          sliderInput(ns("prob"), "Lotería: probabilidad de ganar 2 € (si no, 0 €)", 0, 100, 50, 5, post = " %")
        ))),

    nav_panel("3. Cambiar de unidades",
      panel(
        uiOutput(ns("unidades"), class = "unidades-salida"),
        tagList(
          explicacion("Estirar sí, deformar no",
            p("Contar en céntimos en vez de en euros (multiplicar por 100) o sumar 10 € a todos los resultados no cambia ",
              "ninguna decisión: es como cambiar de escala un termómetro."),
            p("Pero elevar al cubo deforma la escala: ya no es la misma persona, y puede preferir la otra opción."),
            p("Las cantidades son las de la pestaña 2.")),
          idea_clave("Con riesgo, la utilidad solo se puede estirar y desplazar, no deformar.")
        ))),

    nav_panel("Matemáticas",
      panel_curiosos(
        div(h3("Preferencias y utilidad ordinal"),
            p("Una relación de preferencia ⪰ es ", negrita("completa"), " (siempre se pueden comparar dos alternativas) y ",
              negrita("transitiva"), " (si a ⪰ b y b ⪰ c, entonces a ⪰ c). En un conjunto finito, u(a) = |{b : a ⪰ b}| ",
              "(el número de alternativas que a iguala o supera) es una función de utilidad: es la de la pestaña 1.")),
        div(h3("Teorema de von Neumann-Morgenstern"),
            p("Si las preferencias entre loterías son completas, transitivas, continuas y cumplen el axioma de independencia, existe u ",
              "tal que una lotería L = (p₁, …, pₙ) se prefiere a L′ = (q₁, …, qₙ) si y solo si Σ pᵢ u(aᵢ) ≥ Σ qᵢ u(aᵢ).")),
        div(h3("Unicidad"),
            p("Esa u es única salvo transformaciones afines positivas: v = α·u + β con α > 0. Una transformación solo creciente, ",
              "como u³, conserva el orden de los resultados seguros pero puede invertir el de las loterías."))
      ))
  )
}

pag_preferencias_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Puesto elegido para cada comida (1 = la favorita).
    puestos <- reactive({
      v <- vapply(names(COMIDAS), function(k) as.numeric(input[[paste0("r_", k)]] %||% NA), numeric(1))
      validate(need(all(!is.na(v)), ""))
      v
    })

    # u = número de comidas con un puesto igual o peor (Proposición 1.1).
    output$podio <- renderUI({
      r <- puestos()
      u <- vapply(r, function(x) sum(r >= x), numeric(1))
      div(class = "podio",
        lapply(order(-u), function(i) {
          cm <- COMIDAS[[names(COMIDAS)[i]]]
          div(class = "podio-fila",
              span(class = "podio-ico", ico(cm$icono)),
              span(class = "podio-nombre", cm$nombre),
              div(class = "bo-pista", div(class = "bo-relleno j1", style = sprintf("width:%g%%", 100 * u[i] / 4))),
              span(class = "podio-nota", sprintf("%d %s", u[i], if (u[i] == 1) "punto" else "puntos")))
        }))
    })

    output$podio_texto <- renderUI({
      r <- puestos()
      u <- vapply(r, function(x) sum(r >= x), numeric(1))
      top <- names(COMIDAS)[which.max(u)]
      explicacion(NULL,
        p("Cada comida recibe tantos puntos como comidas iguala o supera, contándose a sí misma."),
        p(sprintf("Por ejemplo, la %s tiene %d: es al menos tan buena como %d de las 4.",
                  tolower(COMIDAS[[top]]$nombre), max(u), max(u))))
    })

    # Pestaña 2: utilidad del dinero seguro frente a la utilidad esperada de la lotería.
    output$personas <- renderUI({
      s <- input$seguro %||% 1.2; p <- (input$prob %||% 50) / 100
      div(class = "personas",
        lapply(PERSONAS, function(pe) {
          us <- pe$f(s); ul <- p * pe$f(2) + (1 - p) * pe$f(0)
          maxv <- max(us, ul, 1e-9)
          elige <- if (abs(us - ul) < 1e-9) "igual" else if (us > ul) "segura" else "loteria"
          barra <- function(nombre, v, gana) {
            div(class = paste("persona-barra", if (gana) "gana"),
                span(nombre),
                div(class = "bo-pista", div(class = "bo-relleno eq", style = sprintf("width:%g%%", 100 * v / maxv))),
                span(class = "pb-num", fmt(v)))
          }
          div(class = "persona",
              div(class = "persona-cab", span(class = "persona-ico", ico(pe$icono)),
                  div(div(class = "persona-nombre", pe$nombre), div(class = "persona-desc", pe$desc))),
              barra("Segura", us, elige == "segura"),
              barra("Lotería", ul, elige == "loteria"),
              div(class = "persona-decide",
                  if (elige == "igual") "Le da igual." else if (elige == "segura") "Prefiere el dinero seguro." else "Prefiere arriesgarse."),
              div(class = "nota", pe$formula))
        }))
    })

    # Pestaña 3: las mismas cantidades con utilidades transformadas.
    output$unidades <- renderUI({
      s <- input$seguro %||% 1.2; p <- (input$prob %||% 50) / 100
      fs <- list("Euros (la persona neutral)" = function(x) x,
                 "Céntimos: ×100" = function(x) 100 * x,
                 "Sumando 10 € a todo" = function(x) x + 10,
                 "Elevando al cubo" = function(x) x^3)
      div(class = "unidades",
        lapply(names(fs), function(nm) {
          f <- fs[[nm]]; us <- f(s); ul <- p * f(2) + (1 - p) * f(0)
          dec <- if (abs(us - ul) < 1e-9) "le da igual" else if (us > ul) "segura" else "lotería"
          div(class = paste("unidad-fila", if (nm == "Elevando al cubo") "deforma"),
              span(class = "uf-nombre", nm),
              span(class = "uf-vals", sprintf("segura %s, lotería %s", fmt(us), fmt(ul))),
              span(class = "uf-dec", dec))
        }))
    })
  })
}
