# =============================================================================
# Teoría de Juegos: laboratorio interactivo
# Mario Martín Rodríguez, Universidad de Granada
#
# Para lanzarla: abre este fichero en RStudio y pulsa «Run App».
# Shiny carga automáticamente todos los ficheros de la carpeta R/.
# =============================================================================

# Al arrancar se comprueba que están instalados los paquetes necesarios.
paquetes <- c("shiny", "bslib", "lpSolve", "fontawesome")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) {
  stop("Faltan paquetes. Instálalos con:\n  install.packages(c(",
       paste0('"', faltan, '"', collapse = ", "), "))", call. = FALSE)
}

library(shiny)
library(bslib)

# Tema de Bootstrap 5: paleta de colores y tipografías de la aplicación.
tema <- bs_theme(
  version = 5,
  bg = "#FFFFFF", fg = "#1E2430",
  primary = "#B3282D", secondary = "#5B6270",
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                              "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("Palatino Linotype", "Palatino", "Book Antiqua",
                                 "TeX Gyre Pagella", "Georgia", "serif"),
  "font-size-base" = "1rem",
  "border-radius" = "0.2rem"
)

# Tema global: las pestañas se convierten a texto al construir la interfaz (para
# darles identificadores fijos) y bslib necesita conocer el tema en ese momento.
bs_global_set(tema)

# Cada página es un módulo con su interfaz (pag_*_ui) y su servidor (pag_*_server).
paneles <- list(
  inicio = pag_inicio_ui, preferencias = pag_preferencias_ui, pares = pag_pares_ui,
  prisionero = pag_prisionero_ui, canal = pag_canal_ui, turnos = pag_turnos_ui,
  minimax = pag_minimax_ui, poda = pag_poda_ui, tresenraya = pag_tres_en_raya_ui,
  aprender = pag_aprender_ui, tujuego = pag_tu_juego_ui
)

# Menú superior y, debajo, un navset sin pestañas visibles con una página por juego.
ui <- page(
  theme = tema,
  title = "Teoría de Juegos",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "icon", href = "data:,"),
    tags$link(rel = "stylesheet", href = "estilo.css"),
    script_navegacion()
  ),
  div(class = "app",
    menu_superior(),
    tags$main(class = "contenido",
      ids_unicos(do.call(navset_hidden, c(list(id = "paginas"),
        lapply(names(paneles), function(id) nav_panel_hidden(value = id, paneles[[id]](id))))), "paginas")))
)

server <- function(input, output, session) {
  # El menú superior envía en input$ir_a la página que hay que mostrar.
  observeEvent(input$ir_a, nav_select("paginas", input$ir_a))

  # Servidores de cada página (la de inicio no tiene parte de servidor).
  pag_preferencias_server("preferencias")
  pag_pares_server("pares")
  pag_prisionero_server("prisionero")
  pag_canal_server("canal")
  pag_turnos_server("turnos")
  pag_minimax_server("minimax")
  pag_poda_server("poda")
  pag_tres_en_raya_server("tresenraya")
  pag_aprender_server("aprender")
  pag_tu_juego_server("tujuego")
}

shinyApp(ui, server)
