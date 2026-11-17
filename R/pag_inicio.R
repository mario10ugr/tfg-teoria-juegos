# =============================================================================
# Inicio
# =============================================================================

pag_inicio_ui <- function(id) {
  # Una tarjeta por página, en el mismo orden que el menú.
  tarjetas <- Filter(function(p) p$id != "inicio", PAGINAS)
  div(class = "inicio",
    div(class = "portada",
      div(h1("Teoría de Juegos"),
          p(class = "portada-sub",
            "Las matemáticas de las decisiones cuando no decides solo: lo que te conviene ",
            "depende de lo que hagan los demás. Elige un juego para empezar.")),
      div(class = "claves",
          span(class = "k-j1", "Jugador 1 o MAX"), span(class = "k-j2", "Jugador 2 o MIN"),
          span(class = "k-eq", "Equilibrio o decisión elegida"), span(class = "k-gris", "Descartado"))
    ),
    div(class = "rejilla-juegos",
      lapply(seq_along(tarjetas), function(k) {
        p <- tarjetas[[k]]
        tags$button(class = "tarjeta-juego", type = "button", onclick = sprintf("irA('%s')", p$id),
                    span(class = "tj-num", sprintf("%02d", k)),
                    span(class = "tj-icono", ico(p$icono)),
                    span(class = "tj-titulo", p$titulo),
                    span(class = "tj-gancho", p$gancho),
                    span(class = "tj-entrar", "Entrar"))
      })
    )
  )
}
