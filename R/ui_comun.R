# =============================================================================
# Componentes de interfaz compartidos
# =============================================================================

# Valor por defecto cuando a es NULL.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Negrita sin espacios alrededor: el texto vecino debe llevar sus propios espacios.
negrita <- function(...) tags$strong(..., .noWS = "outside")

# Icono de Font Awesome dibujado como SVG (hereda el color del texto).
ico <- function(nombre, clase = NULL, alto = "1em") {
  span(class = paste("ico", clase), `aria-hidden` = "true",
       HTML(as.character(fontawesome::fa(nombre, fill = "currentColor", height = alto))))
}

# Ficha del tres en raya dibujada a trazo: X azul o círculo granate.
ficha_xo <- function(v) {
  if (v == 1) {
    HTML(sprintf('<svg class="ficha-xo" viewBox="0 0 100 100" aria-hidden="true"><path d="M26 26 L74 74 M74 26 L26 74" stroke="%s" stroke-width="12" stroke-linecap="round" fill="none"/></svg>', COL_J1))
  } else if (v == -1) {
    HTML(sprintf('<svg class="ficha-xo" viewBox="0 0 100 100" aria-hidden="true"><circle cx="50" cy="50" r="26" stroke="%s" stroke-width="11" fill="none"/></svg>', COL_J2))
  } else ""
}

# Páginas de la aplicación, en el orden del menú superior.
PAGINAS <- list(
  list(id = "inicio",       icono = "house",           corto = "Inicio"),
  list(id = "preferencias", icono = "scale-balanced",  corto = "Preferencias",
       titulo = "Preferencias y utilidad",
       gancho = "Cómo se pone nota a lo que uno quiere, incluso cuando hay riesgo."),
  list(id = "pares",        icono = "hand-peace",      corto = "Pares o nones",
       titulo = "Pares o nones",
       gancho = "¿Existe una forma de ganar siempre? Juega contra el ordenador y descúbrelo."),
  list(id = "prisionero",   icono = "handcuffs",       corto = "Prisionero",
       titulo = "El dilema del prisionero",
       gancho = "Por qué dos personas listas acaban peor que si hubieran confiado la una en la otra."),
  list(id = "canal",        icono = "tower-broadcast", corto = "Canal de radio",
       titulo = "Acceso al canal",
       gancho = "Dos móviles, dos canales y ninguna forma de hablar. ¿Cómo se reparten?"),
  list(id = "turnos",       icono = "sitemap",         corto = "Por turnos",
       titulo = "Juego por turnos",
       gancho = "Cuando uno mueve primero y el otro lo ve, se razona desde el final."),
  list(id = "minimax",      icono = "chess-knight",    corto = "Minimax",
       titulo = "Minimax",
       gancho = "Así piensa un ordenador que juega al ajedrez: suponiendo lo peor."),
  list(id = "poda",         icono = "scissors",        corto = "Poda alfa-beta",
       titulo = "Poda alfa-beta",
       gancho = "El truco para llegar a la misma decisión mirando muchas menos jugadas."),
  list(id = "tresenraya",   icono = "hashtag",         corto = "Tres en raya",
       titulo = "Tres en raya contra la IA",
       gancho = "Juega contra la máquina y mira qué está pensando en cada jugada."),
  list(id = "aprender",     icono = "arrows-rotate",   corto = "Aprender jugando",
       titulo = "Aprender jugando",
       gancho = "Qué pasa si dos jugadores repiten el juego mil veces y aprenden del otro."),
  list(id = "tujuego",      icono = "pen-ruler",       corto = "Crea tu juego",
       titulo = "Crea tu propio juego",
       gancho = "Inventa los premios y la aplicación encuentra las soluciones.")
)

# Barra superior con el título y un botón por página.
menu_superior <- function() {
  tags$header(class = "cabecera-app",
    tags$button(class = "marca", type = "button", onclick = "irA('inicio')",
                span(class = "marca-titulo", "Teoría de Juegos"),
                span(class = "marca-sub", "Laboratorio interactivo")),
    tags$nav(class = "menu", `aria-label` = "Juegos",
      lapply(PAGINAS, function(p) {
        tags$button(class = "menu-item", type = "button", `data-destino` = p$id,
                    onclick = sprintf("irA('%s')", p$id),
                    ico(p$icono, "menu-ico"), span(class = "menu-texto", p$corto))
      }))
  )
}

# irA() comunica al servidor la página elegida (input$ir_a) y marca el botón
# activo del menú.
script_navegacion <- function() {
  tags$script(HTML("
    function marcarMenu(d) {
      document.querySelectorAll('.menu-item').forEach(function(b) {
        var activo = b.dataset.destino === d;
        b.classList.toggle('activo', activo);
        if (activo) { b.setAttribute('aria-current', 'page'); b.scrollIntoView({block: 'nearest', inline: 'nearest'}); }
        else b.removeAttribute('aria-current');
      });
    }
    function irA(d) {
      if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('ir_a', d, {priority: 'event'});
      marcarMenu(d);
    }
    $(document).on('shiny:connected', function() { marcarMenu('inicio'); });
  "))
}

# -----------------------------------------------------------------------------
# Estructura de página a pantalla completa
# -----------------------------------------------------------------------------
# Cada página tiene una cabecera compacta y pestañas internas; cada pestaña
# ocupa el resto de la pantalla, sin desplazamiento vertical.

# bslib numera cada conjunto de pestañas con un número aleatorio. Si dos coinciden,
# el navegador muestra el panel de otro conjunto. Para evitarlo, se sustituye ese
# número por un nombre fijo y único (conservando las dependencias de JavaScript).
ids_unicos <- function(etiqueta, prefijo) {
  deps <- htmltools::findDependencies(etiqueta)
  html <- as.character(etiqueta)
  num <- unique(sub("tab-([0-9]+)-.*", "\\1", regmatches(html, gregexpr("tab-[0-9]+-[0-9]+", html))[[1]]))
  for (k in num) {
    html <- gsub(sprintf("tab-%s-", k), sprintf("tab-%s-", prefijo), html, fixed = TRUE)
    html <- gsub(sprintf('data-tabsetid="%s"', k), sprintf('data-tabsetid="%s"', prefijo), html, fixed = TRUE)
  }
  # Fuera de una sesión de Shiny, bslib escribe las pestañas con el marcado de
  # Bootstrap 3 (li.active > a). Se pasa al de Bootstrap 5 (li.nav-item > a.nav-link).
  html <- gsub('<li class="active">(\\s*)<a ', '<li class="nav-item">\\1<a class="nav-link active" ', html, perl = TRUE)
  html <- gsub('<li>(\\s*)<a href="#tab-', '<li class="nav-item">\\1<a class="nav-link" href="#tab-', html, perl = TRUE)
  htmltools::attachDependencies(HTML(html), deps)
}

# Página de un juego: cabecera con icono, título y entradilla, y sus pestañas.
pagina <- function(id, entradilla, ...) {
  pg <- Filter(function(x) x$id == id, PAGINAS)[[1]]
  div(class = "pagina",
    div(class = "cabecera-pagina",
        div(class = "cab-icono", ico(pg$icono)),
        div(class = "cab-textos", h1(pg$titulo), p(class = "entradilla", entradilla))),
    div(class = "pestanas", ids_unicos(navset_underline(id = NS(id)("pestana"), ...), id))
  )
}

# Panel de una pestaña: dibujo a la izquierda (se ajusta al alto disponible)
# y columna lateral con la explicación y los controles.
panel <- function(visual, lateral, ancho = 400) {
  div(class = "panel", style = sprintf("grid-template-columns: minmax(0, 1fr) %dpx;", ancho),
      div(class = "panel-visual", visual),
      div(class = "panel-lateral", lateral))
}

# Panel con varias columnas (proporciones opcionales).
panel_columnas <- function(..., proporciones = NULL) {
  hijos <- list(...)
  prop <- proporciones %||% rep(1, length(hijos))
  div(class = "panel panel-columnas",
      style = sprintf("grid-template-columns: %s;", paste(sprintf("minmax(0, %gfr)", prop), collapse = " ")),
      lapply(hijos, function(h) div(class = "columna", h)))
}

# Pestaña de matemáticas: texto en columnas para que quepa sin desplazarse.
panel_curiosos <- function(...) {
  div(class = "panel panel-curiosos", div(class = "curiosos-texto", ...))
}

# Contenedor que hace que un dibujo o gráfico ocupe todo el alto disponible.
llenar <- function(...) div(class = "llenar", ...)

# Bloques de texto de la columna lateral.
idea_clave <- function(...) {
  div(class = "idea", div(class = "idea-titulo", ico("lightbulb"), "La idea clave"),
      div(class = "idea-texto", ...))
}

explicacion <- function(titulo, ...) {
  div(class = "explicacion", if (!is.null(titulo)) div(class = "exp-titulo", titulo),
      div(class = "exp-texto", ...))
}

# Número destacado con su etiqueta debajo.
cifra <- function(valor, etiqueta, color = NULL) {
  div(class = "cifra",
      div(class = "num", style = if (!is.null(color)) sprintf("color:%s", color), valor),
      div(class = "lbl", etiqueta))
}

# Leyenda de las flechas y la estrella de svg_matriz().
leyenda_matriz <- function(con_flechas = TRUE, con_estrella = TRUE) {
  div(class = "leyenda",
      if (con_flechas) tagList(
        span(class = "ley-flecha j1", "\u2193"), "Hacia dónde quiere cambiar el Jugador 1. ",
        span(class = "ley-flecha j2", "\u2192"), "Hacia dónde quiere cambiar el Jugador 2. "),
      if (con_estrella) tagList(span(class = "ley-estrella", "\u2605"),
                                "Equilibrio: ninguno quiere cambiar."))
}

# Barra horizontal con el reparto de probabilidades de una estrategia mixta.
barra_mixta <- function(probs, nombres, colores) {
  div(class = "barra-mixta",
      lapply(seq_along(probs), function(k) {
        if (probs[k] < 1e-9) return(NULL)
        rgb <- grDevices::col2rgb(colores[k]) / 255
        claro <- sum(c(0.299, 0.587, 0.114) * rgb) > 0.6
        div(class = "barra-trozo", style = sprintf("flex:%g; background:%s; color:%s", probs[k], colores[k],
                                                   if (claro) COL_TINTA else "#FFFFFF"),
            span(sprintf("%s %s", nombres[k], pct(probs[k]))))
      }))
}

# Botón grande de elección con icono.
boton_eleccion <- function(id, icono, texto) {
  actionButton(id, span(ico(icono, "boton-ico"), span(texto)), class = "btn-eleccion")
}

# -----------------------------------------------------------------------------
# Explicaciones paso a paso
# -----------------------------------------------------------------------------

# Botones para avanzar, retroceder, reproducir y reiniciar, y el indicador de
# progreso. Se combinan con servidor_pasos() usando el mismo id.
controles_pasos <- function(ns, id = "pasos") {
  div(class = "pasos-controles",
      actionButton(ns(paste0(id, "_ant")), NULL, icon = icon("chevron-left"),
                   class = "btn-outline-secondary btn-sm", title = "Paso anterior"),
      actionButton(ns(paste0(id, "_sig")), "Siguiente paso", icon = icon("chevron-right"),
                   class = "btn-primary btn-sm"),
      actionButton(ns(paste0(id, "_auto")), "Reproducir", icon = icon("play"),
                   class = "btn-outline-primary btn-sm"),
      actionButton(ns(paste0(id, "_rei")), NULL, icon = icon("rotate-left"),
                   class = "btn-outline-secondary btn-sm", title = "Volver a empezar"),
      uiOutput(ns(paste0(id, "_indicador")), inline = TRUE))
}

# Devuelve un reactiveVal con el paso actual (1..total). 'total' es un
# reactive o un número. Al cambiar el total se vuelve al primer paso.
servidor_pasos <- function(input, output, session, total, id = "pasos", intervalo = 1300) {
  tot <- if (is.function(total)) total else function() total
  paso <- reactiveVal(1)
  auto <- reactiveVal(FALSE)
  observeEvent(input[[paste0(id, "_sig")]], { auto(FALSE); paso(min(paso() + 1, tot())) })
  observeEvent(input[[paste0(id, "_ant")]], { auto(FALSE); paso(max(paso() - 1, 1)) })
  observeEvent(input[[paste0(id, "_rei")]], { auto(FALSE); paso(1) })
  observeEvent(input[[paste0(id, "_auto")]], {
    if (!auto() && paso() >= tot()) paso(1)
    auto(!auto())
  })
  observeEvent(tot(), { auto(FALSE); paso(1) }, ignoreInit = TRUE)
  # Reproducción automática: avanza un paso cada 'intervalo' milisegundos.
  observe({
    if (!auto()) return()
    invalidateLater(intervalo)
    isolate({ if (paso() < tot()) paso(paso() + 1) else auto(FALSE) })
  })
  observeEvent(auto(), {
    updateActionButton(session, paste0(id, "_auto"),
                       label = if (auto()) "Pausa" else "Reproducir",
                       icon = icon(if (auto()) "pause" else "play"))
  })
  # Puntos si hay pocos pasos; contador «k / n» si hay muchos.
  output[[paste0(id, "_indicador")]] <- renderUI({
    n <- tot(); k <- paso()
    if (n <= 12) {
      span(class = "puntos", `aria-label` = sprintf("Paso %d de %d", k, n),
           lapply(seq_len(n), function(i) span(class = paste("punto", if (i == k) "actual" else if (i < k) "hecho"))))
    } else span(class = "contador-pasos", sprintf("%d / %d", k, n))
  })
  paso
}
