# =============================================================================
# Sección 1.2 · Preferencias y utilidad
# =============================================================================

# Lee una relación de preferencia escrita como cadena: "a > b ~ c > d".
# ">" separa clases de indiferencia (de más a menos preferida) y "~" une
# alternativas indiferentes. Devuelve una lista de clases ordenadas.
parse_preferencias <- function(texto) {
  t <- gsub("\u227b", ">", texto)          # ≻
  t <- gsub("[\u223c\u2248]", "~", t)      # ∼ ≈
  t <- gsub("\\s+", "", t)
  if (!nzchar(t)) stop("Escribe al menos una alternativa.")
  clases <- strsplit(t, ">", fixed = TRUE)[[1]]
  if (grepl(">$", t) || any(!nzchar(clases))) {
    stop("Hay un «>» sin alternativa a uno de sus lados.")
  }
  clases <- lapply(clases, function(cl) strsplit(cl, "~", fixed = TRUE)[[1]])
  todas <- unlist(clases)
  if (any(!nzchar(todas)) || any(grepl("~$", t))) {
    stop("Hay un «~» sin alternativa a uno de sus lados.")
  }
  if (anyDuplicated(todas)) {
    stop(sprintf("La alternativa «%s» aparece más de una vez.",
                 todas[anyDuplicated(todas)]))
  }
  clases
}

# Función de utilidad de la demostración de la Proposición 1.1:
# u(a) = |{b in A : a ⪰ b}|.
utilidad_ordinal <- function(clases) {
  k <- length(clases)
  filas <- lapply(seq_len(k), function(i) {
    peores <- unlist(clases[i:k])
    data.frame(
      alternativa = clases[[i]],
      conjunto = paste0("{", paste(peores, collapse = ", "), "}"),
      u = length(peores),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# Utilidad esperada de Von Neumann-Morgenstern: U(L) = sum p_i u(a_i).
utilidad_esperada <- function(p, u) sum(p * u)

# Comprueba que un vector es una lotería válida.
validar_loteria <- function(p, nombre = "L") {
  if (any(is.na(p))) stop(sprintf("Faltan probabilidades en %s.", nombre))
  if (any(p < 0)) stop(sprintf("%s tiene probabilidades negativas.", nombre))
  if (abs(sum(p) - 1) > 1e-6) {
    stop(sprintf("Las probabilidades de %s suman %s; deben sumar 1.",
                 nombre, format(round(sum(p), 4))))
  }
  invisible(TRUE)
}
