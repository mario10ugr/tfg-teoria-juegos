# =============================================================================
# Estilo común para los gráficos de líneas (gráficos base de R)
# =============================================================================

# Márgenes y colores comunes. Devuelve los parámetros anteriores de par() para
# poder restaurarlos con on.exit().
estilo_grafico <- function(mar = c(4.4, 4.8, 1.4, 1)) {
  par(mar = mar, las = 1, bty = "n", cex.axis = 1.05, cex.lab = 1.12,
      col.axis = "#5B6270", col.lab = COL_TINTA, fg = "#C9CED6", col = COL_TINTA, mgp = c(3, 0.7, 0))
}

# Lienzo vacío con rejilla horizontal, ejes sin líneas y coma decimal en las
# etiquetas.
lienzo <- function(xlim, ylim, xlab, ylab, log = "", xat = NULL, yat = NULL, ylabs = NULL) {
  plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, log = log, axes = FALSE)
  ay <- if (is.null(yat)) axTicks(2) else yat
  ax <- if (is.null(xat)) axTicks(1) else xat
  abline(h = ay, col = "#ECEEF1", lwd = 1)
  axis(1, at = ax, labels = gsub("\\.", ",", format(ax, trim = TRUE)), lwd = 0, lwd.ticks = 0)
  axis(2, at = ay, labels = if (is.null(ylabs)) menos(gsub("\\.", ",", format(ay, trim = TRUE))) else ylabs,
       lwd = 0, lwd.ticks = 0)
}
