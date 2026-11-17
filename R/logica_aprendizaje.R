# =============================================================================
# Sección 2.6 · Aprendizaje en juegos repetidos
# =============================================================================

# Simula T rondas de un juego bipersonal G con una de estas dinámicas:
#   "fictitious"  fictitious play (Brown, 1951): mejor respuesta pura a la
#                 frecuencia empírica de las acciones pasadas del rival.
#   "regret"      regret matching: cada acción con probabilidad proporcional a
#                 la parte positiva de su arrepentimiento acumulado.
# creencia1: pesos iniciales del Jugador 1 sobre las acciones del Jugador 2
# creencia2: pesos iniciales del Jugador 2 sobre las acciones del Jugador 1
# desempate: "primera" (siempre la primera acción empatada) o "aleatorio"
simular_aprendizaje <- function(G, T = 1000, metodo = "fictitious",
                                creencia1 = NULL, creencia2 = NULL,
                                desempate = "primera", semilla = 1) {
  set.seed(semilla)
  A <- G$A; B <- G$B; m <- nrow(A); n <- ncol(A)
  if (is.null(creencia1) || sum(creencia1) <= 0) creencia1 <- rep(1, n)
  if (is.null(creencia2) || sum(creencia2) <= 0) creencia2 <- rep(1, m)
  cuenta1 <- creencia1; cuenta2 <- creencia2

  # Mejor respuesta, deshaciendo los empates según 'desempate'.
  elegir <- function(valores) {
    mejores <- which(valores >= max(valores) - 1e-12)
    if (length(mejores) > 1 && desempate == "aleatorio") {
      mejores[sample.int(length(mejores), 1)]
    } else mejores[1]
  }

  a1 <- integer(T); a2 <- integer(T)
  S1 <- numeric(m); S2 <- numeric(n)        # pago acumulado de cada acción fija
  real1 <- 0; real2 <- 0                    # pago acumulado obtenido
  reg1 <- numeric(T); reg2 <- numeric(T)    # arrepentimiento medio R_i^t

  for (t in seq_len(T)) {
    if (metodo == "fictitious") {
      a1[t] <- elegir(as.vector(A %*% (cuenta1 / sum(cuenta1))))
      a2[t] <- elegir(as.vector((cuenta2 / sum(cuenta2)) %*% B))
    } else {
      pos1 <- pmax(S1 - real1, 0); pos2 <- pmax(S2 - real2, 0)
      p1 <- if (sum(pos1) > 0) pos1 / sum(pos1) else rep(1 / m, m)
      p2 <- if (sum(pos2) > 0) pos2 / sum(pos2) else rep(1 / n, n)
      a1[t] <- sample.int(m, 1, prob = p1)
      a2[t] <- sample.int(n, 1, prob = p2)
    }
    cuenta1[a2[t]] <- cuenta1[a2[t]] + 1
    cuenta2[a1[t]] <- cuenta2[a1[t]] + 1
    S1 <- S1 + A[, a2[t]]; S2 <- S2 + B[a1[t], ]
    real1 <- real1 + A[a1[t], a2[t]]; real2 <- real2 + B[a1[t], a2[t]]
    reg1[t] <- (max(S1) - real1) / t
    reg2[t] <- (max(S2) - real2) / t
  }

  # Frecuencias empíricas acumuladas de cada acción, ronda a ronda.
  tt <- seq_len(T)
  F1 <- sapply(seq_len(m), function(s) cumsum(a1 == s) / tt)
  F2 <- sapply(seq_len(n), function(s) cumsum(a2 == s) / tt)
  F1 <- matrix(F1, nrow = T); F2 <- matrix(F2, nrow = T)
  pagos1 <- A[cbind(a1, a2)]; pagos2 <- B[cbind(a1, a2)]
  list(
    a1 = a1, a2 = a2, F1 = F1, F2 = F2, reg1 = reg1, reg2 = reg2,
    pagos1 = pagos1, pagos2 = pagos2,
    pago_medio1 = cumsum(pagos1) / tt, pago_medio2 = cumsum(pagos2) / tt,
    # Pago esperado de las frecuencias empíricas U(σ̄1, σ̄2) (Teorema 2.2)
    U_frec = rowSums((F1 %*% A) * F2)
  )
}

# Ganancia máxima por desviación unilateral frente a (σ̄1, σ̄2) (Prop. 2.3).
ganancias_desviacion <- function(G, x, y) {
  u1 <- as.numeric(t(x) %*% G$A %*% y); u2 <- as.numeric(t(x) %*% G$B %*% y)
  c(j1 = max(G$A %*% y) - u1, j2 = max(t(x) %*% G$B) - u2)
}

# TRUE si A + B = 0 en todas las casillas.
es_suma_cero <- function(G) all(abs(G$A + G$B) < 1e-9)
