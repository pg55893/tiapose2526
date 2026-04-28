# =============================================================
# RF_SANN_v2.R — Eduardo
# Melhorias: 20 runs, mediana profit, calibração T, FES no eixo X,
#             O2 Death Penalty vs Repair
# =============================================================

source("/Users/edias/TIAPOSE2526/utils/config_otimizacao.R")

# --- LOG: guarda todo o output da consola ---
LOG_FILE <- "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/log_SANN_v2.txt"
sink(LOG_FILE, append = FALSE, split = TRUE)   # split=TRUE: escreve no ficheiro E na consola
cat("Log iniciado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

idx_JX  <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))
idx_PR  <- seq(1, 84, by = 3)
NRUNS   <- 20
MAXIT   <- 10000

# =============================================================
# HELPER — repair(S): reduz PR até total_units(S) <= 10000
# Usa <<- BEST para não perder a melhor solução reparada
# =============================================================
repair <- function(S) {
  S <- pmin(pmax(S, lower), upper)
  BEST_repair <- NULL
  best_profit <- -Inf

  # tenta até 200 reduções incrementais de PR
  for (iter in 1:200) {
    u <- total_units(S)
    if (!is.na(u) && u <= 10000) {
      p <- profit(S)
      if (p > best_profit) {
        best_profit  <- p
        BEST_repair  <- S
      }
      break   # viável — sai
    }
    # reduz PR em todos os dias em 5%
    S[idx_PR] <- S[idx_PR] * 0.95
    S[idx_PR] <- pmax(S[idx_PR], 0)
  }
  return(BEST_repair)   # NULL se nunca ficou viável
}

# =============================================================
# WRAPPER FES — conta avaliações da função objetivo
# =============================================================
make_eval_fes <- function(obj_fn) {
  FES    <- 0L
  MELHOR <- -Inf
  hist   <- numeric(0)

  list(
    fn = function(S) {
      v <- obj_fn(S)
      FES    <<- FES + 1L
      p      <- if (is.finite(v)) -v else -Inf
      if (p > MELHOR) MELHOR <<- p
      hist   <<- c(hist, MELHOR)
      return(v)
    },
    get_hist  = function() hist,
    get_fes   = function() FES
  )
}

# =============================================================
# SECÇÃO 1 — CALIBRAÇÃO TEMPERATURA (O1, 20 runs cada T)
# =============================================================
cat("\n=== CALIBRACAO TEMPERATURA (O1) ===\n")
cat("Temperaturas: 500, 1000, 2000\n\n")

temps_calib <- c(500, 1000, 2000)
calib_mediana <- numeric(length(temps_calib))
calib_hist    <- list()

set.seed(42)
for (ti in seq_along(temps_calib)) {
  T_val  <- temps_calib[ti]
  lucros <- numeric(NRUNS)
  hists  <- list()

  for (i in 1:NRUNS) {
    tracker <- make_eval_fes(function(S) {
      S <- pmin(pmax(S, lower), upper)
      -profit(S)
    })
    s0 <- runif(84) * (upper - lower) + lower
    sa <- optim(par = s0, fn = tracker$fn, method = "SANN",
                control = list(maxit = MAXIT, temp = T_val, trace = FALSE))
    S_tmp <- pmin(pmax(sa$par, lower), upper)
    S_tmp[idx_JX] <- round(S_tmp[idx_JX])
    lucros[i] <- profit(S_tmp)
    hists[[i]] <- tracker$get_hist()
  }

  med <- median(lucros)
  calib_mediana[ti] <- med
  calib_hist[[ti]]  <- hists
  cat(sprintf("T=%4d | mediana=%8.2f | min=%8.2f | max=%8.2f\n",
              T_val, med, min(lucros), max(lucros)))
}

# Escolhe melhor T
best_T_idx <- which.max(calib_mediana)
BEST_T     <- temps_calib[best_T_idx]
cat(sprintf("\n>> Melhor T selecionado: %d (mediana=%.2f)\n", BEST_T, calib_mediana[best_T_idx]))

# Plota curva de mediana por temperatura (FES)
pdf(file.path(dirname(sys.frame(1)$ofile %||% ""), "calib_temperatura_O1.pdf") %||%
    "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/calib_temperatura_O1.pdf",
    width = 9, height = 6)
cores_T <- c("steelblue", "firebrick", "forestgreen")
primeiro <- TRUE
for (ti in seq_along(temps_calib)) {
  # mediana por posição FES entre os 20 runs
  max_len <- max(sapply(calib_hist[[ti]], length))
  mat     <- sapply(calib_hist[[ti]], function(h) {
    c(h, rep(h[length(h)], max_len - length(h)))
  })
  med_curve <- apply(mat, 1, median)
  x_fes     <- seq_along(med_curve)
  if (primeiro) {
    plot(x_fes, med_curve, type = "l", col = cores_T[ti], lwd = 2,
         xlab = "Numero de Avaliacoes (FES)", ylab = "Mediana Melhor Lucro",
         main = "Calibracao Temperatura SANN - O1 (mediana 20 runs)")
    primeiro <- FALSE
  } else {
    lines(x_fes, med_curve, col = cores_T[ti], lwd = 2)
  }
}
legend("bottomright", legend = paste0("T=", temps_calib),
       col = cores_T, lwd = 2, bty = "n")
dev.off()
cat("Grafico calibracao guardado.\n")

# =============================================================
# SECÇÃO 2 — O1: SANN 20 runs, mediana profit, FES no eixo X
# =============================================================
cat("\n=== O1: SANN (20 runs, T=", BEST_T, ") ===\n")

lucros_O1   <- numeric(NRUNS)
BEST_S_O1   <- NULL
best_O1     <- -Inf
hists_O1    <- list()

set.seed(123)
for (i in 1:NRUNS) {
  tracker <- make_eval_fes(function(S) {
    S <- pmin(pmax(S, lower), upper)
    -profit(S)
  })
  s0 <- runif(84) * (upper - lower) + lower
  sa <- optim(par = s0, fn = tracker$fn, method = "SANN",
              control = list(maxit = MAXIT, temp = BEST_T, trace = FALSE))
  S_tmp <- pmin(pmax(sa$par, lower), upper)
  S_tmp[idx_JX] <- round(S_tmp[idx_JX])
  L <- profit(S_tmp)
  lucros_O1[i] <- L
  hists_O1[[i]] <- tracker$get_hist()
  cat(sprintf("Run %2d | Lucro: %8.2f\n", i, L))
  if (L > best_O1) { best_O1 <- L; BEST_S_O1 <- S_tmp }
}

med_O1 <- median(lucros_O1)
cat(sprintf("\n>> Mediana lucro O1 : %.2f\n", med_O1))
cat(sprintf(">> Melhor lucro O1  : %.2f\n", best_O1))
cat(sprintf(">> Unidades         : %d\n",   total_units(BEST_S_O1)))

# Gráfico O1: 20 runs individuais (cinzento) + mediana (azul)
plot_runs <- function(hists, med_curve, titulo, pdf_out, cor_med = "steelblue") {
  max_len <- max(sapply(hists, length))
  mat     <- sapply(hists, function(h) c(h, rep(h[length(h)], max_len - length(h))))
  x_fes   <- seq_len(max_len)
  y_rng   <- range(mat[is.finite(mat)], na.rm = TRUE)
  pdf(pdf_out, width = 9, height = 6)
  # runs individuais
  for (j in seq_len(ncol(mat))) {
    if (j == 1)
      plot(x_fes, mat[, j], type = "l", col = "grey80", lwd = 1,
           xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro",
           main = titulo, ylim = y_rng)
    else
      lines(x_fes, mat[, j], col = "grey80", lwd = 1)
  }
  # mediana a bold
  lines(x_fes, med_curve, col = cor_med, lwd = 2.5)
  legend("bottomright",
         legend = c("Runs individuais", "Mediana"),
         col    = c("grey70", cor_med), lwd = c(1, 2.5), bty = "n")
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
}

plot_runs(
  hists_O1, med_curve_O1,
  titulo  = paste0("Convergencia SANN O1 - T=", BEST_T, " - ", NRUNS, " runs"),
  pdf_out = "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/convergencia_O1_SANN_v2.pdf"
)

# Guarda historico para comparativos
saveRDS(med_curve_O1, "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/convergencia_O1_SANN.rds")

# =============================================================
# SECÇÃO 3 — O2 Death Penalty, 20 runs, FES no eixo X
# =============================================================
cat("\n=== O2: SANN + Death Penalty (20 runs) ===\n")

lucros_O2_dp  <- numeric(NRUNS)
BEST_S_O2_dp  <- NULL
best_O2_dp    <- -Inf
hists_O2_dp   <- list()

set.seed(456)
for (i in 1:NRUNS) {
  tracker <- make_eval_fes(function(S) {
    S <- pmin(pmax(S, lower), upper)
    if (any(S[idx_PR] >= 1, na.rm = TRUE)) return(Inf)
    u <- total_units(S)
    if (is.na(u) || u > 10000) return(Inf)
    -profit(S)
  })
  s0           <- lower
  s0[idx_PR]   <- 0.10
  s0[seq(2, 84, by = 3)] <- 1
  s0[seq(3, 84, by = 3)] <- 1
  s0 <- pmin(pmax(s0 + runif(84) * 0.5, lower), upper)

  sa <- optim(par = s0, fn = tracker$fn, method = "SANN",
              control = list(maxit = MAXIT * 2, temp = BEST_T, trace = FALSE))
  S_tmp <- pmin(pmax(sa$par, lower), upper)
  S_tmp[idx_JX] <- round(S_tmp[idx_JX])
  u <- total_units(S_tmp)
  if (!is.na(u) && u <= 10000) {
    L <- profit(S_tmp)
    lucros_O2_dp[i] <- L
    hists_O2_dp[[i]] <- tracker$get_hist()
    cat(sprintf("Run %2d | Lucro: %8.2f | Unidades: %d\n", i, L, u))
    if (L > best_O2_dp) { best_O2_dp <- L; BEST_S_O2_dp <- S_tmp }
  } else {
    lucros_O2_dp[i] <- NA
    hists_O2_dp[[i]] <- tracker$get_hist()
    cat(sprintf("Run %2d | INVALIDA | Unidades: %s\n", i, ifelse(is.na(u), "NA", u)))
  }
}

med_O2_dp <- median(lucros_O2_dp, na.rm = TRUE)
cat(sprintf("\n>> Mediana lucro O2 (DP) : %.2f\n", med_O2_dp))
cat(sprintf(">> Melhor lucro O2 (DP)  : %.2f\n", best_O2_dp))
if (!is.null(BEST_S_O2_dp))
  cat(sprintf(">> Unidades              : %d\n", total_units(BEST_S_O2_dp)))

# =============================================================
# SECÇÃO 4 — O2 Repair, 20 runs, FES no eixo X
# =============================================================
cat("\n=== O2: SANN + Repair (20 runs) ===\n")

# Repair RÁPIDO: máx 30 reduções (×0.90) em vez de 200×0.95
# 30 passos = PR reduzido ~5% do original — converge rápido
repair_fast <- function(S) {
  S <- pmin(pmax(S, lower), upper)
  for (k in 1:30) {
    u <- total_units(S)
    if (!is.na(u) && u <= 10000) return(S)
    S[idx_PR] <- S[idx_PR] * 0.90
    S[idx_PR] <- pmax(S[idx_PR], 0)
  }
  u <- total_units(S)
  if (!is.na(u) && u <= 10000) return(S)
  return(NULL)
}

lucros_O2_rep  <- numeric(NRUNS)
BEST_S_O2_rep  <- NULL
best_O2_rep    <- -Inf
hists_O2_rep   <- list()

set.seed(789)
for (i in 1:NRUNS) {
  # Variáveis locais ao ambiente do loop — sem closure aninhado
  best_rep_run   <- -Inf
  best_S_rep_run <- NULL
  hist_rep_run   <- numeric(0)

  eval_O2_rep_i <- local({
    bv <- -Inf; bs <- NULL; ht <- numeric(0)
    list(
      fn = function(S) {
        S <- pmin(pmax(S, lower), upper)
        S_r <- repair_fast(S)
        if (is.null(S_r)) { ht <<- c(ht, bv); return(Inf) }
        p <- profit(S_r)
        if (p > bv) { bv <<- p; bs <<- S_r }
        ht <<- c(ht, bv)
        return(-p)
      },
      get_best  = function() bs,
      get_lucro = function() bv,
      get_hist  = function() ht
    )
  })

  s0         <- lower
  s0[idx_PR] <- 0.05
  s0[seq(2, 84, by = 3)] <- 1
  s0[seq(3, 84, by = 3)] <- 1
  s0 <- pmin(pmax(s0 + runif(84) * 0.2, lower), upper)

  optim(par = s0, fn = eval_O2_rep_i$fn, method = "SANN",
        control = list(maxit = MAXIT * 2, temp = BEST_T, trace = FALSE))

  L_run  <- eval_O2_rep_i$get_lucro()
  S_run  <- eval_O2_rep_i$get_best()
  h_run  <- eval_O2_rep_i$get_hist()

  if (!is.null(S_run) && L_run > -Inf) {
    lucros_O2_rep[i] <- L_run
    hists_O2_rep[[i]] <- h_run
    cat(sprintf("Run %2d | Lucro: %8.2f | Unidades: %d\n",
                i, L_run, total_units(S_run)))
    if (L_run > best_O2_rep) {
      best_O2_rep   <- L_run
      BEST_S_O2_rep <- S_run
    }
  } else {
    lucros_O2_rep[i] <- NA
    hists_O2_rep[[i]] <- h_run
    cat(sprintf("Run %2d | Repair falhou\n", i))
  }
}

med_O2_rep <- median(lucros_O2_rep, na.rm = TRUE)
cat(sprintf("\n>> Mediana lucro O2 (Repair) : %.2f\n", med_O2_rep))
cat(sprintf(">> Melhor lucro O2 (Repair)  : %.2f\n", best_O2_rep))
if (!is.null(BEST_S_O2_rep))
  cat(sprintf(">> Unidades                  : %d\n", total_units(BEST_S_O2_rep)))

# =============================================================

# SECÇÃO 5 — Curva comparativa O2: DP vs Repair (FES no eixo X)
# =============================================================
cat("\n=== Convergencia O2: Death Penalty vs Repair ===\n")

build_med_curve <- function(hists) {
  valid <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  max_len <- max(sapply(valid, length))
  mat <- sapply(valid, function(h) c(h, rep(h[length(h)], max_len - length(h))))
  apply(mat, 1, median, na.rm = TRUE)
}

curve_dp  <- build_med_curve(hists_O2_dp)
curve_rep <- build_med_curve(hists_O2_rep)

# Gráfico O2: runs individuais + mediana para DP e Repair
pdf("/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/convergencia_O2_dp_vs_repair.pdf", width = 10, height = 6)

# --- constrói matrizes alinhadas ---
build_mat <- function(hists) {
  valid <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  ml <- max(sapply(valid, length))
  sapply(valid, function(h) c(h, rep(h[length(h)], ml - length(h))))
}
mat_dp  <- build_mat(hists_O2_dp)
mat_rep <- build_mat(hists_O2_rep)

y_all <- c(if (!is.null(mat_dp))  mat_dp[is.finite(mat_dp)],
           if (!is.null(mat_rep)) mat_rep[is.finite(mat_rep)])
y_rng <- range(y_all, na.rm = TRUE)

# Runs DP (vermelho claro)
if (!is.null(mat_dp)) {
  for (j in seq_len(ncol(mat_dp))) {
    if (j == 1)
      plot(seq_len(nrow(mat_dp)), mat_dp[, j], type = "l",
           col = "#FFAAAA", lwd = 1,
           xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro",
           main = "O2 SANN: Death Penalty vs Repair (runs + mediana)",
           ylim = y_rng)
    else
      lines(seq_len(nrow(mat_dp)), mat_dp[, j], col = "#FFAAAA", lwd = 1)
  }
  lines(seq_along(curve_dp), curve_dp, col = "firebrick", lwd = 2.5)
} else {
  plot.new()
}

# Runs Repair (verde claro)
if (!is.null(mat_rep)) {
  for (j in seq_len(ncol(mat_rep)))
    lines(seq_len(nrow(mat_rep)), mat_rep[, j], col = "#AADDAA", lwd = 1)
  lines(seq_along(curve_rep), curve_rep, col = "forestgreen", lwd = 2.5)
}

legend("bottomright",
       legend = c(sprintf("DP runs (med=%.0f)",     med_O2_dp),
                  sprintf("Repair runs (med=%.0f)", med_O2_rep),
                  "Mediana DP", "Mediana Repair"),
       col  = c("#FFAAAA", "#AADDAA", "firebrick", "forestgreen"),
       lwd  = c(1, 1, 2.5, 2.5), bty = "n")
dev.off()
cat("Grafico DP vs Repair guardado.\n")

# Guarda histórico O2 para comparativos.R
best_curve_O2 <- if (!is.null(curve_rep) && med_O2_rep >= med_O2_dp) curve_rep else curve_dp
saveRDS(best_curve_O2, "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/convergencia_O2_SANN.rds")

# =============================================================
# SECÇÃO 6 — EXPORTAR RESULTADOS
# =============================================================
BEST_S_O2 <- if (!is.null(BEST_S_O2_rep) && best_O2_rep >= best_O2_dp) BEST_S_O2_rep else BEST_S_O2_dp
best_O2   <- max(best_O2_rep, best_O2_dp, na.rm = TRUE)

resultado <- data.frame(
  membro    = "Eduardo",
  algoritmo = "SANN",
  objetivo  = c("O1", "O2"),
  lucro     = c(round(best_O1,  2), round(best_O2,  2)),
  mediana   = c(round(med_O1,   2), round(max(med_O2_dp, med_O2_rep, na.rm = TRUE), 2)),
  unidades  = c(total_units(BEST_S_O1),
                ifelse(is.null(BEST_S_O2), NA, total_units(BEST_S_O2))),
  total_HR  = c(sum(BEST_S_O1[idx_JX]),
                ifelse(is.null(BEST_S_O2), NA, sum(BEST_S_O2[idx_JX]))),
  stringsAsFactors = FALSE
)
rownames(resultado) <- NULL
write.csv(resultado, "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/resultado_SANN.csv", row.names = FALSE)
cat("\n=== Resultado Final ===\n")
print(resultado)

cat("\n=== FIM RF_SANN_v2.R ===")
cat("\nLog guardado em:", LOG_FILE, "\n")
sink()   # fecha o log
