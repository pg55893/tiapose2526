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
# =======================================================# === CALIBRACAO TEMPERATURA (O1) — COMENTADO PARA EXECUTAR APENAS O2 ===
# cat("\n=== CALIBRACAO TEMPERATURA (O1) ===\n")
# cat("Temperaturas: 500, 1000, 2000\n\n")
#
# temps_calib <- c(500, 1000, 2000)
# calib_mediana <- numeric(length(temps_calib))
# calib_hist    <- list()
#
# set.seed(42)
# for (ti in seq_along(temps_calib)) {
#   T_val  <- temps_calib[ti]
#   lucros <- numeric(NRUNS)
#   hists  <- list()
#
#   for (i in 1:NRUNS) {
#     tracker <- make_eval_fes(function(S) {
#       S <- pmin(pmax(S, lower), upper)
#       -profit(S)
#     })
#     s0 <- runif(84) * (upper - lower) + lower
#     sa <- optim(par = s0, fn = tracker$fn, method = "SANN",
#                 control = list(maxit = MAXIT, temp = T_val, trace = FALSE))
#     S_tmp <- pmin(pmax(sa$par, lower), upper)
#     S_tmp[idx_JX] <- round(S_tmp[idx_JX])
#     lucros[i] <- profit(S_tmp)
#     hists[[i]] <- tracker$get_hist()
#   }
#
#   med <- median(lucros)
#   calib_mediana[ti] <- med
#   calib_hist[[ti]]  <- hists
#   cat(sprintf("T=%4d | mediana=%8.2f | min=%8.2f | max=%8.2f\n",
#               T_val, med, min(lucros), max(lucros)))
# }
#
# # Escolhe melhor T
# best_T_idx <- which.max(calib_mediana)
# BEST_T     <- temps_calib[best_T_idx]
# cat(sprintf("\n>> Melhor T selecionado: %d (mediana=%.2f)\n", BEST_T, calib_mediana[best_T_idx]))
BEST_T <- 500 # Valor fixo para evitar erro se usado em algum lugar

# Helper para desenhar bandas (min-max) e mediana (col_line permite mudar a cor da linha)
plot_banded_series <- function(x, mat, col, add = FALSE, col_line = "red", ...) {
  med <- apply(mat, 1, median, na.rm = TRUE)
  inf <- apply(mat, 1, min, na.rm = TRUE)
  sup <- apply(mat, 1, max, na.rm = TRUE)
  col_band <- adjustcolor(col, alpha.f = 0.25)

  if (!add) {
    plot(x, med, type = "n", ...)
    grid(col = "grey90", lty = 1)
  }
  polygon(c(x, rev(x)), c(sup, rev(inf)), col = col_band, border = NA)
  lines(x, med, col = col_line, lwd = 2.5)
  abline(h = tail(med, 1), lty = 2, col = adjustcolor(col_line, alpha.f = 0.6))
}

# =============================================================
# SECÇÃO 2 — O1: SANN 20 runs, mediana profit, FES no eixo X
# =============================================================
# === O1: SANN (20 runs) — COMENTADO PARA EXECUTAR APENAS O2 ===
# cat("\n=== O1: SANN (20 runs, T=", BEST_T, ") ===\n")
#
# lucros_O1   <- numeric(NRUNS)
# BEST_S_O1   <- NULL
# best_O1     <- -Inf
# hists_O1    <- list()
#
# set.seed(123)
# for (i in 1:NRUNS) {
#   tracker <- make_eval_fes(function(S) {
#     S <- pmin(pmax(S, lower), upper)
#     -profit(S)
#   })
#   s0 <- runif(84) * (upper - lower) + lower
#   sa <- optim(par = s0, fn = tracker$fn, method = "SANN",
#               control = list(maxit = MAXIT, temp = BEST_T, trace = FALSE))
#   S_tmp <- pmin(pmax(sa$par, lower), upper)
#   S_tmp[idx_JX] <- round(S_tmp[idx_JX])
#   L <- profit(S_tmp)
#   lucros_O1[i] <- L
#   hists_O1[[i]] <- tracker$get_hist()
#   cat(sprintf("Run %2d | Lucro: %8.2f\n", i, L))
#   if (L > best_O1) { best_O1 <- L; BEST_S_O1 <- S_tmp }
# }
#
# med_O1 <- median(lucros_O1)
# cat(sprintf("\n>> Mediana lucro O1 : %.2f\n", med_O1))
# cat(sprintf(">> Melhor lucro O1  : %.2f\n", best_O1))
# cat(sprintf(">> Unidades         : %d\n",   total_units(BEST_S_O1)))
#
# # Calcular curva mediana O1
# max_len_O1 <- max(sapply(hists_O1, length))
# mat_O1     <- sapply(hists_O1, function(h) c(h, rep(h[length(h)], max_len_O1 - length(h))))
# med_curve_O1 <- apply(mat_O1, 1, median)
#
# # Gráfico O1: Estilo com banda e cor laranja conforme pedido
# pdf("/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/convergencia_O1_SANN_v2.pdf", width = 10, height = 6.5)
# x_fes_O1 <- seq_len(max_len_O1)
# plot_banded_series(x_fes_O1, mat_O1, "darkorange",
#                    xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro",
#                    main = paste0("Convergencia SANN O1 (Mediana e Banda Min-Max) - T=", BEST_T))
# legend("bottomright",
#        legend = c(sprintf("SANN Mediana (med=%.0f)", med_O1), "Mediana final", "Banda Min-Max (20 runs)"),
#        col    = c("red", "grey40", "grey80"),
#        fill   = c(NA, NA, "grey80"),
#        border = c(NA, NA, "grey80"),
#        lwd    = c(2.5, 1, NA), lty = c(1, 2, NA), bty = "n")
# dev.off()
# cat("Grafico convergencia O1 guardado com bandas e cor laranja.\n")

# Guarda historico para comparativos
saveRDS(NULL, "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/convergencia_O1_SANN.rds")

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

build_mat <- function(hists) {
  valid <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  ml <- max(sapply(valid, length))
  sapply(valid, function(h) c(h, rep(h[length(h)], ml - length(h))))
}
mat_dp  <- build_mat(hists_O2_dp)
mat_rep <- build_mat(hists_O2_rep)
pdf("/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/convergencia_O2_dp_vs_repair.pdf", width = 10, height = 6.5)

y_all <- c(if (!is.null(mat_dp))  mat_dp[is.finite(mat_dp)],
           if (!is.null(mat_rep)) mat_rep[is.finite(mat_rep)])
y_rng <- range(y_all, na.rm = TRUE)

# 1. Death Penalty (Laranja)
if (!is.null(mat_dp)) {
  x_dp <- seq_len(nrow(mat_dp))
  plot_banded_series(x_dp, mat_dp, "darkorange", col_line = "darkorange", add = FALSE,
                     ylim = y_rng,
                     xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro",
                     main = "O2 SANN: Death Penalty vs Repair (Mediana e Bandas)")
}

# 2. Repair (Azul Água)
if (!is.null(mat_rep)) {
  x_rep <- seq_len(nrow(mat_rep))
  plot_banded_series(x_rep, mat_rep, "#00CED1", col_line = "#00CED1", add = TRUE)
}

legend("bottomright",
       legend = c(sprintf("Death Penalty (med=%.0f)", med_O2_dp),
                  sprintf("Repair (med=%.0f)",        med_O2_rep),
                  "Mediana Final (Dashed)"),
       col    = c("darkorange", "#00CED1", "grey40"),
       fill   = c(NA, NA, NA),
       border = c(NA, NA, NA),
       lty    = c(1, 1, 2),
       lwd    = c(2.5, 2.5, 1), bty = "n")
dev.off()
cat("Grafico O2 DP vs Repair guardado com bandas.\n")

# Guarda histórico O2 para comparativos.R
curve_dp  <- if(!is.null(mat_dp)) apply(mat_dp, 1, median, na.rm = TRUE) else NULL
curve_rep <- if(!is.null(mat_rep)) apply(mat_rep, 1, median, na.rm = TRUE) else NULL
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
