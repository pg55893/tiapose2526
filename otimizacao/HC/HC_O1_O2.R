# =============================================================
# HC_O1_O2.R — Eduardo
# Hill Climbing com vizinhança multiplicativa — O1 e O2
# 20 runs, mediana profit, FES no eixo X
# Logs: HC/log_HC.txt | Graficos: runs individuais + mediana
# =============================================================

source("/Users/edias/TIAPOSE2526/utils/config_otimizacao.R")

# --- LOG ---
LOG_FILE <- "/Users/edias/TIAPOSE2526/otimizacao/HC/log_HC.txt"
sink(LOG_FILE, append = FALSE, split = TRUE)
cat("Log iniciado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))
idx_PR <- seq(1, 84, by = 3)
NRUNS  <- 20

# =============================================================
# VIZINHANÇA MULTIPLICATIVA
# =============================================================
vizinhanca_mult <- function(S, lower, upper, sigma = 0.15) {
  S_new         <- S * runif(length(S), 1 - sigma, 1 + sigma)
  S_new[idx_JX] <- round(S_new[idx_JX])
  pmin(pmax(S_new, lower), upper)
}

# =============================================================
# HILL CLIMBING GENÉRICO (retorna par, value, hist)
# =============================================================
hill_climbing <- function(fn, S_init, max_iter = 20000,
                          lower, upper, sigma = 0.15) {
  S_curr         <- pmin(pmax(S_init, lower), upper)
  S_curr[idx_JX] <- round(S_curr[idx_JX])
  f_curr <- fn(S_curr)
  hist   <- numeric(max_iter)

  for (iter in seq_len(max_iter)) {
    S_new <- vizinhanca_mult(S_curr, lower, upper, sigma)
    f_new <- fn(S_new)
    if (is.finite(f_new) && f_new < f_curr) {
      S_curr <- S_new
      f_curr <- f_new
    }
    hist[iter] <- if (is.finite(f_curr)) -f_curr else -Inf
  }
  list(par = S_curr, value = f_curr, hist = hist)
}

# =============================================================
# REPAIR RÁPIDO — 30 reduções x0.90 (evita hang)
# =============================================================
repair_fast <- function(S) {
  S <- pmin(pmax(S, lower), upper)
  for (k in 1:30) {
    u <- total_units(S)
    if (!is.na(u) && u <= 10000) return(S)
    S[idx_PR] <- pmax(S[idx_PR] * 0.90, 0)
  }
  u <- total_units(S)
  if (!is.na(u) && u <= 10000) return(S)
  return(NULL)
}

# =============================================================
# HELPERS — curva mediana e gráfico com runs individuais
# =============================================================
build_mat <- function(hists) {
  valid   <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  max_len <- max(sapply(valid, length))
  sapply(valid, function(h) c(h, rep(h[length(h)], max_len - length(h))))
}

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

plot_runs <- function(hists, titulo, pdf_out, cor_med = "darkorange") {
  mat <- build_mat(hists)
  if (is.null(mat)) { cat("Sem dados para grafico:", titulo, "\n"); return() }
  med_curve <- apply(mat, 1, median, na.rm = TRUE)
  x_fes     <- seq_len(nrow(mat))

  pdf(pdf_out, width = 10, height = 6.5)
  plot_banded_series(x_fes, mat, cor_med,
                     xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro",
                     main = titulo)
  legend("bottomright",
         legend = c("Mediana (Red)", "Mediana final", "Banda Min-Max"),
         col    = c("red", "grey40", "grey80"), fill = c(NA, NA, "grey80"),
         border = c(NA, NA, "grey80"), lwd = c(2.5, 1, NA), lty = c(1, 2, NA),
         bty = "n", cex = 0.85)
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
  invisible(med_curve)
}

plot_dp_rep <- function(hists_dp, hists_rep, med_dp, med_rep, pdf_out) {
  mat_dp  <- build_mat(hists_dp)
  mat_rep <- build_mat(hists_rep)
  y_all   <- c(if (!is.null(mat_dp))  mat_dp[is.finite(mat_dp)],
               if (!is.null(mat_rep)) mat_rep[is.finite(mat_rep)])
  if (length(y_all) == 0) { cat("Sem dados O2 para grafico.\n"); return() }
  y_rng <- range(y_all, na.rm = TRUE)

  pdf(pdf_out, width = 10, height = 6.5)

  # 1. Death Penalty (Laranja)
  if (!is.null(mat_dp)) {
    x_dp <- seq_len(nrow(mat_dp))
    plot_banded_series(x_dp, mat_dp, "darkorange", col_line = "darkorange", add = FALSE,
                       ylim = y_rng,
                       xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro",
                       main = "HC O2: Death Penalty vs Repair (Mediana e Bandas)")
  }

  # 2. Repair (Azul Água)
  if (!is.null(mat_rep)) {
    x_rep <- seq_len(nrow(mat_rep))
    plot_banded_series(x_rep, mat_rep, "#00CED1", col_line = "#00CED1", add = TRUE)
  }

  legend("bottomright",
         legend = c(sprintf("Death Penalty (med=%.0f)", med_dp),
                    sprintf("Repair (med=%.0f)",        med_rep),
                    "Mediana Final (Dashed)"),
         col  = c("darkorange", "#00CED1", "grey40"),
         fill = c(NA, NA, NA), border = c(NA, NA, NA),
         lwd  = c(2.5, 2.5, 1), lty = c(1, 1, 2), bty = "n")
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
}

# =============================================================
# O1 — Hill Climbing, 20 runs (COMENTADO PARA EXECUTAR APENAS O2)
# =============================================================
# cat("=== O1: Hill Climbing (vizinhanca multiplicativa, 20 runs) ===\n")
# 
# eval_O1_hc <- function(S) -profit(pmin(pmax(S, lower), upper))
# 
# lucros_O1 <- numeric(NRUNS)
# BEST_S_O1 <- NULL
# best_O1   <- -Inf
# hists_O1  <- list()
# 
# set.seed(42)
# for (i in 1:NRUNS) {
#   s0  <- runif(84) * (upper - lower) + lower
#   res <- hill_climbing(eval_O1_hc, s0, max_iter = 20000, lower, upper)
#   S_tmp         <- pmin(pmax(res$par, lower), upper)
#   S_tmp[idx_JX] <- round(S_tmp[idx_JX])
#   L             <- profit(S_tmp)
#   lucros_O1[i]  <- L
#   hists_O1[[i]] <- res$hist
#   cat(sprintf("Run %2d | Lucro: %8.2f\n", i, L))
#   if (L > best_O1) { best_O1 <- L; BEST_S_O1 <- S_tmp }
# }
# 
# med_O1 <- median(lucros_O1)
# cat(sprintf("\n>> Mediana lucro O1 : %.2f\n", med_O1))
# cat(sprintf(">> Melhor lucro O1  : %.2f\n", best_O1))
# cat(sprintf(">> Unidades         : %d\n",   total_units(BEST_S_O1)))
# 
# med_curve_O1 <- plot_runs(
#   hists_O1,
#   titulo  = paste0("HC O1 - vizinhanca multiplicativa - ", NRUNS, " runs"),
#   pdf_out = "/Users/edias/TIAPOSE2526/otimizacao/HC/HC_convergencia_O1.pdf"
# )
# saveRDS(med_curve_O1, "/Users/edias/TIAPOSE2526/otimizacao/HC/convergencia_O1_HC.rds")

# =============================================================
# O2 — Death Penalty, 20 runs
# =============================================================
cat("\n=== O2: Hill Climbing + Death Penalty (20 runs) ===\n")

eval_O2_dp <- function(S) {
  S <- pmin(pmax(S, lower), upper)
  if (any(S[idx_PR] >= 1, na.rm = TRUE)) return(Inf)
  u <- total_units(S)
  if (is.na(u) || u > 10000) return(Inf)
  -profit(S)
}

lucros_O2_dp <- numeric(NRUNS)
BEST_S_O2_dp <- NULL
best_O2_dp   <- -Inf
hists_O2_dp  <- list()

set.seed(123)
for (i in 1:NRUNS) {
  s0         <- lower
  s0[idx_PR] <- 0.05
  s0[seq(2, 84, by = 3)] <- 1
  s0[seq(3, 84, by = 3)] <- 1
  s0 <- pmin(pmax(s0 + runif(84) * 0.2, lower), upper)

  res           <- hill_climbing(eval_O2_dp, s0, max_iter = 20000, lower, upper, sigma = 0.10)
  S_tmp         <- pmin(pmax(res$par, lower), upper)
  S_tmp[idx_JX] <- round(S_tmp[idx_JX])
  u             <- total_units(S_tmp)

  if (!is.na(u) && u <= 10000) {
    L               <- profit(S_tmp)
    lucros_O2_dp[i] <- L
    hists_O2_dp[[i]] <- res$hist
    cat(sprintf("Run %2d | Lucro: %8.2f | Unidades: %d\n", i, L, u))
    if (L > best_O2_dp) { best_O2_dp <- L; BEST_S_O2_dp <- S_tmp }
  } else {
    lucros_O2_dp[i]  <- NA
    hists_O2_dp[[i]] <- res$hist
    cat(sprintf("Run %2d | INVALIDA\n", i))
  }
}

med_O2_dp <- median(lucros_O2_dp, na.rm = TRUE)
cat(sprintf("\n>> Mediana O2 (DP) : %.2f\n", med_O2_dp))
cat(sprintf(">> Melhor O2  (DP) : %.2f\n", best_O2_dp))

# =============================================================
# O2 — Repair, 20 runs  (repair_fast: 30 iter x0.90)
# =============================================================
cat("\n=== O2: Hill Climbing + Repair (20 runs) ===\n")

lucros_O2_rep <- numeric(NRUNS)
BEST_S_O2_rep <- NULL
best_O2_rep   <- -Inf
hists_O2_rep  <- list()

set.seed(456)
for (i in 1:NRUNS) {
  # eval com repair embutido — usa local() para scoping correcto
  ev <- local({
    bv <- -Inf; bs <- NULL; ht <- numeric(0)
    list(
      fn = function(S) {
        S   <- pmin(pmax(S, lower), upper)
        S_r <- repair_fast(S)
        if (is.null(S_r)) { ht <<- c(ht, bv); return(Inf) }
        p <- profit(S_r)
        if (p > bv) { bv <<- p; bs <<- S_r }
        ht <<- c(ht, bv)
        -p
      },
      get_lucro = function() bv,
      get_best  = function() bs,
      get_hist  = function() ht
    )
  })

  s0         <- lower
  s0[idx_PR] <- 0.05
  s0[seq(2, 84, by = 3)] <- 1
  s0[seq(3, 84, by = 3)] <- 1
  s0 <- pmin(pmax(s0 + runif(84) * 0.2, lower), upper)

  # HC com a função de repair embutida
  S_curr         <- pmin(pmax(s0, lower), upper)
  S_curr[idx_JX] <- round(S_curr[idx_JX])
  f_curr         <- ev$fn(S_curr)

  for (iter in 1:20000) {
    S_new <- vizinhanca_mult(S_curr, lower, upper, sigma = 0.10)
    f_new <- ev$fn(S_new)
    if (is.finite(f_new) && f_new < f_curr) {
      S_curr <- S_new
      f_curr <- f_new
    }
  }

  L_run <- ev$get_lucro()
  S_run <- ev$get_best()
  h_run <- ev$get_hist()

  if (!is.null(S_run) && L_run > -Inf) {
    lucros_O2_rep[i] <- L_run
    hists_O2_rep[[i]] <- h_run
    cat(sprintf("Run %2d | Lucro: %8.2f | Unidades: %d\n",
                i, L_run, total_units(S_run)))
    if (L_run > best_O2_rep) { best_O2_rep <- L_run; BEST_S_O2_rep <- S_run }
  } else {
    lucros_O2_rep[i]  <- NA
    hists_O2_rep[[i]] <- h_run
    cat(sprintf("Run %2d | Repair falhou\n", i))
  }
}

med_O2_rep <- median(lucros_O2_rep, na.rm = TRUE)
cat(sprintf("\n>> Mediana O2 (Repair) : %.2f\n", med_O2_rep))
cat(sprintf(">> Melhor O2  (Repair) : %.2f\n", best_O2_rep))
if (!is.null(BEST_S_O2_rep))
  cat(sprintf(">> Unidades            : %d\n", total_units(BEST_S_O2_rep)))

# =============================================================
# GRAFICOS O2: runs individuais + mediana DP vs Repair
# =============================================================
plot_dp_rep(
  hists_O2_dp, hists_O2_rep, med_O2_dp, med_O2_rep,
  pdf_out = "/Users/edias/TIAPOSE2526/otimizacao/HC/HC_convergencia_O2_dp_vs_repair.pdf"
)

# =============================================================
# EXPORTAR RESULTADOS
# =============================================================
BEST_S_O2 <- if (!is.null(BEST_S_O2_rep) && best_O2_rep >= best_O2_dp) BEST_S_O2_rep else BEST_S_O2_dp
best_O2   <- max(best_O2_rep, best_O2_dp, na.rm = TRUE)
med_O2    <- max(med_O2_dp,   med_O2_rep, na.rm = TRUE)

resultado_HC <- data.frame(
  membro    = "Eduardo",
  algoritmo = "HC",
  objetivo  = c("O1", "O2"),
  lucro     = c(round(best_O1, 2), round(best_O2, 2)),
  mediana   = c(round(med_O1,  2), round(med_O2,  2)),
  unidades  = c(total_units(BEST_S_O1),
                ifelse(is.null(BEST_S_O2), NA, total_units(BEST_S_O2))),
  total_HR  = c(sum(BEST_S_O1[idx_JX]),
                ifelse(is.null(BEST_S_O2), NA, sum(BEST_S_O2[idx_JX]))),
  stringsAsFactors = FALSE
)
rownames(resultado_HC) <- NULL
write.csv(resultado_HC, "/Users/edias/TIAPOSE2526/otimizacao/HC/resultado_HC.csv", row.names = FALSE)

cat("\n=== Resultado HC ===\n")
print(resultado_HC)

cat("\n=== FIM HC_O1_O2.R ===")
cat("\nLog guardado em:", LOG_FILE, "\n")
sink()
