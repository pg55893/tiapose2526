# =============================================================
# MC_v2.R — Nuno
# Monte Carlo O1, O2 Death Penalty e O2 Repair
# 20 runs independentes, mediana profit, FES no eixo X
# Padrão compatível com comparativos.R do grupo (mesmo que SANN v2)
# =============================================================

BASE_DIR <- "/Users/martinho/Documents/MEGSI/2Semestre/TIAPOSE/TIAPOSE_project"
source(file.path(BASE_DIR, "utils", "config_otimizacao.R"))

MC_OUT_DIR <- file.path(BASE_DIR, "otimizacao", "MC")
dir.create(MC_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
LOG_FILE   <- file.path(MC_OUT_DIR, "log_MC_v2.txt")
sink(LOG_FILE, append = FALSE, split = TRUE)
cat("Log iniciado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# =============================================================
# PARÂMETROS
# =============================================================
NRUNS  <- 20
N_ITER <- 10000

idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))
idx_PR <- seq(1, 84, by = 3)

cat("NRUNS  =", NRUNS, "\n")
cat("N_ITER =", N_ITER, "(avaliacoes por run)\n")
cat("D      = 84 (dim do vetor S)\n\n")

# =============================================================
# REPAIR — reduz PR x0.95 iterativamente até total_units <= 10000
# Mesmo padrão que SANN v2 e HC (Eduardo)
# =============================================================
repair <- function(S) {
  S <- pmin(pmax(S, lower), upper)
  BEST_repair <- NULL
  best_profit <- -Inf
  for (iter in 1:200) {
    u <- total_units(S)
    if (!is.na(u) && u <= 10000) {
      p <- profit(S)
      if (p > best_profit) { best_profit <- p; BEST_repair <- S }
      break
    }
    S[idx_PR] <- pmax(S[idx_PR] * 0.95, 0)
  }
  return(BEST_repair)
}

# =============================================================
# WRAPPER FES
# =============================================================
make_eval_fes <- function(obj_fn) {
  FES    <- 0L
  MELHOR <- -Inf
  hist   <- numeric(0)
  list(
    fn = function(S) {
      v   <- obj_fn(S)
      FES <<- FES + 1L
      p   <- if (is.finite(v)) -v else -Inf
      if (p > MELHOR) MELHOR <<- p
      hist <<- c(hist, MELHOR)
      return(v)
    },
    get_hist = function() hist,
    get_fes  = function() FES
  )
}

# =============================================================
# HELPERS — curva mediana e gráfico
# =============================================================
build_mat_mc <- function(hists) {
  valid   <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  max_len <- max(sapply(valid, length))
  sapply(valid, function(h) c(h, rep(h[length(h)], max_len - length(h))))
}

plot_runs_mc <- function(hists, titulo, pdf_out, cor_med = "steelblue") {
  mat <- build_mat_mc(hists)
  if (is.null(mat)) { cat("Sem dados para grafico:", titulo, "\n"); return(NULL) }
  fv  <- mat[is.finite(mat)]
  if (length(fv) == 0) { cat("AVISO: sem solucoes validas:", titulo, "\n"); return(NULL) }
  med_curve <- apply(mat, 1, median, na.rm = TRUE)
  x_fes     <- seq_len(nrow(mat))
  pdf(pdf_out, width = 9, height = 6)
  for (j in seq_len(ncol(mat))) {
    if (j == 1)
      plot(x_fes, mat[, j], type = "l", col = "grey80", lwd = 1,
           xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro ($)",
           main = titulo, ylim = range(fv))
    else
      lines(x_fes, mat[, j], col = "grey80", lwd = 1)
  }
  lines(x_fes, med_curve, col = cor_med, lwd = 2.5)
  legend("bottomright", legend = c("Runs individuais", "Mediana"),
         col = c("grey70", cor_med), lwd = c(1, 2.5), bty = "n")
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
  invisible(med_curve)
}

# =============================================================
# VALIDAÇÃO INICIAL
# =============================================================
cat("=== VALIDACAO CONFIG ===\n")
cat("profit(S1)      =", profit(S1), "\n")
cat("eval(S1)        =", eval(S1),   "\n")
cat("eval_O2(S1)     =", eval_O2(S1), "\n")
cat("total_units(S1) =", total_units(S1), "\n")
cat("lower[1:6]      =", lower[1:6], "\n")
cat("upper[1:6]      =", upper[1:6], "\n")
set.seed(0)
S_test <- runif(84, min = lower, max = upper)
cat("Bounds OK:", all(S_test >= lower - 1e-10 & S_test <= upper + 1e-10), "\n\n")

# =============================================================
# O1 — Monte Carlo, sem restrições
# =============================================================
cat(sprintf("=== O1: Monte Carlo (%d runs, %d aval/run) ===\n", NRUNS, N_ITER))

lucros_O1 <- numeric(NRUNS)
BEST_S_O1 <- NULL
best_O1   <- -Inf
hists_O1  <- list()

set.seed(42)
for (i in 1:NRUNS) {
  tracker    <- make_eval_fes(function(S) { S <- pmin(pmax(S, lower), upper); eval(S) })
  best_run   <- -Inf
  best_S_run <- NULL
  for (k in 1:N_ITER) {
    S_rand <- runif(84, min = lower, max = upper)
    val    <- tracker$fn(S_rand)
    p      <- -val
    if (is.finite(p) && p > best_run) { best_run <- p; best_S_run <- S_rand }
  }
  if (!is.null(best_S_run)) { best_S_run[idx_JX] <- round(best_S_run[idx_JX]); L <- profit(best_S_run) } else { L <- -Inf }
  lucros_O1[i]  <- L
  hists_O1[[i]] <- tracker$get_hist()
  cat(sprintf("Run %2d | Lucro: %8.2f | FES: %d\n", i, L, tracker$get_fes()))
  if (L > best_O1) { best_O1 <- L; BEST_S_O1 <- best_S_run }
}

med_O1 <- median(lucros_O1)
cat(sprintf("\n>> Mediana O1 : %.2f\n", med_O1))
cat(sprintf(">> Melhor O1  : %.2f\n", best_O1))
if (!is.null(BEST_S_O1)) cat(sprintf(">> Unidades   : %d\n", total_units(BEST_S_O1)))

med_curve_O1 <- plot_runs_mc(hists_O1,
  titulo  = paste0("Monte Carlo O1 — ", NRUNS, " runs (", N_ITER, " aval/run)"),
  pdf_out = file.path(MC_OUT_DIR, "convergencia_O1_MC_v2.pdf"),
  cor_med = "steelblue")
saveRDS(med_curve_O1, file.path(MC_OUT_DIR, "convergencia_O1_MC.rds"))
cat("RDS O1 guardado.\n\n")

# =============================================================
# O2 — Death Penalty
# Bounds de J e X reduzidos a 1/6 para aumentar probabilidade
# de sampling dentro da zona viável (restrição <= 10000 unidades)
# =============================================================
cat(sprintf("=== O2: Monte Carlo + Death Penalty (%d runs, %d aval/run) ===\n", NRUNS, N_ITER))

upper_O2 <- upper
upper_O2[seq(2, 84, by = 3)] <- ceiling(upper[seq(2, 84, by = 3)] / 6)
upper_O2[seq(3, 84, by = 3)] <- ceiling(upper[seq(3, 84, by = 3)] / 6)
cat(sprintf("Bounds reduzidos: upper_J max=%.0f | upper_X max=%.0f\n",
            max(upper_O2[seq(2, 84, by = 3)]), max(upper_O2[seq(3, 84, by = 3)])))

lucros_O2_dp <- numeric(NRUNS)
BEST_S_O2_dp <- NULL
best_O2_dp   <- -Inf
hists_O2_dp  <- list()

set.seed(123)
for (i in 1:NRUNS) {
  tracker    <- make_eval_fes(function(S) { S <- pmin(pmax(S, lower), upper); eval_O2(S) })
  best_run   <- -Inf
  best_S_run <- NULL
  for (k in 1:N_ITER) {
    S_rand <- runif(84, min = lower, max = upper_O2)
    val    <- tracker$fn(S_rand)
    if (is.finite(val)) {
      p <- -val
      if (p > best_run) { best_run <- p; best_S_run <- S_rand }
    }
  }
  if (!is.null(best_S_run) && best_run > -Inf) {
    best_S_run[idx_JX] <- round(best_S_run[idx_JX])
    u <- total_units(best_S_run)
    L <- if (!is.na(u) && u <= 10000) profit(best_S_run) else NA
  } else { L <- NA }
  lucros_O2_dp[i]  <- L
  hists_O2_dp[[i]] <- tracker$get_hist()
  if (!is.na(L)) {
    cat(sprintf("Run %2d | Lucro: %8.2f | Unidades: %d | FES: %d\n",
                i, L, total_units(best_S_run), tracker$get_fes()))
    if (L > best_O2_dp) { best_O2_dp <- L; BEST_S_O2_dp <- best_S_run }
  } else {
    cat(sprintf("Run %2d | Sem solucao valida | FES: %d\n", i, tracker$get_fes()))
  }
}

med_O2_dp <- median(lucros_O2_dp, na.rm = TRUE)
cat(sprintf("\n>> Mediana O2 (DP) : %.2f\n", med_O2_dp))
cat(sprintf(">> Melhor O2 (DP)  : %.2f\n", best_O2_dp))
if (!is.null(BEST_S_O2_dp)) cat(sprintf(">> Unidades        : %d\n", total_units(BEST_S_O2_dp)))

med_curve_O2_dp <- plot_runs_mc(hists_O2_dp,
  titulo  = paste0("Monte Carlo O2 (Death Penalty) — ", NRUNS, " runs"),
  pdf_out = file.path(MC_OUT_DIR, "convergencia_O2_dp_MC.pdf"),
  cor_med = "tomato")
saveRDS(med_curve_O2_dp, file.path(MC_OUT_DIR, "convergencia_O2_dp_MC.rds"))
cat("RDS O2 DP guardado.\n\n")

# =============================================================
# O2 — Repair
# Bounds completos; cada candidato é reparado (PR x0.95) antes
# de ser avaliado — mesmo padrão SANN v2 e HC (Eduardo)
# =============================================================
cat(sprintf("=== O2: Monte Carlo + Repair (%d runs, %d aval/run) ===\n", NRUNS, N_ITER))

lucros_O2_rep <- numeric(NRUNS)
BEST_S_O2_rep <- NULL
best_O2_rep   <- -Inf
hists_O2_rep  <- list()

set.seed(456)
for (i in 1:NRUNS) {
  tracker    <- make_eval_fes(function(S) { S_r <- repair(S); if (is.null(S_r)) Inf else -profit(S_r) })
  best_run   <- -Inf
  best_S_run <- NULL
  for (k in 1:N_ITER) {
    S_rand <- runif(84, min = lower, max = upper)
    val    <- tracker$fn(S_rand)
    if (is.finite(val)) {
      p <- -val
      if (p > best_run) { best_run <- p; best_S_run <- repair(S_rand) }
    }
  }
  if (!is.null(best_S_run) && best_run > -Inf) {
    best_S_run[idx_JX] <- round(best_S_run[idx_JX])
    u <- total_units(best_S_run)
    L <- if (!is.na(u) && u <= 10000) profit(best_S_run) else NA
  } else { L <- NA }
  lucros_O2_rep[i]  <- L
  hists_O2_rep[[i]] <- tracker$get_hist()
  if (!is.na(L)) {
    cat(sprintf("Run %2d | Lucro: %8.2f | Unidades: %d | FES: %d\n",
                i, L, total_units(best_S_run), tracker$get_fes()))
    if (L > best_O2_rep) { best_O2_rep <- L; BEST_S_O2_rep <- best_S_run }
  } else {
    cat(sprintf("Run %2d | Repair falhou | FES: %d\n", i, tracker$get_fes()))
  }
}

med_O2_rep <- median(lucros_O2_rep, na.rm = TRUE)
cat(sprintf("\n>> Mediana O2 (Repair) : %.2f\n", med_O2_rep))
cat(sprintf(">> Melhor O2 (Repair)  : %.2f\n", best_O2_rep))
if (!is.null(BEST_S_O2_rep)) cat(sprintf(">> Unidades            : %d\n", total_units(BEST_S_O2_rep)))

med_curve_O2_rep <- plot_runs_mc(hists_O2_rep,
  titulo  = paste0("Monte Carlo O2 (Repair) — ", NRUNS, " runs"),
  pdf_out = file.path(MC_OUT_DIR, "convergencia_O2_rep_MC.pdf"),
  cor_med = "darkorange")
saveRDS(med_curve_O2_rep, file.path(MC_OUT_DIR, "convergencia_O2_rep_MC.rds"))
# RDS principal O2 = Repair (para comparativos.R)
saveRDS(med_curve_O2_rep, file.path(MC_OUT_DIR, "convergencia_O2_MC.rds"))
cat("RDS O2 Repair guardado.\n\n")

# =============================================================
# COMPARATIVO O2: Death Penalty vs Repair
# =============================================================
cat("=== Convergencia O2: Death Penalty vs Repair ===\n")

build_med_curve <- function(hists) {
  valid <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  ml  <- max(sapply(valid, length))
  mat <- sapply(valid, function(h) c(h, rep(h[length(h)], ml - length(h))))
  apply(mat, 1, median, na.rm = TRUE)
}

curve_dp  <- build_med_curve(hists_O2_dp)
curve_rep <- build_med_curve(hists_O2_rep)

pdf(file.path(MC_OUT_DIR, "convergencia_O2_dp_vs_repair_MC.pdf"), width = 10, height = 6)
all_vals <- c(curve_dp[is.finite(curve_dp)], curve_rep[is.finite(curve_rep)])
if (length(all_vals) > 0) {
  ylim_cmp <- range(all_vals)
  x_dp  <- seq_along(curve_dp)
  x_rep <- seq_along(curve_rep)
  x_max <- max(length(curve_dp), length(curve_rep), 1)
  plot(NA, xlim = c(1, x_max), ylim = ylim_cmp,
       xlab = "Numero de Avaliacoes (FES)", ylab = "Mediana Melhor Lucro ($)",
       main = paste0("Monte Carlo O2 — Death Penalty vs Repair (", NRUNS, " runs)"))
  if (!is.null(curve_dp))  lines(x_dp,  curve_dp,  col = "tomato",     lwd = 2.5)
  if (!is.null(curve_rep)) lines(x_rep, curve_rep, col = "darkorange",  lwd = 2.5)
  legend("bottomright",
         legend = c(sprintf("Death Penalty (med=%.0f)", med_O2_dp),
                    sprintf("Repair        (med=%.0f)", med_O2_rep)),
         col = c("tomato", "darkorange"), lwd = 2.5, bty = "n")
}
dev.off()
cat("Grafico comparativo O2 guardado.\n\n")

# =============================================================
# EXPORTAR RESULTADOS
# =============================================================
resultado_MC <- data.frame(
  membro    = "Nuno",
  algoritmo = "Monte Carlo",
  objetivo  = c("O1", "O2_DP", "O2_Repair"),
  metodo_O2 = c("—", "Death Penalty", "Repair"),
  lucro     = c(round(best_O1, 2),     round(best_O2_dp, 2),  round(best_O2_rep, 2)),
  mediana   = c(round(med_O1,  2),     round(med_O2_dp,  2),  round(med_O2_rep,  2)),
  unidades  = c(
    if (!is.null(BEST_S_O1))     total_units(BEST_S_O1)     else NA,
    if (!is.null(BEST_S_O2_dp))  total_units(BEST_S_O2_dp)  else NA,
    if (!is.null(BEST_S_O2_rep)) total_units(BEST_S_O2_rep) else NA
  ),
  N_runs = NRUNS,
  N_iter = N_ITER,
  stringsAsFactors = FALSE
)
rownames(resultado_MC) <- NULL

CSV_OUT <- file.path(MC_OUT_DIR, "resultado_MC.csv")
write.csv(resultado_MC, CSV_OUT, row.names = FALSE)
cat("=== Resultado Final MC ===\n")
print(resultado_MC)
cat("\nCSV guardado em:", CSV_OUT, "\n")

# =============================================================
# VALIDAÇÕES FINAIS
# =============================================================
cat("\n=== VALIDACOES FINAIS ===\n")
cat(sprintf("1. FES/run: esperado=%d | O1=%d | O2_DP=%d | O2_Rep=%d\n",
            N_ITER, length(hists_O1[[1]]), length(hists_O2_dp[[1]]),
            if (!is.null(hists_O2_rep[[1]])) length(hists_O2_rep[[1]]) else 0))
cat(sprintf("2. Mediana O1=%.2f | Max=%.2f | Mean=%.2f\n",
            median(lucros_O1), max(lucros_O1), mean(lucros_O1)))
set.seed(999)
bounds_ok <- all(replicate(100, {
  S <- runif(84, min = lower, max = upper)
  all(S >= lower - 1e-10 & S <= upper + 1e-10)
}))
cat("3. Bounds respeitados:", bounds_ok, "\n")
if (!is.null(BEST_S_O2_dp)) {
  u <- total_units(BEST_S_O2_dp)
  cat(sprintf("4. Melhor O2 DP: %d unidades (<= 10000): %s\n",
              u, ifelse(u <= 10000, "OK", "VIOLADA!")))
}
if (!is.null(BEST_S_O2_rep)) {
  u <- total_units(BEST_S_O2_rep)
  cat(sprintf("5. Melhor O2 Repair: %d unidades (<= 10000): %s\n",
              u, ifelse(u <= 10000, "OK", "VIOLADA!")))
}

cat("\n=== FIM MC_v2.R ===\n")
cat("Log guardado em:", LOG_FILE, "\n")
sink()
