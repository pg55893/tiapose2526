# =============================================================
# tabu_O1_O2.R — Nuno (Bonus Tarefa 4)
# Tabu Search para O1, O2 Death Penalty e O2 Repair
# Codificacao binaria: 476 bits (28 posicoes x 17 bits)
# PR: 5 bits | J: 6 bits | X: 6 bits por (loja, dia)
# 20 runs, mediana profit, FES no eixo X
# =============================================================

library(tabuSearch)

BASE_DIR <- "/Users/martinho/Documents/MEGSI/2Semestre/TIAPOSE/TIAPOSE_project"
source(file.path(BASE_DIR, "utils", "config_otimizacao.R"))

TABU_DIR <- file.path(BASE_DIR, "otimizacao", "Tabu")
dir.create(TABU_DIR, showWarnings = FALSE)
LOG_FILE <- file.path(TABU_DIR, "log_tabu.txt")
sink(LOG_FILE, append = FALSE, split = TRUE)
cat("Log iniciado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# =============================================================
# CODIFICACAO BINARIA
# 28 posicoes (4 lojas x 7 dias), cada uma com 17 bits:
#   PR : 5 bits -> [0, 0.30]
#   J  : 6 bits -> [0, upper_J] clamped
#   X  : 6 bits -> [0, upper_X] clamped
# Total: 476 bits
# =============================================================
N_BITS_PR  <- 5L
N_BITS_J   <- 6L
N_BITS_X   <- 6L
N_BITS_POS <- N_BITS_PR + N_BITS_J + N_BITS_X   # 17
N_BITS     <- 28L * N_BITS_POS                   # 476

decode_S <- function(bits) {
  S <- numeric(84)
  for (pos in 1:28) {
    st  <- (pos - 1L) * N_BITS_POS + 1L
    idx <- (pos - 1L) * 3L + 1L
    pr_int    <- sum(bits[st:(st + N_BITS_PR - 1L)] * 2^((N_BITS_PR-1L):0L))
    S[idx]    <- pr_int / (2^N_BITS_PR - 1) * 0.30
    j_int     <- sum(bits[(st+N_BITS_PR):(st+N_BITS_PR+N_BITS_J-1L)] * 2^((N_BITS_J-1L):0L))
    S[idx+1L] <- min(j_int, upper[idx+1L])
    x_int     <- sum(bits[(st+N_BITS_PR+N_BITS_J):(st+N_BITS_POS-1L)] * 2^((N_BITS_X-1L):0L))
    S[idx+2L] <- min(x_int, upper[idx+2L])
  }
  return(S)
}

idx_PR_local <- seq(1, 84, by = 3)
idx_J_all    <- seq(2, 84, by = 3)
idx_X_all    <- seq(3, 84, by = 3)

# =============================================================
# REPAIR — reduz PR x0.95 iterativamente até total_units <= 10000
# Aplicado ao vetor S contínuo após decode_S(bits)
# Mesmo padrão que SANN v2 e HC (Eduardo)
# =============================================================
repair <- function(S) {
  S <- pmin(pmax(S, lower), upper)
  BEST_repair <- NULL
  best_p      <- -Inf
  for (iter in 1:200) {
    u <- total_units(S)
    if (!is.na(u) && u <= 10000) {
      p <- profit(S)
      if (p > best_p) { best_p <- p; BEST_repair <- S }
      break
    }
    S[idx_PR_local] <- pmax(S[idx_PR_local] * 0.95, 0)
  }
  return(BEST_repair)
}

# =============================================================
# VALIDACAO DA CODIFICACAO
# =============================================================
cat("=== VALIDACAO CODIFICACAO ===\n")
cat("N_BITS =", N_BITS, "\n")
set.seed(0)
bits_test <- sample(0:1, N_BITS, replace = TRUE)
S_test    <- decode_S(bits_test)
cat("length(S)      =", length(S_test), "\n")
cat("PR em [0,0.30]:", all(S_test[idx_PR_local] >= 0 & S_test[idx_PR_local] <= 0.30), "\n")
cat("J  <= upper_J :", all(S_test[idx_J_all] <= upper[idx_J_all]), "\n")
cat("X  <= upper_X :", all(S_test[idx_X_all] <= upper[idx_X_all]), "\n\n")

# =============================================================
# PARAMETROS
# =============================================================
NRUNS    <- 20
TS_ITERS <- 1000
TS_NEIGH <- 10     # FES/run ~ 10000
TS_LIST  <- 15

cat(sprintf("NRUNS=%d | TS_ITERS=%d | TS_NEIGH=%d | FES/run~%d\n\n",
            NRUNS, TS_ITERS, TS_NEIGH, TS_ITERS * TS_NEIGH))

# =============================================================
# HELPERS — gráficos
# =============================================================
build_mat_tabu <- function(hists) {
  valid   <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  max_len <- max(sapply(valid, length))
  sapply(valid, function(h) c(h, rep(h[length(h)], max_len - length(h))))
}

plot_tabu <- function(hists, titulo, pdf_out, cor_med = "steelblue") {
  mat <- build_mat_tabu(hists)
  if (is.null(mat)) return(NULL)
  fv  <- mat[is.finite(mat)]
  if (length(fv) == 0) { cat("Sem dados para grafico.\n"); return(NULL) }
  med_curve <- apply(mat, 1, median, na.rm = TRUE)
  x_fes     <- seq_len(nrow(mat))
  pdf(pdf_out, width = 9, height = 6)
  for (j in seq_len(ncol(mat))) {
    if (j == 1)
      plot(x_fes, mat[, j], type = "l", col = "grey80", lwd = 1,
           xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro ($)",
           main = titulo, ylim = range(fv))
    else lines(x_fes, mat[, j], col = "grey80", lwd = 1)
  }
  lines(x_fes, med_curve, col = cor_med, lwd = 2.5)
  legend("bottomright", legend = c("Runs individuais", "Mediana"),
         col = c("grey70", cor_med), lwd = c(1, 2.5), bty = "n")
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
  invisible(med_curve)
}

# =============================================================
# FUNÇÃO AUXILIAR — corre NRUNS do tabuSearch
# obj_lucro_fn recebe bits e retorna lucro (ou -1e9 se inválido)
# tabuSearch MAXIMIZA objFunc
# =============================================================
run_tabu <- function(obj_lucro_fn, label, seed_base, cor_med = "steelblue",
                     pdf_name, rds_name) {
  cat(sprintf("\n=== Tabu Search %s (%d runs) ===\n", label, NRUNS))
  lucros <- numeric(NRUNS)
  hists  <- list()
  BEST_S <- NULL
  best   <- -Inf

  for (i in 1:NRUNS) {
    set.seed(seed_base + i)
    best_val_run  <- -Inf
    best_bits_run <- sample(0:1, N_BITS, replace = TRUE)
    FES_run       <- 0L
    hist_run      <- numeric(0)

    obj_fn_i <- function(bits) {
      p            <- obj_lucro_fn(bits)
      FES_run      <<- FES_run + 1L
      p_val        <- if (is.finite(p) && p > -1e8) p else -Inf
      if (p_val > best_val_run) { best_val_run <<- p_val; best_bits_run <<- bits }
      hist_run     <<- c(hist_run, best_val_run)
      return(p)
    }

    bits0 <- sample(0:1, N_BITS, replace = TRUE)
    tryCatch(
      tabuSearch(size = N_BITS, iters = TS_ITERS, objFunc = obj_fn_i,
                 config = bits0, neigh = TS_NEIGH, listSize = TS_LIST,
                 nRestarts = 0, verbose = FALSE),
      error = function(e) cat("ERRO run", i, ":", conditionMessage(e), "\n")
    )

    S_best <- decode_S(best_bits_run)
    L      <- if (best_val_run > -Inf) best_val_run else NA
    lucros[i]  <- L
    hists[[i]] <- hist_run
    cat(sprintf("Run %2d | Lucro: %8.2f | FES: %d\n",
                i, ifelse(is.na(L), -Inf, L), FES_run))
    if (!is.na(L) && L > best) { best <- L; BEST_S <- S_best }
  }

  med <- median(lucros, na.rm = TRUE)
  cat(sprintf("\n>> Mediana %s : %.2f\n", label, med))
  cat(sprintf(">> Melhor  %s : %.2f\n", label, best))
  if (!is.null(BEST_S)) cat(sprintf(">> Unidades   : %d\n", total_units(BEST_S)))

  med_curve <- plot_tabu(hists,
    titulo  = paste0("Tabu Search ", label, " — ", NRUNS, " runs"),
    pdf_out = file.path(TABU_DIR, pdf_name),
    cor_med = cor_med)
  saveRDS(med_curve, file.path(TABU_DIR, rds_name))

  list(lucros = lucros, mediana = med, melhor = best,
       BEST_S = BEST_S, hists = hists, med_curve = med_curve)
}

# =============================================================
# O1 — Tabu Search, sem restrições
# =============================================================
res_O1 <- run_tabu(
  obj_lucro_fn = function(bits) { profit(decode_S(bits)) },
  label    = "O1",
  seed_base = 42,
  cor_med  = "steelblue",
  pdf_name = "convergencia_O1_tabu.pdf",
  rds_name = "convergencia_O1_tabu.rds"
)

# =============================================================
# O2 — Death Penalty
# Soluções com total_units > 10000 recebem lucro -1e9
# =============================================================
res_O2_dp <- run_tabu(
  obj_lucro_fn = function(bits) {
    S <- decode_S(bits)
    u <- total_units(S)
    if (is.na(u) || u > 10000) return(-1e9)
    profit(S)
  },
  label    = "O2 (Death Penalty)",
  seed_base = 123,
  cor_med  = "tomato",
  pdf_name = "convergencia_O2_dp_tabu.pdf",
  rds_name = "convergencia_O2_dp_tabu.rds"
)

# =============================================================
# O2 — Repair
# Após decode, aplica repair ao vetor S antes de avaliar
# Soluções irreparáveis (NULL) recebem lucro -1e9
# =============================================================
res_O2_rep <- run_tabu(
  obj_lucro_fn = function(bits) {
    S   <- decode_S(bits)
    S_r <- repair(S)
    if (is.null(S_r)) return(-1e9)
    profit(S_r)
  },
  label    = "O2 (Repair)",
  seed_base = 456,
  cor_med  = "darkorange",
  pdf_name = "convergencia_O2_rep_tabu.pdf",
  rds_name = "convergencia_O2_rep_tabu.rds"
)

# RDS principal O2 = Repair (para comparativos.R)
saveRDS(res_O2_rep$med_curve, file.path(TABU_DIR, "convergencia_O2_tabu.rds"))

# =============================================================
# COMPARATIVO O2: Death Penalty vs Repair
# =============================================================
cat("\n=== Convergencia O2: Death Penalty vs Repair ===\n")

build_med_curve <- function(hists) {
  valid <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  ml  <- max(sapply(valid, length))
  mat <- sapply(valid, function(h) c(h, rep(h[length(h)], ml - length(h))))
  apply(mat, 1, median, na.rm = TRUE)
}

curve_dp  <- build_med_curve(res_O2_dp$hists)
curve_rep <- build_med_curve(res_O2_rep$hists)

pdf(file.path(TABU_DIR, "convergencia_O2_dp_vs_repair_tabu.pdf"), width = 10, height = 6)
all_vals <- c(curve_dp[is.finite(curve_dp)], curve_rep[is.finite(curve_rep)])
if (length(all_vals) > 0) {
  x_max <- max(length(curve_dp), length(curve_rep), 1)
  plot(NA, xlim = c(1, x_max), ylim = range(all_vals),
       xlab = "Numero de Avaliacoes (FES)", ylab = "Mediana Melhor Lucro ($)",
       main = paste0("Tabu Search O2 — Death Penalty vs Repair (", NRUNS, " runs)"))
  if (!is.null(curve_dp))
    lines(seq_along(curve_dp),  curve_dp,  col = "tomato",    lwd = 2.5)
  if (!is.null(curve_rep))
    lines(seq_along(curve_rep), curve_rep, col = "darkorange", lwd = 2.5)
  legend("bottomright",
         legend = c(sprintf("Death Penalty (med=%.0f)", res_O2_dp$mediana),
                    sprintf("Repair        (med=%.0f)", res_O2_rep$mediana)),
         col = c("tomato", "darkorange"), lwd = 2.5, bty = "n")
}
dev.off()
cat("Grafico comparativo O2 guardado.\n")

# =============================================================
# EXPORTAR RESULTADOS
# =============================================================
idx_JX_all <- c(seq(2, 84, by = 3), seq(3, 84, by = 3))

resultado_tabu <- data.frame(
  membro    = "Nuno",
  algoritmo = "Tabu Search",
  objetivo  = c("O1", "O2_DP", "O2_Repair"),
  metodo_O2 = c("—", "Death Penalty", "Repair"),
  lucro     = c(round(res_O1$melhor, 2),     round(res_O2_dp$melhor, 2),  round(res_O2_rep$melhor, 2)),
  mediana   = c(round(res_O1$mediana, 2),    round(res_O2_dp$mediana, 2), round(res_O2_rep$mediana, 2)),
  unidades  = c(
    if (!is.null(res_O1$BEST_S))     total_units(res_O1$BEST_S)     else NA,
    if (!is.null(res_O2_dp$BEST_S))  total_units(res_O2_dp$BEST_S)  else NA,
    if (!is.null(res_O2_rep$BEST_S)) total_units(res_O2_rep$BEST_S) else NA
  ),
  N_bits   = N_BITS,
  N_runs   = NRUNS,
  TS_iters = TS_ITERS,
  TS_neigh = TS_NEIGH,
  stringsAsFactors = FALSE
)
CSV_OUT <- file.path(TABU_DIR, "resultado_tabu.csv")
write.csv(resultado_tabu, CSV_OUT, row.names = FALSE)
cat("\n=== Resultado Final Tabu Search ===\n")
print(resultado_tabu)

# =============================================================
# VALIDAÇÕES FINAIS
# =============================================================
cat("\n=== VALIDACOES FINAIS ===\n")
cat("1. N_BITS =", N_BITS, "(esperado 476):", N_BITS == 476, "\n")
cat("2. Mediana O1:", res_O1$mediana, "vs Melhor:", res_O1$melhor, "\n")
set.seed(99); bits_v <- sample(0:1, N_BITS, replace = TRUE)
cat("3. Codificacao PR em [0,0.30]:", all(decode_S(bits_v)[idx_PR_local] <= 0.30), "\n")
if (!is.null(res_O2_dp$BEST_S)) {
  u <- total_units(res_O2_dp$BEST_S)
  cat(sprintf("4. Melhor O2 DP: %d unidades (<= 10000): %s\n",
              u, ifelse(u <= 10000, "OK", "VIOLADA!")))
}
if (!is.null(res_O2_rep$BEST_S)) {
  u <- total_units(res_O2_rep$BEST_S)
  cat(sprintf("5. Melhor O2 Repair: %d unidades (<= 10000): %s\n",
              u, ifelse(u <= 10000, "OK", "VIOLADA!")))
}

cat("\n=== FIM tabu_O1_O2.R ===\n")
sink()
